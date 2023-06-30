package types

import (
	"fmt"
	"path"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"golang.org/x/mod/module"
)

// TODO: forward declarations for local defs?
func (r *Resolver) Resolve(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.BinaryExpr:
		n.Left = r.Resolve(env, n.Left)
		n.Right = r.Resolve(env, n.Right)
		return n
	case parser.Stmt:
		n.Stmt = r.Resolve(env, n.Stmt)
		return n
	case ResolvedPackage:
		// introduce package scope
		packageScope := env.AddScope()
		origPkg := n.OriginalPackage.(parser.Package)
		for i := range origPkg.PackageFiles {
			origPkg.PackageFiles[i] = resolveTopLevel(packageScope, origPkg.PackageFiles[i])
		}
		// Copy defs inside
		for i := range origPkg.ScriptFiles {
			// there should only be one script file in the whole build
			scriptScope := packageScope.AddScope() // should this be env.AddScope()?
			// origPkg.ScriptFiles[i] = r.Infer(scriptScope, origPkg.ScriptFiles[i])
			origPkg.ScriptFiles[i] = r.Resolve(scriptScope, origPkg.ScriptFiles[i])
		}
		n.OriginalPackage = origPkg
		sym := NewDefinition(n, nil, NotForward)
		for k, v := range packageScope.symbols {
			if sym.Child == nil {
				sym.Child = env.AddScope()
			}
			sym.Child.symbols[k] = v
		}
		env.AddSymbol(n.Path, sym)
		// TODO: resolve cross-file identifiers
		return n
	case parser.File:
		n.Body = r.Resolve(env, n.Body)
		return n
	case parser.Ident:
		// what if ident is "_"?
		// it shouldn't happen in the first place. a "_" will never be inserted into env.
		// if n.Name.Data == "_" {
		// 	return n
		// }
		def, ok := env.LookupStack(n.Name.Data)
		if ok {
			return def.Def
		}
		return UnresolvedIdent{OriginalIdent: n, Env: env}
	case parser.TypeArg:
		def, ok := env.LookupStack(n.TypeArg.Data)
		if ok {
			return def.Def
		}
		return UnresolvedTypeArg{OriginalTypeVar: n, Env: env}
	case parser.Illegal:
		n.Node = r.Resolve(env, n.Node)
		return n // At what point do we reject programs with illegal nodes?
	case parser.Block:
		env := env.AddScope()
		for i := range n.Body {
			n.Body[i] = r.Resolve(env, n.Body[i])
		}
		return resolveUnresolved(env, n)
	case parser.EmptyExpr:
		return n
	case parser.FunctionSignature:
		// new plan: if a tvar doesn't exist in the env, it's a new type param.
		n.Param = r.defineResolveTypeAnnotation(env, true, n.Param, n)
		// n.Param = r.resolve(env, n.Param)
		for i := range n.Arrows {
			n.Arrows[i] = r.defineResolveTypeAnnotation(env, true, n.Arrows[i], n)
		}
		n.Clause = r.defineResolveTypeAnnotation(env, false, n.Clause, n)
		// n.Clause = r.resolve(env, n.Clause)
		return n

		// n.Clause = r.resolveWithConstraint(env, n.Clause)
		/*
		 */
		/*
			in order to know if 'a is a new param or not, we need to examine the "with" clause.
			param names get added to scope.
			if a parameter type is a non-forall type variable, call defineTypeParam on it
		*/
		// n.Param = r.resolve(env, n.Param)
		// for i := range n.Arrows {
		// 	n.Arrows[i] = r.resolve(env, n.Arrows[i])
		// }
	// case parser.Arrow:
	// 	n.Type = r.resolve(env, n.Type)
	case parser.LetFunction:
		// what if someone write let _ x = 1?
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			delete(v.Undefined, id)
		}
		n.Name = r.defineDestructure(env, false, n.Name, n, func(string) {})
		bodyScope := env.AddScope()
		define := true
		for i := range n.TypeParams {
			n.TypeParams[i] = r.defineResolveTypeAnnotation(bodyScope, true, n.TypeParams[i], n)
			define = false
		}
		sig := n.Signature.(parser.FunctionSignature)
		sig.Param = r.defineResolveTypeAnnotation(env, define, sig.Param, sig)
		for i := range sig.Arrows {
			sig.Arrows[i] = r.defineResolveTypeAnnotation(env, define, sig.Arrows[i], sig)
		}
		sig.Clause = r.defineResolveTypeAnnotation(env, false, sig.Clause, sig)
		n.Signature = sig
		// n.Signature = r.Resolve(bodyScope, n.Signature)
		// sig := n.Signature.(parser.FunctionSignature)
		// sig.Param = r.defineDestructure(bodyScope, sig.Param, sig, func(id string) {})
		// n.Signature = sig
		n.Body = r.Resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.Function:
		// We don't have to do closure conversion cause the target supports them.
		var id string
		if n.Name != nil {
			id = n.Name.(parser.Ident).Name.Data
			for _, v := range env.symbols {
				delete(v.Undefined, id)
			}
			n.Name = r.defineDestructure(env, false, n.Name, n, func(string) {})
		}
		bodyScope := env.AddScope()
		define := true
		for i := range n.TypeParams {
			n.TypeParams[i] = r.defineResolveTypeAnnotation(bodyScope, true, n.TypeParams[i], n)
			define = false
		}
		sig := n.Signature.(parser.FunctionSignature)
		sig.Param = r.defineResolveTypeAnnotation(env, define, sig.Param, sig)
		for i := range sig.Arrows {
			sig.Arrows[i] = r.defineResolveTypeAnnotation(env, define, sig.Arrows[i], sig)
		}
		sig.Clause = r.defineResolveTypeAnnotation(env, false, sig.Clause, sig)
		n.Signature = sig
		n.Body = r.Resolve(bodyScope, n.Body)
		if n.Name != nil {
			setIllegals(env, id, n.Body)
		}
		return n
	case parser.CommaElement:
		n.X = r.Resolve(env, n.X)
		return n
	case parser.Tuple:
		// In reality, if it's a struct, there should be a new scope created for the struct fields that are introduced.
		tupleScope := env.AddScope()
		for i := range n.Elements {
			// if it's an assignment to an ident, add lhs to scope. it shadows, but its introduction is unknown, since
			// rhs is in parent scope.
			// TODO: this has a bug because it should be inside a CommaElement.
			if elem, ok := n.Elements[i].(parser.CommaElement); ok {
				if binExp, ok := elem.X.(parser.BinaryExpr); ok && binExp.Op.Type == lexer.Assign {
					binExp.Right = r.Resolve(env, binExp.Right)
					if id, ok := binExp.Left.(parser.Ident); ok {
						binExp.Left = r.defineDestructure(tupleScope, false, id, n, func(id string) {
							setIllegals(tupleScope, id, binExp.Right)
						})
					} else {
						// can't have a tuple assignment to a non-ident
						binExp.Left = parser.Illegal{Node: binExp.Left, Msg: "can't have a tuple assignment to a non-ident"}
					}
					elem.X = binExp
					n.Elements[i] = elem
				} else {
					n.Elements[i] = r.Resolve(env, n.Elements[i])
				}
			} else {
				n.Elements[i] = r.Resolve(env, n.Elements[i])
			}
		}
		// if parent is a declaration, it should probably set child map
		return n
	case parser.LetDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = r.Resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = r.defineDestructure(env, false, n.Destructure, n, func(id string) {
			// setIllegalsUsingWalk(env, id, n.Rhs)
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.VarDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = r.Resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = r.defineDestructure(env, false, n.Destructure, n, func(id string) {
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.IfHeader:
		n.Cond = r.Resolve(env, n.Cond)
		return n
	case parser.If:
		n.IfHeader = r.Resolve(env, n.IfHeader)
		n.Body = r.Resolve(env, n.Body)
	case parser.IfElse:
		n.IfHeader = r.Resolve(env, n.IfHeader)
		n.Body = r.Resolve(env, n.Body)
		n.ElseBody = r.Resolve(env, n.ElseBody)
		return n
	case parser.IfMatch:
		n.IfHeader = r.Resolve(env, n.IfHeader)
		for i := range n.Cases {
			/*
				1. Add a scope for each case.
				2. Add undefined from IfHeader.
			*/
			patternCase := n.Cases[i].(parser.PatternCase)
			patternScope := env.AddScope()
			// TODO: defineDestructure for pattern cases needs to consider Cons vs Var.
			// potentially bring in the Cons from the scope.

			// if id, ok := pat.(parser.Ident); ok {
			// 	if d, ok := patternScope.LookupStack(id.Name.Data); ok {
			// 		if _, ok := d.Def.(Cons); ok {
			// 			pat = d.Def
			// 		}
			// 	}
			// }
			// n.Cases[i] =
			pat := patternCase.Pattern
			patternCase.Pattern = r.defineDestructure(patternScope, false, pat, patternCase, func(id string) {
				setIllegals(patternScope, id, n.IfHeader)
			})
			patternCase.Guard = r.Resolve(patternScope, patternCase.Guard)
			patternCase.Expr = r.Resolve(patternScope, patternCase.Expr)
			n.Cases[i] = patternCase
		}
		return n
	case parser.PatternCase:
		// patternScope := env.AddScope()
		// n.Pattern = defineDestructure(patternScope, n.Pattern, n, func(id string) {
		// 	//
		// })
	case parser.TypeDecl:
		id := n.Name.(parser.Ident).Name.Data
		n.Name = defineType(env, n.Name, n)
		bodyScope := env.AddScope()
		for i := range n.TypeParams {
			typeParam := n.TypeParams[i].(parser.TypeArg)
			id := typeParam.TypeArg.Data
			if localDef, ok := bodyScope.LookupLocal(id); ok {
				n.TypeParams[i] = parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
			} else {
				// params are *always* defined for type decls
				d := NewDefinition(ResolvedTypeArg{OriginalTypeVar: typeParam, Env: bodyScope}, n, NotForward)
				bodyScope.AddSymbol(id, d)
				n.TypeParams[i] = d.Def
			}
		}
		n.Clause = r.defineResolveTypeAnnotation(bodyScope, false, n.Clause, n)
		n.Body = r.defineResolveTypeAnnotation(bodyScope, false, n.Body, n)
		// copy bodyScope.symbols to def.children
		// does this make sense for tuples?
		// i think so, for field access. not necessarily for A.b but for a.b
		for symbolName, symbolDef := range bodyScope.symbols {
			if sym, ok := env.symbols[id]; ok {
				if sym.Child == nil {
					sym.Child = env.AddScope()
				}
				sym.Child.symbols[symbolName] = symbolDef
				env.symbols[id] = sym
			}
		}
		return n
		// setIllegals
	case parser.Number:
	case parser.PrefixExpr:
		n.X = r.Resolve(env, n.X)
		return n
	case parser.CallExpr:
		for i := range n.Elements {
			n.Elements[i] = r.Resolve(env, n.Elements[i])
			// check if caller references an ident whose len(def.Undefined) != 0
			// if so, error.
			nd := n.Elements[i]
			if id, ok := findUndefined(nd); ok {
				n.Elements[i] = parser.Illegal{Node: nd, Msg: fmt.Sprintf("%s is used in application before it is declared", id)}
			}
		}
		return n
	case parser.PostfixExpr:
		n.X = r.Resolve(env, n.X)
		return n
	case parser.SelectorExpr:
		n.X = r.Resolve(env, n.X)
		n.Name = Select(n.X, n.Name)
		return n
	case parser.BasicString:
	case parser.InterpolatedString:
	case parser.IndexExpr:
		n.X = r.Resolve(env, n.X)
		for i := range n.IndexElements {
			n.IndexElements[i] = r.Resolve(env, n.IndexElements[i])
		}
		return n
	case parser.ImportDecl:
		n.Package = r.Resolve(env, n.Package)
	case parser.ImportDeclPackage:
		switch bind := n.Binding.(type) {
		case parser.Ident:
			id := bind.Name.Data
			if localDef, ok := env.LookupLocal(id); ok {
				n.Binding = parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
			} else {
				u := PackageName{OriginalIdent: bind, Env: env}
				env.AddSymbol(id, NewDefinition(u, n, NotForward))
				n.Binding = u
			}
		case parser.Tuple:
			for i := range bind.Elements {
				switch elem := bind.Elements[i].(type) {
				case parser.Ident:
					bind.Elements[i] = r.defineDestructure(env, false, elem, n, func(id string) {})
				case parser.IndexExpr:
					bind.Elements[i] = r.defineDestructure(env, false, elem.X, n, func(id string) {})
					for j := range elem.IndexElements {
						if indexElem, ok := elem.IndexElements[j].(parser.Ident); ok {
							elem.IndexElements[j] = r.defineDestructure(env, false, indexElem, n, func(id string) {})
						}
					}
				}
			}
			n.Binding = bind
		default:
			if spath, ok := n.Path.(parser.InterpolatedString); ok {
				if spath, ok := spath.Parts[0].(parser.BasicString); ok {
					importPath := spath.Lit.Data
					if prefix, _, ok := module.SplitPathVersion(importPath); ok {
						id := path.Base(prefix)
						if localDef, ok := env.LookupLocal(id); ok {
							n.Binding = parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
						} else {
							u := PackageName{OriginalIdent: bind, Env: env}
							env.AddSymbol(id, NewDefinition(u, n, NotForward))
							n.Binding = u
						}
					}
				}
			}
		}
	case parser.ImplDecl:
		n.Name = r.Resolve(env, n.Name)
		implScope := env.AddScope()
		n.Args = r.defineResolveTypeAnnotation(implScope, true, n.Args, n)
		n.Clause = r.defineResolveTypeAnnotation(implScope, false, n.Clause, n)
		n.Body = r.Resolve(implScope, n.Body)
		return n
	case parser.ArrayType:
	case parser.NillableType:
		n.Type = r.Resolve(env, n.Type)
		return n
	case Var: // is it possible to hit this case?
	case TypeName:
	case UnresolvedIdent:
	case Cons:
	case PackageName:
	}
	return n
}
