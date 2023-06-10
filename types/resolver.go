package types

import (
	"fmt"
	"path"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"golang.org/x/mod/module"
)

type ForwardStatus int

const (
	NotForward ForwardStatus = iota
	Forward
	ForwardResolved
)

// TODO: add Type field so we can do type checking.
type Definition struct {
	Def           parser.Node
	Ctx           parser.Node         // additional context for error messages
	ForwardStatus ForwardStatus       // is this a forward declaration?
	Undefined     map[string]struct{} // if function or type that references undefined symbols, we can track them here.
	Child         *Env
	Type          Type
}

func NewDefinition(def, ctx parser.Node, forwardStatus ForwardStatus) Definition {
	return Definition{
		Def:           def,
		Ctx:           ctx,
		ForwardStatus: forwardStatus,
		Undefined:     make(map[string]struct{}),
	}
}

type Env struct {
	parent  *Env
	symbols map[string]Definition // we're gonna share the same symbol table for all symbols.
}

func (e *Env) AddScope() *Env {
	return &Env{
		parent:  e,
		symbols: make(map[string]Definition),
	}
}

func (e *Env) AddSymbol(name string, d Definition) *Env {
	if name == "_" {
		return e
	}
	e.symbols[name] = d // probably need a dedup check
	return e
}

func (e *Env) LookupLocal(name string) (Definition, bool) {
	d, ok := e.symbols[name]
	return d, ok
}

func (e *Env) LookupStack(name string) (d Definition, ok bool) {
	p := e
	for p != nil {
		if d, ok = p.LookupLocal(name); ok {
			return d, ok
		}
		p = p.parent
	}
	return d, false
}

func (d *Env) SetType(name string, t Type) {
	if name == "_" {
		return
	}
	sym, ok := d.symbols[name]
	if !ok {
		panic(fmt.Sprintf("symbol %q not found", name))
	}
	sym.Type = t
	d.symbols[name] = sym
}

// TODO: add builtins to this
var Universe = &Env{
	parent:  nil,
	symbols: make(map[string]Definition),
}

// Replace ident with var that points to table?
// That means we need a var node.
// We'd like it to be a pointer to a Scope.
// func Resolve(n parser.Node) parser.Node {
// 	return (&Resolver{}).Resolve(Universe, n)
// }

type Resolver struct {
	importer *parser.Importer
}

func NewResolver(importer *parser.Importer) *Resolver {
	return &Resolver{importer: importer}
}

func (r *Resolver) ResolveBuild() {
	for _, path := range r.importer.Sorted {
		path := path
		pkg := r.importer.PkgCache[path].(parser.Package)
		r.importer.PkgCache[path] = r.Resolve(Universe, ResolvedPackage{OriginalPackage: pkg, Path: path, Env: Universe})
		r.importer.PkgCache[path] = r.Infer(r.importer.PkgCache[path])
	}
}

// should this be a method on Env?
func Select(x parser.Node, sel parser.Node) parser.Node {
	// switch sel.(type) {
	// case Var, Cons:
	// 	return sel
	// }
	switch x := x.(type) {
	case TypeName:
		id := x.OriginalIdent.(parser.Ident).Name.Data
		var selID string
		switch sel := sel.(type) {
		case parser.Ident:
			selID = sel.Name.Data
		case parser.TypeArg:
			selID = sel.TypeArg.Data
		default:
			panic(fmt.Sprintf("invalid selector %T", sel))
		}
		ch := x.Env.symbols[id].Child.symbols[selID].Def
		// ch := x.Env.symbols[id].Children[selID]
		if ch == nil {
			return parser.Illegal{Node: sel, Msg: fmt.Sprintf("%s is not a member of %s", selID, id)}
		}
		return ch
	case PackageName:
		panic("todo")
		// Go to importer, get package, look up children.
	case parser.SelectorExpr:
		return Select(Select(x.X, x.Name), sel)
	}
	// TODO: we are not processing tuple access right now. That should be done in type checking.
	return parser.Illegal{Node: sel, Msg: fmt.Sprintf("cannot select from %T", x)}
}

func defineType(env *Env, nd, ctx parser.Node) parser.Node {
	n, ok := nd.(parser.Ident)
	if !ok {
		panic("unreachable")
	}
	id := n.Name.Data
	if id == "_" {
		return n
	}
	// isForward := false
	forwardStatus := NotForward
	if typeDecl, ok := ctx.(parser.TypeDecl); ok {
		if typeDecl.Body == nil {
			forwardStatus = Forward
		}
		// isForward = typeDecl.Body == nil
	}
	if localDef, ok := env.LookupLocal(id); ok {
		// we don't allow shadowing in local scope
		if localDef.ForwardStatus == Forward && forwardStatus == NotForward {
			localDef.ForwardStatus = ForwardResolved
			env.symbols[id] = localDef
			return localDef.Def
		}
		// if localDef.IsForward && !isForward {
		// 	return localDef.Def
		// }
		return parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
	}
	t := TypeName{OriginalIdent: n, Env: env}
	env.AddSymbol(id, NewDefinition(t, ctx, forwardStatus)) // Does this correspond to our logic?
	return t
}

func defineTag(env *Env, nd, ctx parser.Node) parser.Node {
	n, ok := nd.(parser.Ident)
	if !ok {
		panic("unreachable")
	}
	id := n.Name.Data
	if id == "_" {
		return n
	}
	if localDef, ok := env.LookupLocal(id); ok {
		return parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
	}
	c := Cons{OriginalIdent: n, Env: env}
	env.AddSymbol(id, NewDefinition(c, ctx, NotForward)) // Does this correspond to our logic?
	return c
}

func (r *Resolver) defineDestructure(env *Env, defineTypeArg bool, n, ctx parser.Node, f func(string)) parser.Node {
	switch n := n.(type) {
	case parser.Ident:
		id := n.Name.Data
		if id == "_" {
			return n
		}
		defer f(id)
		forwardStatus := NotForward
		switch ctx := ctx.(type) {
		case parser.LetFunction:
			if ctx.Body == nil {
				forwardStatus = Forward
			}
		case parser.Function:
			if ctx.Body == nil {
				forwardStatus = Forward
			}
		}
		if localDef, ok := env.LookupLocal(id); ok {
			if localDef.ForwardStatus == Forward && forwardStatus == NotForward {
				localDef.ForwardStatus = ForwardResolved
				env.symbols[id] = localDef
				return localDef.Def
			}
			// we don't allow shadowing in local scope
			return parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
		}
		v := Var{OriginalIdent: n, Env: env}
		env.AddSymbol(id, NewDefinition(v, ctx, forwardStatus))
		return v
	case parser.CallExpr:
		// assuming this is a pattern case
		n.Elements[0] = r.Resolve(env, n.Elements[0])
		if tag, ok := n.Elements[0].(parser.SelectorExpr); ok {
			if _, ok := tag.Name.(Cons); ok {
				n.Elements[1] = r.defineDestructure(env, defineTypeArg, n.Elements[1], ctx, f)
			}
		}
		return n
		// switch tag := tag.(type) {
		// case parser.SelectorExpr:
		// 	if tag.Name
		// case Cons:
		// 	n.Elements[1] = defineDestructure(env, pat, ctx, f)
		// 	return n
		// }
	case Cons:
		return n
	case parser.SelectorExpr:
		if _, ok := n.Name.(Cons); ok {
			return n
		}
		return r.defineDestructure(env, defineTypeArg, r.Resolve(env, n), ctx, f)
		// assuming this is a pattern case
		// n.X = resolve(env, n.X)
		// switch x := n.X.(type) {
		// case Cons:
		// }
	// patterns should be handled by looking up the ident and seeing it's a Cons instead of a Var.
	// case parser.Pattern:
	// 	switch p := n.X.(type) {
	// 	case parser.Ident:
	// 		// this is a tag, so we don't need to do anything.
	// 		return n
	// 	case parser.Tuple:
	// 	case parser.CallExpr:
	// 	case parser.TypeAnnotation:
	// 	case parser.BinaryExpr:
	// 	}
	case parser.Tuple:
		for i := range n.Elements {
			n.Elements[i] = r.defineDestructure(env, defineTypeArg, n.Elements[i], ctx, f)
		}
		return n
	case parser.CommaElement:
		return r.defineDestructure(env, defineTypeArg, n.X, ctx, f)
	case parser.TypeAnnotation:
		n.Destructure = r.defineDestructure(env, defineTypeArg, n.Destructure, ctx, f)
		n.Type = r.defineResolveTypeAnnotation(env, defineTypeArg, n.Type, ctx)
		return n
	}
	panic(fmt.Sprintf("unreachable %T", n))
}

func (r *Resolver) resolveTypeAssignment(env *Env, defineTypeArg bool, elem parser.Tuple, lastCaller, ctx parser.Node) parser.Node {
	for i := range elem.Elements {
		comm := elem.Elements[i].(parser.CommaElement)
		binExp := comm.X.(parser.BinaryExpr)
		typeArg := binExp.Left.(parser.TypeArg)
		binExp.Left = Select(lastCaller, typeArg)
		binExp.Right = r.defineResolveTypeAnnotation(env, defineTypeArg, binExp.Right, ctx)
		comm.X = binExp
		elem.Elements[i] = comm
	}
	return elem
}

func (r *Resolver) defineResolveTypeAnnotation(env *Env, define bool, n, ctx parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.Tuple:
		for i := range n.Elements {
			n.Elements[i] = r.defineResolveTypeAnnotation(env, define, n.Elements[i], ctx)
		}
		return n
	// case parser.BinaryExpr:
	// 'a = Type
	// the type param here refers to the type params defined by the caller in the type application.
	// So if we had type Foo 'a = ...
	// and we do Foo ('a = int), we have to look up Foo to find the type param being referenced.
	case parser.CommaElement:
		n.X = r.defineResolveTypeAnnotation(env, define, n.X, ctx)
		return n
	case parser.TypeAnnotation:
		n.Destructure = r.defineDestructure(env, define, n.Destructure, ctx, func(id string) {})
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		return n
	case parser.Field:
		n.Name = r.defineDestructure(env, define, n.Name, ctx, func(id string) {})
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		// what's interesting is that n.Default can reference the function name and existing parameters
		// TODO: make sure to test this.
		n.Default = r.Resolve(env, n.Default)
		return n
	case parser.TypeArg:
		if d, ok := env.LookupStack(n.TypeArg.Data); ok {
			return d.Def
		}
		if define {
			d := NewDefinition(ResolvedTypeArg{OriginalTypeVar: n, Env: env}, ctx, NotForward)
			env.AddSymbol(n.TypeArg.Data, d)
			return d.Def
		} else {
			// return an UnresolvedTypeVar
			return UnresolvedTypeArg{OriginalTypeVar: n, Env: env}
		}
	case parser.Ident:
		return r.Resolve(env, n)
	case parser.FunctionType:
		n.Param = r.defineResolveTypeAnnotation(env, define, n.Param, ctx)
		for i := range n.Arrows {
			n.Arrows[i] = r.defineResolveTypeAnnotation(env, define, n.Arrows[i], ctx)
		}
		return n
	case parser.Arrow:
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		return n
	case parser.NillableType:
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		return n
	case parser.ForallType:
		forAllScope := env.AddScope()
		n.TypeArg = r.defineResolveTypeAnnotation(forAllScope, define, n.TypeArg, ctx)
		n.Type = r.defineResolveTypeAnnotation(forAllScope, define, n.Type, ctx)
		return n
	case parser.ArrayType:
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		return n
	case parser.SelectorExpr:
		return r.Resolve(env, n)
	case parser.CallExpr:
		var lastCaller parser.Node
		for i := range n.Elements {
			// if n.Elements[i] is an ident or selector expr, then we need to resolve it.
			// then keep track of it until we hit a tuple with a binary expr assignment to a TypeArg
			// then we replace the TypeArg with the resolved TypeVar.
			// if it's not there, then it's an UnresolvedTypeVar.
			switch elem := n.Elements[i].(type) {
			case parser.Ident:
				n.Elements[i] = r.Resolve(env, elem)
				lastCaller = n.Elements[i]
			case parser.SelectorExpr:
				n.Elements[i] = r.Resolve(env, elem)
				lastCaller = n.Elements[i]
			case parser.Tuple:
				if len(elem.Elements) >= 1 {
					if comm, ok := elem.Elements[0].(parser.CommaElement); ok {
						if binExp, ok := comm.X.(parser.BinaryExpr); ok {
							if _, ok := binExp.Left.(parser.TypeArg); ok && binExp.Op.Type == lexer.Assign {
								n.Elements[i] = r.resolveTypeAssignment(env, define, elem, lastCaller, ctx)
								break
							}
						}
					}
				}
				n.Elements[i] = r.defineResolveTypeAnnotation(env, define, elem, ctx)
			default:
				n.Elements[i] = r.defineResolveTypeAnnotation(env, define, elem, ctx)
			}
		}
		return n
	case parser.SumType:
		for i := range n.Elements {
			n.Elements[i] = r.defineResolveTypeAnnotation(env, define, n.Elements[i], ctx)
		}
		return n
	case parser.SumTypeElement:
		n.Name = defineTag(env, n.Name, n)
		n.Type = r.defineResolveTypeAnnotation(env, define, n.Type, ctx)
		return n
	case nil:
		return nil
	}
	return parser.Illegal{Node: n, Msg: "not a valid type"}
}

func setIllegals(env *Env, id string, n parser.Node) {
	if id == "_" {
		return
	}
	visit(n, func(body parser.Node, rec visitorRec) (parser.Node, bool) {
		switch body := body.(type) {
		case UnresolvedIdent:
			switch nid := body.OriginalIdent.(type) {
			case parser.Ident:
				sym := env.symbols[id]
				sym.Undefined[nid.Name.Data] = struct{}{}
				return body, false
			default:
				return rec(nid)
			}
		case Var:
			nid := body.OriginalIdent.(parser.Ident).Name.Data
			for udef := range body.Env.symbols[nid].Undefined {
				sym := env.symbols[id]
				sym.Undefined[udef] = struct{}{}
			}
			return body, false
			// case nil, parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			// 	return body, false
		}
		return rec(body) // if base cases aren't handled, this could recur forever.
	})
}

// This is basically like Go's top-level top-sort based name resolution, that handles cycles.
// Completely order-independent.
// Only declarations are allowed with the value restriction. No side effects.
func resolveTopLevel(env *Env, n parser.Node) parser.Node {
	panic("TODO")
}

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
			if binExp, ok := n.Elements[i].(parser.BinaryExpr); ok && binExp.Op.Type == lexer.Assign {
				binExp.Right = r.Resolve(env, binExp.Right)
				if id, ok := binExp.Left.(parser.Ident); ok {
					binExp.Left = r.defineDestructure(tupleScope, false, id, n, func(id string) {
						setIllegals(tupleScope, id, binExp.Right)
					})
				} else {
					// can't have a tuple assignment to a non-ident
					binExp.Left = parser.Illegal{Node: binExp.Left, Msg: "can't have a tuple assignment to a non-ident"}
				}
				n.Elements[i] = binExp
			} else {
				n.Elements[i] = r.Resolve(env, n.Elements[i])
			}
		}
		// if parent is a declaration, it should probably set child map
		return n
	case parser.LetDecl:
		// TODO: deal with type vars introduced in the type annotation
		// their scope should extend to the rhs
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
			patternCase = n.Cases[i].(parser.PatternCase)
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
	// case parser.TypeApplication:
	// 	for i := range n.Elements {
	// 		n.Elements[i] = r.resolve(env, n.Elements[i])
	// 	}
	// 	return n
	// case parser.SumType:
	// 	for i := range n.Elements {
	// 		n.Elements[i] = r.resolve(env, n.Elements[i])
	// 	}
	// 	return n
	// case parser.SumTypeElement:
	// 	n.Name = defineTag(env, n.Name, n)
	// 	n.Type = r.resolve(env, n.Type)
	// 	return n
	// case parser.ForallType:
	// TODO: just use defineResolveTypeAnnotation for all types?
	// return r.defineResolveTypeAnnotation(env, n, n)
	// typeScope := env.AddScope()
	// n.TypeArg = defineTypeParam(typeScope, n.TypeArg, n)
	// n.Type = r.resolve(typeScope, n.Type)
	// return n
	// case parser.FunctionType:
	// 	n.Param = r.resolve(env, n.Param)
	// 	for i := range n.Arrows {
	// 		n.Arrows[i] = r.resolve(env, n.Arrows[i])
	// 	}
	// 	return n
	// case parser.Field:
	// 	n.Name = r.defineDestructure(env, n.Name, n, func(id string) {})
	// 	n.Type = r.resolve(env, n.Type)
	// 	return n
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
	case parser.StringPart:
	case parser.String:
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
			if spath, ok := n.Path.(parser.String); ok {
				if spath, ok := spath.Parts[0].(parser.StringPart); ok {
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

// func defineTypeParam(bodyScope *Env, n parser.Node, ctx parser.TypeDecl) parser.Node {
// 	switch n := n.(type) {
// 	case parser.NamedTypeParameter:
// 	case parser.Tuple:
// 	case parser.TypeApplication:
// 	}
// 	// namedtypeparam
// 	// tupletypeparam
// 	// TypeApplication
// }

// func (r *resolver) resolveWithConstraint(env *Env, n parser.Node) parser.Node {
// 	switch n := n.(type) {
// 	case parser.NillableType:
// 		n.Type = r.resolveWithConstraint(env, n.Type)
// 		return n
// 	case parser.ForallType:
// 		forallScope := env.AddScope()
// 		n.Type = r.resolveWithConstraint(forallScope, n.Type)
// 		return n
// 	case parser.ArrayType:
// 		n.Type = r.resolveWithConstraint(env, n.Type)
// 		return n
// 	case parser.FunctionType:
// 	case parser.Ident:
// 		return r.resolve(env, n)
// 	case parser.Tuple:
// 		for i := range n.Elements {
// 			n.Elements[i] = r.resolveWithConstraint(env, n.Elements[i])
// 		}
// 		return n
// 	case parser.SelectorExpr:
// 		return r.resolve(env, n)
// 	case parser.NamedTypeArgument:
// 		// right now assuming this is at the top-level
// 		return defineTypeVar(env, n)
// 	case parser.TypeApplication:
// 	}
// 	// actually consider selector as well
// 	// what about nested withs?
// }

func resolveUnresolved(env *Env, body parser.Node) parser.Node {
	return visit(body, func(n parser.Node, rec visitorRec) (parser.Node, bool) {
		switch n := n.(type) {
		case UnresolvedIdent:
			switch nid := n.OriginalIdent.(type) {
			case parser.Ident:
				if def, ok := env.LookupLocal(nid.Name.Data); ok {
					// update this node to point to the definition.
					return def.Def, false
				}
				return n, false
			default:
				return rec(nid)
			}
		case parser.CommaElement:
			var quit bool
			if binExp, ok := n.X.(parser.BinaryExpr); ok {
				if nid, ok := binExp.Left.(Var); ok && binExp.Op.Type == lexer.Assign {
					binExp.Left = UnresolvedIdent(nid)
				} else {
					binExp.Left, quit = rec(binExp.Left)
				}
				if n.X = binExp; quit {
					return n, true
				}
				binExp.Right, quit = rec(binExp.Right)
				if n.X = binExp; quit {
					return n, true
				}
			} else {
				if n.X, quit = rec(n.X); quit {
					return n, true
				}
			}
			return n, false
		case Var:
			id := n.OriginalIdent.(parser.Ident).Name.Data
			if n.Env.symbols[id].ForwardStatus == Forward {
				// unresolved forward declaration.
				// this is illegal.
				return parser.Illegal{Node: n, Msg: fmt.Sprintf("%s was not defined", id)}, false
			}
			return n, false
		case TypeName:
			id := n.OriginalIdent.(parser.Ident).Name.Data
			if n.Env.symbols[id].ForwardStatus == Forward {
				// unresolved forward declaration.
				// this is illegal.
				return parser.Illegal{Node: n, Msg: fmt.Sprintf("%s was not defined", id)}, false
			}
			return n, false
		case parser.Illegal:
			// no need to resolve subnodes of an illegal node.
			return n, false
			// case parser.Illegal:
			// 	switch nid := n.Node.(type) {
			// 	case parser.Ident:
			// 		if def, ok := env.LookupLocal(nid.Name.Data); ok {
			// 			// update this node to point to the definition.
			// 			return def.Def, false
			// 		} else {
			// 			// should we modify the message to be that the ident is undefined?
			// 			n.Msg = fmt.Sprintf("undefined: %s", nid.Name.Data)
			// 			return n, false
			// 		}
			// 	case Var:
			// 		id := nid.OriginalIdent.(parser.Ident).Name.Data
			// 		if def, ok := env.LookupLocal(id); ok {
			// 			// update this node to point to the definition.
			// 			return def.Def, false
			// 		} else {
			// 			// should we modify the message to be that the ident is undefined?
			// 			n.Msg = fmt.Sprintf("undefined: %s", id)
			// 			return n, false
			// 		}
			// 	}
			// case nil, parser.Ident, parser.NamedTypeArgument, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			// 	return n, false
		}
		return rec(n)
	})
}

func findUndefined(nd parser.Node) (id string, found bool) {
	visit(nd, func(nd parser.Node, rec visitorRec) (parser.Node, bool) {
		switch nd := nd.(type) {
		case Var:
			udef := nd.Env.symbols[nd.OriginalIdent.(parser.Ident).Name.Data].Undefined
			for k := range udef {
				id = k
				found = true
				return nd, true
			}
			return nd, false
			// case parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			// 	return nd, false
		}
		return rec(nd)
	})
	return id, found
}
