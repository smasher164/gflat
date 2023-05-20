package names

import (
	"fmt"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
)

// Var is a Node that points into a Scope object.
type Var struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

func (v Var) ASTString(depth int) string {
	return fmt.Sprintf("Var: %s", v.OriginalIdent.ASTString(depth))
}

func (v Var) LeadingTrivia() []lexer.Token {
	return v.OriginalIdent.LeadingTrivia()
}

func (v Var) Span() lexer.Span {
	return v.OriginalIdent.Span()
}

type Cons struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

var _ parser.Node = Var{}

type Definition struct {
	Def       parser.Node
	Ctx       parser.Node         // additional context for error messages
	IsForward bool                // is this a forward declaration?
	Undefined map[string]struct{} // if function or type that references undefined symbols, we can track them here.
}

type Env struct {
	parent *Env
	// may need multiple tables for different kinds of symbols. i.e. packages, types, bindings.
	symbols map[string]Definition
}

func (e *Env) AddScope() *Env {
	return &Env{parent: e, symbols: make(map[string]Definition)}
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

// add builtins to this
var Universe = &Env{
	parent: nil,
}

// Replace ident with var that points to table?
// That means we need a var node.
// We'd like it to be a pointer to a Scope.
func Resolve(n parser.Node) parser.Node {
	return resolve(Universe.AddScope(), n)
}

func defineDestructure(env *Env, n, ctx parser.Node, f func(string)) parser.Node {
	switch n := n.(type) {
	case parser.Ident:
		id := n.Name.Data
		if id == "_" {
			return n
		}
		defer f(id)
		isForward := false
		switch ctx := ctx.(type) {
		case parser.LetFunction:
			isForward = ctx.Body == nil
		case parser.Function:
			isForward = ctx.Body == nil
		}
		if localDef, ok := env.LookupLocal(id); ok {
			// we don't allow shadowing in local scope
			if localDef.IsForward && !isForward {
				return localDef.Def
			}
			return parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", id)}
		}
		v := Var{OriginalIdent: n, Env: env}
		env.AddSymbol(id, Definition{Def: v, Ctx: ctx, IsForward: isForward, Undefined: make(map[string]struct{})}) // Does this correspond to our logic?
		return v
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
			n.Elements[i] = defineDestructure(env, n.Elements[i], ctx, f)
		}
		return n
	case parser.TupleElement:
		return defineDestructure(env, n.X, ctx, f)
	case parser.TypeAnnotation:
		panic("todo")
	case parser.Param:
		return defineDestructure(env, n.Name, ctx, f)
	}
	panic(fmt.Sprintf("unreachable %T", n))
}

/*
	func(body parser.Node) parser.Node {
			switch body := body.(type) {
			case parser.Illegal:
				switch nid := body.Node.(type) {
				case parser.Ident:
					sym := env.symbols[id]
					sym.Undefined[nid.Name.Data] = struct{}{}
				default:
					setIllegals(env, id, nid)
				}
			case Var:
			default:
				return body
			}
		}
*/

type visitorRec func(parser.Node) (parser.Node, bool)
type visitorFunc func(parser.Node, visitorRec) (parser.Node, bool)

func setIllegals(env *Env, id string, n parser.Node) {
	if id == "_" {
		return
	}
	visit(n, func(body parser.Node, rec visitorRec) (parser.Node, bool) {
		switch body := body.(type) {
		case parser.Illegal:
			switch nid := body.Node.(type) {
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
		case parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			return body, false
		}
		return rec(body) // if base cases aren't handled, this could recur forever.
	})
}

func visit1(n parser.Node, f visitorFunc) (parser.Node, bool) {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	var quit bool
	switch n := n.(type) {
	case parser.BinaryExpr:
		if n.Left, quit = f(n.Left, rec); quit {
			return n, quit
		}
		n.Right, quit = f(n.Right, rec)
		return n, quit
	case parser.Block:
		for i := range n.Body {
			if n.Body[i], quit = f(n.Body[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.Tuple:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.TupleElement:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.CallExpr:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.LetFunction:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.Function:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		if n.Signature, quit = f(n.Signature, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.LetDecl:
		if n.Destructure, quit = f(n.Destructure, rec); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs, rec)
		return n, quit
	case parser.VarDecl:
		if n.Destructure, quit = f(n.Destructure, rec); quit {
			return n, quit
		}
		n.Rhs, quit = f(n.Rhs, rec)
		return n, quit
	case parser.Stmt:
		n.Stmt, quit = f(n.Stmt, rec)
		return n, quit
	case parser.Illegal:
		n.Node, quit = f(n.Node, rec)
		return n, quit
	case parser.PrefixExpr:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.PostfixExpr:
		n.X, quit = f(n.X, rec)
		return n, quit
	case parser.If:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.IfElse:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		if n.Body, quit = f(n.Body, rec); quit {
			return n, quit
		}
		n.ElseBody, quit = f(n.ElseBody, rec)
		return n, quit
	default:
		return f(n, rec)
	}
}

func visit(n parser.Node, f visitorFunc) parser.Node {
	rec := func(x parser.Node) (parser.Node, bool) { return visit1(x, f) }
	n, _ = f(n, rec)
	return n
}

func resolve(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.BinaryExpr:
		n.Left = resolve(env, n.Left)
		n.Right = resolve(env, n.Right)
		return n
	case parser.Stmt:
		n.Stmt = resolve(env, n.Stmt)
		return n
	case parser.File:
		n.Body = resolve(env, n.Body)
		return n
	case parser.Ident:
		// what if ident is "_"?
		// if n.Name.Data == "_" {
		// 	return n
		// }
		def, ok := env.LookupStack(n.Name.Data)
		if ok {
			return def.Def
		}
		return parser.Illegal{Node: n, Msg: fmt.Sprintf("undefined: %s", n.Name.Data)}
	case parser.Illegal:
		n.Node = resolve(env, n.Node)
		return n // At what point do we reject programs with illegal nodes?
	case parser.Block:
		env := env.AddScope()
		for i := range n.Body {
			n.Body[i] = resolve(env, n.Body[i])
		}
		return resolveIllegals(env, n)
	case parser.EmptyExpr:
		return n
	case parser.TypeAnnotation:
	case parser.TupleParam:
	case parser.FunctionSignature:
	case parser.Param:
	case parser.Arrow:
	case parser.LetFunction:
		// what if someone write let _ x = 1?
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			v := v
			delete(v.Undefined, id)
		}
		n.Name = defineDestructure(env, n.Name, n, func(string) {})
		n.Signature = resolve(env, n.Signature)
		sig := n.Signature.(parser.FunctionSignature)
		// Add function name to scope. But allow it to get shadowed.
		fnNameScope := env.AddScope().AddSymbol(id, Definition{Def: n.Name, Ctx: n, Undefined: make(map[string]struct{})})
		bodyScope := fnNameScope.AddScope()
		sig.Param = defineDestructure(bodyScope, sig.Param, sig, func(id string) {})
		n.Signature = sig
		n.Body = resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.Function:
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			v := v
			delete(v.Undefined, id)
		}
		n.Name = defineDestructure(env, n.Name, n, func(string) {})
		n.Signature = resolve(env, n.Signature)
		sig := n.Signature.(parser.FunctionSignature)
		// Add function name to scope. But allow it to get shadowed.
		fnNameScope := env.AddScope().AddSymbol(id, Definition{Def: n.Name, Ctx: n, Undefined: make(map[string]struct{})})
		bodyScope := fnNameScope.AddScope()
		sig.Param = defineDestructure(bodyScope, sig.Param, sig, func(id string) {})
		n.Signature = sig
		n.Body = resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.TupleElement:
		n.X = resolve(env, n.X)
		return n
	case parser.Tuple:
		// Note: Tuple literals have an ambiguity in that they can be used for struct literals and map literals.
		// Since we don't have type information, we can't resolve this ambiguity here.
		// For struct literals, if a field name is assigned, and that ident isn't in scope, we treat that as an error
		// and fix it up in type checking. If it *is* in scope, it is a false positive.
		// In reality, if it's a struct, there should be a new scope created for the struct fields that are introduced.
		for i := range n.Elements {
			n.Elements[i] = resolve(env, n.Elements[i])
		}
		return n
	case parser.LetDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = defineDestructure(env, n.Destructure, n, func(id string) {
			// setIllegalsUsingWalk(env, id, n.Rhs)
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.VarDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = defineDestructure(env, n.Destructure, n, func(id string) {
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.IfHeader:
		n.Cond = resolve(env, n.Cond)
		return n
	case parser.If:
		n.IfHeader = resolve(env, n.IfHeader)
		n.Body = resolve(env, n.Body)
	case parser.IfElse:
		n.IfHeader = resolve(env, n.IfHeader)
		n.Body = resolve(env, n.Body)
		n.ElseBody = resolve(env, n.ElseBody)
		return n
	case parser.IfMatch:
		n.IfHeader = resolve(env, n.IfHeader)
		for i := range n.Cases {
			/*
				1. Add a scope for each case.
				2. Add undefined from IfHeader.
			*/
			patternCase := n.Cases[i].(parser.PatternCase)
			patternScope := env.AddScope()
			// TODO: defineDestructure for pattern cases needs to consider Cons vs Var.
			n.Cases[i] = defineDestructure(patternScope, patternCase.Pattern, patternCase, func(id string) {
				setIllegals(patternScope, id, n.IfHeader)
			})
			patternCase = n.Cases[i].(parser.PatternCase)
			patternCase.Guard = resolve(patternScope, patternCase.Guard)
			patternCase.Expr = resolve(patternScope, patternCase.Expr)
			n.Cases[i] = patternCase
		}
		return n
	case parser.PatternCase:
		// patternScope := env.AddScope()
		// n.Pattern = defineDestructure(patternScope, n.Pattern, n, func(id string) {
		// 	//
		// })
	case parser.TypeDecl:
	case parser.Number:
	case parser.NamedTypeParameter:
	case parser.NamedTypeArgument:
	case parser.TypeApplication:
	case parser.NamedType:
	case parser.SumType:
	case parser.SumTypeElement:
	case parser.ForallType:
	case parser.FunctionType:
	case parser.Field:
	case parser.PrefixExpr:
		n.X = resolve(env, n.X)
		return n
	case parser.CallExpr:
		for i := range n.Elements {
			n.Elements[i] = resolve(env, n.Elements[i]) // TODO: fix refs to Illegal node later.
			// check if caller references an ident whose len(def.Undefined) != 0
			// if so, error.
			nd := n.Elements[i]
			if id, ok := findUndefined(nd); ok {
				n.Elements[i] = parser.Illegal{Node: nd, Msg: fmt.Sprintf("%s is used in application before it is declared", id)}
			}
		}
		return n
	case parser.PostfixExpr:
		n.X = resolve(env, n.X)
		return n
	case parser.SelectorExpr:
	case parser.StringPart:
	case parser.String:
	case parser.IndexExpr:
		n.X = resolve(env, n.X)
		n.Index = resolve(env, n.Index)
		return n
	case parser.ImportDecl:
	case parser.ImportDeclPackage:
	case parser.With:
	case parser.ImplDecl:
	case parser.ArrayType:
	case parser.NillableType:
	case Var: // is it possible to hit this case?
	}
	return n
}

func resolveIllegals(env *Env, body parser.Node) parser.Node {
	return visit(body, func(n parser.Node, rec visitorRec) (parser.Node, bool) {
		switch n := n.(type) {
		case parser.Illegal:
			switch nid := n.Node.(type) {
			case parser.Ident:
				if def, ok := env.LookupLocal(nid.Name.Data); ok {
					// update this node to point to the definition.
					return def.Def, false
				} else {
					// should we modify the message to be that the ident is undefined?
					n.Msg = fmt.Sprintf("undefined: %s", nid.Name.Data)
					return n, false
				}
			case Var:
				id := nid.OriginalIdent.(parser.Ident).Name.Data
				if def, ok := env.LookupLocal(id); ok {
					// update this node to point to the definition.
					return def.Def, false
				} else {
					// should we modify the message to be that the ident is undefined?
					n.Msg = fmt.Sprintf("undefined: %s", id)
					return n, false
				}
			}
		case Var, parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			return n, false
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
		case parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			return nd, false
		}
		return rec(nd)
	})
	return id, found
}
