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
		defer f(n.Name.Data)
		isForward := false
		switch ctx := ctx.(type) {
		case parser.LetFunction:
			isForward = ctx.Body == nil
		case parser.Function:
			isForward = ctx.Body == nil
		}
		if localDef, ok := env.LookupLocal(n.Name.Data); ok {
			// we don't allow shadowing in local scope
			if localDef.IsForward && !isForward {
				return localDef.Def
			}
			return parser.Illegal{Node: localDef.Def, Msg: fmt.Sprintf("%s was already defined", n.Name.Data)}
		}
		v := Var{OriginalIdent: n, Env: env}
		env.AddSymbol(n.Name.Data, Definition{Def: v, Ctx: ctx, IsForward: isForward, Undefined: make(map[string]struct{})}) // Does this correspond to our logic?
		return v
	case parser.Tuple:
		for i := range n.Elements {
			n.Elements[i] = defineDestructure(env, n.Elements[i], ctx, f)
		}
		return n
	case parser.TupleElement:
		return defineDestructure(env, n.X, ctx, f)
	case parser.TypeAnnotation:
		panic("todo")
	}
	panic(fmt.Sprintf("unreachable %T", n))
}

func setIllegals(env *Env, id string, body parser.Node) {
	switch body := body.(type) {
	case parser.Illegal:
		if nid, ok := body.Node.(parser.Ident); ok {
			sym := env.symbols[id]
			sym.Undefined[nid.Name.Data] = struct{}{}
		}
	case Var:
		nid := body.OriginalIdent.(parser.Ident).Name.Data
		for udef := range body.Env.symbols[nid].Undefined {
			sym := env.symbols[id]
			sym.Undefined[udef] = struct{}{}
		}
	case parser.BinaryExpr:
		setIllegals(env, id, body.Left)
		setIllegals(env, id, body.Right)
	case parser.Block:
		for i := range body.Body {
			setIllegals(env, id, body.Body[i])
		}
	case parser.CallExpr:
		for i := range body.Elements {
			setIllegals(env, id, body.Elements[i]) // do we need thet last element?
		}
	}
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
		// add an ident to requires if it's from the scope we care about.
		// if def, ok := env.LookupLocal(n.Name.Data); ok {
		// 	requires[n.Name.Data] = struct{}{}
		// 	return def.Def // might as well return the definition here, since we have it.
		// }
		// does this have to take mutual recursion into account?
		def, ok := env.LookupStack(n.Name.Data)
		// if def is from the same scope as the requires, add it to the requires set.
		if ok {
			return def.Def
		}

		return parser.Illegal{Node: n, Msg: fmt.Sprintf("undefined: %s", n.Name.Data)}
		// this is a lookup, not an introduction.
		// v := Var{OriginalIdent: n, Env: env}
		// env.AddSymbol(n.Name.Data, v) // Does this correspond to our logic?
		// return v
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
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			v := v
			delete(v.Undefined, id)
		}
		n.Name = defineDestructure(env, n.Name, n, func(string) {})
		n.Signature = resolve(env, n.Signature)
		// Add function name to scope. But allow it to get shadowed.
		fnNameScope := env.AddScope().AddSymbol(id, Definition{Def: n.Name, Ctx: n, Undefined: make(map[string]struct{})})
		bodyScope := fnNameScope.AddScope()
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
		// Add function name to scope. But allow it to get shadowed.
		fnNameScope := env.AddScope().AddSymbol(id, Definition{Def: n.Name, Ctx: n, Undefined: make(map[string]struct{})})
		bodyScope := fnNameScope.AddScope()
		n.Body = resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.TupleElement:
	case parser.Tuple:
	case parser.LetDecl:
		n.Rhs = resolve(env, n.Rhs) // Does this correspond to my strategy?
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		// id := n.Destructure.(parser.Ident).Name.Data
		n.Destructure = defineDestructure(env, n.Destructure, n, func(id string) {
			setIllegals(env, id, n.Rhs)
		})
		// setIllegals(env, id, n.Rhs)
		return n
	case parser.VarDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		// id := n.Destructure.(parser.Ident).Name.Data
		n.Rhs = resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = defineDestructure(env, n.Destructure, n, func(id string) {
			setIllegals(env, id, n.Rhs)
		})
		// setIllegals(env, id, n.Rhs)
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
	case parser.CallExpr:
		for i := range n.Elements {
			n.Elements[i] = resolve(env, n.Elements[i]) // TODO: fix refs to Illegal node later.
			// check if caller references an ident whose len(def.Undefined) != 0
			// if so, error.
			if i != len(n.Elements)-1 {
				nd := n.Elements[i]
				if id, ok := findUndefined(nd); ok {
					return parser.Illegal{Node: n, Msg: fmt.Sprintf("%s called before it is declared", id)}
				}
				// for s := range env.symbols[id].Undefined {
				// 	return parser.Illegal{Node: n, Msg: fmt.Sprintf("cannot bind %s before declaring %s", id, s)}
				// }
			}
		}
	case parser.PostfixExpr:
	case parser.SelectorExpr:
	case parser.PatternCase:
	case parser.StringPart:
	case parser.String:
	case parser.IndexExpr:
	case parser.ImportDecl:
	case parser.ImportDeclPackage:
	case parser.Where:
	case parser.ImplDecl:
	case parser.ArrayType:
	case parser.NillableType:
	case Var: // is it possible to hit this case?
	}
	return n
}

func resolveIllegals(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.Block:
		for i := range n.Body {
			n.Body[i] = resolveIllegals(env, n.Body[i])
		}
		return n
	case parser.Illegal:
		if nid, ok := n.Node.(parser.Ident); ok {
			if def, ok := env.LookupLocal(nid.Name.Data); ok {
				// update this node to point to the definition.
				return def.Def
			} else {
				// should we modify the message to be that the ident is undefined?
				n.Msg = fmt.Sprintf("undefined: %s", nid.Name.Data)
				return n
			}
		}
	case parser.BinaryExpr:
		n.Left = resolveIllegals(env, n.Left)
		n.Right = resolveIllegals(env, n.Right)
		return n
	case parser.CallExpr:
		for i := range n.Elements {
			n.Elements[i] = resolveIllegals(env, n.Elements[i])
		}
		return n
	case parser.Function:
		n.Body = resolveIllegals(env, n.Body)
		return n
	case parser.Stmt:
		n.Stmt = resolveIllegals(env, n.Stmt)
		return n
	}
	return n
}

func findUndefined(nd parser.Node) (string, bool) {
	switch nd := nd.(type) {
	case Var:
		udef := nd.Env.symbols[nd.OriginalIdent.(parser.Ident).Name.Data].Undefined
		for k := range udef {
			return k, true
		}
	}
	return "", false
}
