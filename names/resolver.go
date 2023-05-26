package names

import (
	"fmt"
	"path"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"golang.org/x/exp/maps"
	"golang.org/x/mod/module"
)

var (
	_ parser.Node = Var{}
	_ parser.Node = TypeName{}
	_ parser.Node = UnresolvedIdent{}
	_ parser.Node = PackageName{}
	_ parser.Node = Cons{}
)

// Var is a variable Node that points into a Scope object.
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

type TypeName struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

func (v TypeName) ASTString(depth int) string {
	return fmt.Sprintf("TypeName: %s", v.OriginalIdent.ASTString(depth))
}

func (v TypeName) LeadingTrivia() []lexer.Token {
	return v.OriginalIdent.LeadingTrivia()
}

func (v TypeName) Span() lexer.Span {
	return v.OriginalIdent.Span()
}

// UnresolvedIdent is an identifier that has not been resolved yet.
type UnresolvedIdent struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

func (v UnresolvedIdent) ASTString(depth int) string {
	return fmt.Sprintf("UnknownIdent: %s", v.OriginalIdent.ASTString(depth))
}

func (v UnresolvedIdent) LeadingTrivia() []lexer.Token {
	return v.OriginalIdent.LeadingTrivia()
}

func (v UnresolvedIdent) Span() lexer.Span {
	return v.OriginalIdent.Span()
}

type PackageName struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

func (v PackageName) ASTString(depth int) string {
	return fmt.Sprintf("PackageName: %s", v.OriginalIdent.ASTString(depth))
}

func (v PackageName) LeadingTrivia() []lexer.Token {
	return v.OriginalIdent.LeadingTrivia()
}

func (v PackageName) Span() lexer.Span {
	return v.OriginalIdent.Span()
}

// Cons is a constructor Node that points into a Scope object.
type Cons struct {
	OriginalIdent parser.Node
	// Reference to environment here.
	Env *Env
}

func (c Cons) ASTString(depth int) string {
	return fmt.Sprintf("Cons: %s", c.OriginalIdent.ASTString(depth))
}

func (c Cons) LeadingTrivia() []lexer.Token {
	return c.OriginalIdent.LeadingTrivia()
}

func (c Cons) Span() lexer.Span {
	return c.OriginalIdent.Span()
}

type ForwardStatus int

const (
	NotForward ForwardStatus = iota
	Forward
	ForwardResolved
)

// TODO: turn this into an interface so that the typechecker can extend it.
type Definition struct {
	Def           parser.Node
	Ctx           parser.Node         // additional context for error messages
	ForwardStatus ForwardStatus       // is this a forward declaration?
	Undefined     map[string]struct{} // if function or type that references undefined symbols, we can track them here.
	Child         *Env
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
		parent: e,
		// packages: make(map[string]Definition),
		// types:    make(map[string]Definition),
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

// add builtins to this
var Universe = &Env{
	parent: nil,
}

// Replace ident with var that points to table?
// That means we need a var node.
// We'd like it to be a pointer to a Scope.
func Resolve(n parser.Node) parser.Node {
	return (&resolver{}).resolve(Universe, n)
}

type resolver struct {
	imports map[string]struct{}
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
		selID := sel.(parser.Ident).Name.Data
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
	// visit(x, func(n parser.Node, rec visitorRec) (parser.Node, bool) {
	// 	switch n := n.(type) {
	// 	case parser.Tuple:
	// 	}
	// })
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

func (r *resolver) defineDestructure(env *Env, n, ctx parser.Node, f func(string)) parser.Node {
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
		n.Elements[0] = r.resolve(env, n.Elements[0])
		if tag, ok := n.Elements[0].(parser.SelectorExpr); ok {
			if _, ok := tag.Name.(Cons); ok {
				n.Elements[1] = r.defineDestructure(env, n.Elements[1], ctx, f)
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
		return r.defineDestructure(env, r.resolve(env, n), ctx, f)
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
			n.Elements[i] = r.defineDestructure(env, n.Elements[i], ctx, f)
		}
		return n
	case parser.CommaElement:
		return r.defineDestructure(env, n.X, ctx, f)
	case parser.TypeAnnotation:
		n.Destructure = r.defineDestructure(env, n.Destructure, ctx, f)
		n.Type = r.resolve(env, n.Type)
		return n
	case parser.Param:
		return r.defineDestructure(env, n.Name, ctx, f)
	}
	panic(fmt.Sprintf("unreachable %T", n))
}

type visitorRec func(parser.Node) (parser.Node, bool)
type visitorFunc func(parser.Node, visitorRec) (parser.Node, bool)

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
		case nil, parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
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
	case parser.CommaElement:
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
	case parser.IfHeader:
		n.Cond, quit = f(n.Cond, rec)
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
	case parser.IfMatch:
		if n.IfHeader, quit = f(n.IfHeader, rec); quit {
			return n, quit
		}
		for i := range n.Cases {
			if n.Cases[i], quit = f(n.Cases[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.PatternCase:
		if n.Pattern, quit = f(n.Pattern, rec); quit {
			return n, quit
		}
		if n.Guard, quit = f(n.Guard, rec); quit {
			return n, quit
		}
		n.Expr, quit = f(n.Expr, rec)
		return n, quit
	case parser.TypeDecl:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		// ignoring type params for now
		// also ignoring default params for now
		n.Body, quit = f(n.Body, rec)
		return n, quit
	case parser.SumType:
		for i := range n.Elements {
			if n.Elements[i], quit = f(n.Elements[i], rec); quit {
				return n, quit
			}
		}
		return n, quit
	case parser.SumTypeElement:
		if n.Name, quit = f(n.Name, rec); quit {
			return n, quit
		}
		n.Type, quit = f(n.Type, rec)
		return n, quit
	case parser.SelectorExpr:
		if n.X, quit = f(n.X, rec); quit {
			return n, quit
		}
		n.Name, quit = f(n.Name, rec)
		return n, quit
	case Var:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case TypeName:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case UnresolvedIdent:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case PackageName:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
		return n, quit
	case Cons:
		n.OriginalIdent, quit = f(n.OriginalIdent, rec)
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

// This is basically like Go's top-level top-sort based name resolution, that handles cycles.
// Completely order independent.
// Only declarations are allowed with the value restriction. No side effects.
func resolveTopLevel(env *Env, n parser.Node) parser.Node {
	panic("TODO")
}

func (r *resolver) resolve(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.BinaryExpr:
		n.Left = r.resolve(env, n.Left)
		n.Right = r.resolve(env, n.Right)
		return n
	case parser.Stmt:
		n.Stmt = r.resolve(env, n.Stmt)
		return n
	case parser.Package:
		n.Imports = make(map[string]struct{})
		// introduce package scope
		packageScope := env.AddScope()
		for i := range n.PackageFiles {
			n.PackageFiles[i] = resolveTopLevel(packageScope, n.PackageFiles[i])
			if f, ok := n.PackageFiles[i].(parser.File); ok {
				maps.Copy(n.Imports, f.Imports)
			}
		}
		for i := range n.ScriptFiles {
			n.ScriptFiles[i] = r.resolve(env.AddScope(), n.ScriptFiles[i])
			if f, ok := n.ScriptFiles[i].(parser.File); ok {
				maps.Copy(n.Imports, f.Imports)
			}
		}
		// TODO: resolve cross-file identifiers
		return n
	case parser.File:
		r.imports = make(map[string]struct{})
		n.Body = r.resolve(env, n.Body)
		n.Imports = r.imports
		r.imports = nil
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
	case parser.Illegal:
		n.Node = r.resolve(env, n.Node)
		return n // At what point do we reject programs with illegal nodes?
	case parser.Block:
		env := env.AddScope()
		for i := range n.Body {
			n.Body[i] = r.resolve(env, n.Body[i])
		}
		return resolveUnresolved(env, n)
	case parser.EmptyExpr:
		return n
	case parser.TypeAnnotation:
	case parser.FunctionSignature:
	case parser.Param:
	case parser.Arrow:
		n.Type = r.resolve(env, n.Type)
	case parser.LetFunction:
		// what if someone write let _ x = 1?
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			delete(v.Undefined, id)
		}
		n.Name = r.defineDestructure(env, n.Name, n, func(string) {})
		n.Signature = r.resolve(env, n.Signature)
		sig := n.Signature.(parser.FunctionSignature)
		bodyScope := env.AddScope()
		sig.Param = r.defineDestructure(bodyScope, sig.Param, sig, func(id string) {})
		n.Signature = sig
		n.Body = r.resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.Function:
		// We don't have to do closure conversion cause the target supports them.
		id := n.Name.(parser.Ident).Name.Data
		for _, v := range env.symbols {
			delete(v.Undefined, id)
		}
		n.Name = r.defineDestructure(env, n.Name, n, func(string) {})
		n.Signature = r.resolve(env, n.Signature)
		sig := n.Signature.(parser.FunctionSignature)
		bodyScope := env.AddScope()
		sig.Param = r.defineDestructure(bodyScope, sig.Param, sig, func(id string) {})
		n.Signature = sig
		n.Body = r.resolve(bodyScope, n.Body)
		setIllegals(env, id, n.Body)
		return n
	case parser.CommaElement:
		n.X = r.resolve(env, n.X)
		return n
	case parser.Tuple:
		// Note: Tuple literals have an ambiguity in that they can be used for struct literals and map literals.
		// Since we don't have type information, we can't resolve this ambiguity here.
		// For struct literals, if a field name is assigned, and that ident isn't in scope, we treat that as an error
		// and fix it up in type checking. If it *is* in scope, it is a false positive.
		// In reality, if it's a struct, there should be a new scope created for the struct fields that are introduced.
		tupleScope := env.AddScope()
		for i := range n.Elements {
			n.Elements[i] = r.resolve(tupleScope, n.Elements[i])
		}
		return n
	case parser.LetDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = r.resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = r.defineDestructure(env, n.Destructure, n, func(id string) {
			// setIllegalsUsingWalk(env, id, n.Rhs)
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.VarDecl:
		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		n.Rhs = r.resolve(env, n.Rhs) // Does this correspond to my strategy?
		n.Destructure = r.defineDestructure(env, n.Destructure, n, func(id string) {
			setIllegals(env, id, n.Rhs)
		})
		return n
	case parser.IfHeader:
		n.Cond = r.resolve(env, n.Cond)
		return n
	case parser.If:
		n.IfHeader = r.resolve(env, n.IfHeader)
		n.Body = r.resolve(env, n.Body)
	case parser.IfElse:
		n.IfHeader = r.resolve(env, n.IfHeader)
		n.Body = r.resolve(env, n.Body)
		n.ElseBody = r.resolve(env, n.ElseBody)
		return n
	case parser.IfMatch:
		n.IfHeader = r.resolve(env, n.IfHeader)
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
			patternCase.Pattern = r.defineDestructure(patternScope, pat, patternCase, func(id string) {
				setIllegals(patternScope, id, n.IfHeader)
			})
			patternCase.Guard = r.resolve(patternScope, patternCase.Guard)
			patternCase.Expr = r.resolve(patternScope, patternCase.Expr)
			n.Cases[i] = patternCase
		}
		return n
	case parser.PatternCase:
		// patternScope := env.AddScope()
		// n.Pattern = defineDestructure(patternScope, n.Pattern, n, func(id string) {
		// 	//
		// })
	case parser.TypeDecl:
		// TODO: deal with type params
		// TODO: deal with default params
		// TODO: deal with With clauses
		// n.Name = defineDestructure(env, n.Name, n, func(string) {})
		id := n.Name.(parser.Ident).Name.Data
		n.Name = defineType(env, n.Name, n)
		bodyScope := env.AddScope()
		n.Body = r.resolve(bodyScope, n.Body)
		// copy bodyScope.symbols to def.children
		for k, v := range bodyScope.symbols {
			if sym, ok := env.symbols[id]; ok {
				if sym.Child == nil {
					sym.Child = env.AddScope()
				}
				sym.Child.symbols[k] = v
				env.symbols[id] = sym
			}
		}
		return n
		// setIllegals
	case parser.Number:
	case parser.NamedTypeParameter:
	case parser.NamedTypeArgument:
	case parser.TypeApplication:
		for i := range n.Elements {
			n.Elements[i] = r.resolve(env, n.Elements[i])
		}
		return n
	case parser.NamedType:
	case parser.SumType:
		for i := range n.Elements {
			n.Elements[i] = r.resolve(env, n.Elements[i])
		}
		return n
	case parser.SumTypeElement:
		n.Name = defineTag(env, n.Name, n)
		n.Type = r.resolve(env, n.Type)
		return n
	case parser.ForallType:
	case parser.FunctionType:
		n.Param = r.resolve(env, n.Param)
		for i := range n.Arrows {
			n.Arrows[i] = r.resolve(env, n.Arrows[i])
		}
		return n
	case parser.Field:
		n.Name = r.defineDestructure(env, n.Name, n, func(id string) {})
		n.Type = r.resolve(env, n.Type)
		return n
	case parser.PrefixExpr:
		n.X = r.resolve(env, n.X)
		return n
	case parser.CallExpr:
		for i := range n.Elements {
			n.Elements[i] = r.resolve(env, n.Elements[i])
			// check if caller references an ident whose len(def.Undefined) != 0
			// if so, error.
			nd := n.Elements[i]
			if id, ok := findUndefined(nd); ok {
				n.Elements[i] = parser.Illegal{Node: nd, Msg: fmt.Sprintf("%s is used in application before it is declared", id)}
			}
		}
		return n
	case parser.PostfixExpr:
		n.X = r.resolve(env, n.X)
		return n
	case parser.SelectorExpr:
		n.X = r.resolve(env, n.X)
		n.Name = Select(n.X, n.Name)
		return n
	case parser.StringPart:
	case parser.String:
	case parser.IndexExpr:
		n.X = r.resolve(env, n.X)
		for i := range n.IndexElements {
			n.IndexElements[i] = r.resolve(env, n.IndexElements[i])
		}
		return n
	case parser.ImportDecl:
		n.Package = r.resolve(env, n.Package)
	case parser.ImportDeclPackage:
		var importPath string
		if path, ok := n.Path.(parser.String); ok {
			if path, ok := path.Parts[0].(parser.StringPart); ok {
				importPath = path.Lit.Data
				r.imports[importPath] = struct{}{}
			}
		}
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
					bind.Elements[i] = r.defineDestructure(env, elem, n, func(id string) {})
				case parser.IndexExpr:
					bind.Elements[i] = r.defineDestructure(env, elem.X, n, func(id string) {})
					for j := range elem.IndexElements {
						if indexElem, ok := elem.IndexElements[j].(parser.Ident); ok {
							elem.IndexElements[j] = r.defineDestructure(env, indexElem, n, func(id string) {})
						}
					}
				}
			}
			n.Binding = bind
		default:
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
	case parser.With:
	case parser.ImplDecl:
	case parser.ArrayType:
	case parser.NillableType:
		n.Type = r.resolve(env, n.Type)
		return n
	case Var: // is it possible to hit this case?
	case TypeName:
	case UnresolvedIdent:
	case Cons:
	case PackageName:
	}
	return n
}

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
				if nid, ok := binExp.Left.(Var); ok && binExp.Op.Type == lexer.Equals {
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
		case nil, parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
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
			return nd, false
		case parser.Ident, parser.Number, parser.String, parser.StringPart, parser.FunctionSignature:
			return nd, false
		}
		return rec(nd)
	})
	return id, found
}
