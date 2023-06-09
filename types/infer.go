package types

import (
	"fmt"

	"github.com/smasher164/gflat/parser"
)

// type TypedNode interface {
// 	parser.Node
// 	Type() Type
// }

type TypedNode struct {
	parser.Node
	Type Type
}

func indent(depth int) string {
	return fmt.Sprintf("%*s", depth*2, "")
}

func (t TypedNode) ASTString(depth int) string {
	// prints the underlying node along with its type.
	return fmt.Sprintf("TypedNode\n%sNode: %s\n%sType: %s", indent(depth+1), t.Node.ASTString(depth+1), indent(depth+1), t.Type)
}

// Infer accepts an untyped AST and returns a typed AST, resolving names and inferring types.
func (r *Resolver) Infer(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.File:
		n.Body = r.Infer(env, n.Body)
		if tbody, ok := n.Body.(TypedNode); ok {
			return TypedNode{Node: n, Type: tbody.Type}
		}
		return parser.Illegal{Node: n, Msg: "file body does not have a valid type"}
	case parser.Block:
		bodyScope := env.AddScope()
		var lastType Type = Unit
		for i := range n.Body {
			n.Body[i] = r.Infer(bodyScope, n.Body[i])
			if tbody, ok := n.Body[i].(TypedNode); ok {
				lastType = tbody.Type
			}
		}
		// n = fixUnresolved(n)
		return TypedNode{Node: n, Type: lastType}
	case parser.BinaryExpr:
		// just for demo purposes. this should be using traits.
		n.Left = r.Check(env, n.Left, Int)
		n.Right = r.Check(env, n.Right, Int)
		return TypedNode{Node: n, Type: Int}
		// tRight := r.Infer(env, n.Right)
		// switch n.Op {
		// case DotDot, Plus, Minus, Times, Divide, Remainder, LeftShift, RightShift, And, Or, Caret, LogicalAnd, LogicalOr, LogicalEquals, NotEquals, Equals, LessThan, LessThanEquals, GreaterThan, GreaterThanEquals, LeftArrow, Exponentiation, Colon:
		// }
	case parser.Number:
		// TODO: Number trait
		// t := r.freshTypeVar(env)

		// return TypedNode{Node: n, Type: Int}
	case parser.Stmt:
		n.Stmt = r.Infer(env, n.Stmt)
		return TypedNode{Node: n, Type: Unit}
	case parser.LetDecl:
		// ignore tuples for now
		// no let-gen or inferred function signatures for now
		// get type annotation
		rhsEnv := env.AddScope()
		switch des := n.Destructure.(type) {
		case parser.Ident:
			id := des.Name.Data
			n.Rhs = r.Infer(rhsEnv, n.Rhs)
			if _, ok := env.LookupLocal(id); ok {
				n.Destructure = parser.Illegal{Node: des, Msg: fmt.Sprintf("redefinition of %s", id)}
			} else {
				if trhs, ok := n.Rhs.(TypedNode); ok {
					n.Destructure = TypedNode{Node: Var{OriginalIdent: des, Env: env}, Type: trhs.Type}
				} else {
					n.Destructure = parser.Illegal{Node: des, Msg: "rhs does not have a valid type"}
				}
				if id != "_" {
					env.AddSymbol(id, NewDefinition(n.Destructure, des, NotForward))
				}
			}
		}
		return n
	case parser.Ident:
		def, ok := env.LookupStack(n.Name.Data)
		if ok {
			return def.Def
		}
		return UnresolvedIdent{OriginalIdent: n, Env: env}
		// add binding to current scope

		/*
		   this is how our old resolver code used to look:
		   		// resolve right, then introduce bindings. don't leave it up to the ident rule.
		   		n.Rhs = r.Resolve(env, n.Rhs) // Does this correspond to my strategy?
		   		n.Destructure = r.defineDestructure(env, false, n.Destructure, n, func(id string) {
		   			// setIllegalsUsingWalk(env, id, n.Rhs)
		   			setIllegals(env, id, n.Rhs)
		   		})
		   		return n
		*/
		// Create a new scope for the rhs.
		// Define new type variables based on destructure.
	}
	panic(fmt.Sprintf("unimplemented: %T", n))
}

func (r *Resolver) Check(env *Env, node parser.Node, t Type) parser.Node {
	tnode := r.Infer(env, node)
	if tnode, ok := tnode.(TypedNode); ok {
		if !r.Unify(tnode.Type, t) {
			return parser.Illegal{Node: tnode.Node, Msg: fmt.Sprintf("expected %s, got %s", t, tnode.Type)}
		}
	}
	return tnode
}

func (r *Resolver) get(t Type) Type {
	if tvar, ok := t.(TypeVar); ok {
		if tvar.Ref.Bound {
			return r.get(tvar.Ref.Type)
		}
	}
	return t
}

func (r *Resolver) occurs(tvar TypeVar, t Type) bool {
	switch t := t.(type) {
	case TypeVar:
		// return tvar.Ref.ID == t.Ref.ID // TODO: is this correct with scoped type variables?
		return tvar.Ref == t.Ref // TODO: is this correct with scoped type variables?
	case Function:
		for _, elem := range t.Elements {
			if r.occurs(tvar, elem) {
				return true
			}
		}
		return false
	case Base:
		return false
	}
	panic(fmt.Sprintf("unimplemented: %T", t))
}

// Unify attempts to unify two types, and returns true if they are unifiable.
func (r *Resolver) Unify(t1, t2 Type) bool {
	t1, t2 = r.get(t1), r.get(t2)
	if SimpleEquals(t1, t2) {
		return true
	}
	if tvar1, ok := t1.(TypeVar); ok && !tvar1.Ref.Bound && !r.occurs(tvar1, t2) {
		tvar1.Ref.Bound = true
		tvar1.Ref.Type = t2
		return true
	}
	if tvar2, ok := t2.(TypeVar); ok && !tvar2.Ref.Bound && !r.occurs(tvar2, t1) {
		tvar2.Ref.Bound = true
		tvar2.Ref.Type = t1
		return true
	}
	if tarr1, ok := t1.(Function); ok {
		if tarr2, ok := t2.(Function); ok {
			if len(tarr1.Elements) != len(tarr2.Elements) {
				return false
			}
			for i := range tarr1.Elements {
				if !r.Unify(tarr1.Elements[i], tarr2.Elements[i]) {
					return false
				}
			}
			return true
		}
	}
	return false
}
