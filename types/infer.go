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

// Infer accepts an untyped AST and returns a typed AST.
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
		return TypedNode{Node: n, Type: Int}
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

func (r *Resolver) force(t Type) Type {
	panic("TODO force")
}

// Unify attempts to unify two types, and returns true if they are unifiable.
func (r *Resolver) Unify(t1, t2 Type) bool {
	// t1, t2 := r.force(t1), r.force(t2)
	switch {
	case t1.Equal(t2):
		return true
		// case IsTypeVar(t1)
	}
	return false
}
