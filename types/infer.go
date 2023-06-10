package types

import (
	"fmt"

	"github.com/smasher164/gflat/lexer"
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
func (r *Resolver) Infer(n parser.Node) parser.Node {
	switch n := n.(type) {
	case ResolvedPackage:
		pkg := n.OriginalPackage.(parser.Package)
		// skip PackageFiles for now
		for i := range pkg.ScriptFiles {
			pkg.ScriptFiles[i] = r.Infer(pkg.ScriptFiles[i])
		}
		n.OriginalPackage = pkg
		return n
	case parser.File:
		n.Body = r.Infer(n.Body)
		return n
	case parser.Block:
		var lastType Type = Unit
		for i := range n.Body {
			n.Body[i] = r.Infer(n.Body[i])
			if tbody, ok := n.Body[i].(TypedNode); ok {
				lastType = tbody.Type
			}
		}
		return TypedNode{Node: n, Type: lastType}
	case parser.BinaryExpr:
		// just for demo purposes. this should be using traits.
		// based on the actual op
		switch n.Op.Type {
		case lexer.DotDot:
		case lexer.Plus:
			n.Left = r.Infer(n.Left)
			if tleft, ok := n.Left.(TypedNode); ok {
				n.Right = r.Check(n.Right, tleft.Type)
				// check if right is typed as a number or string
				if tright, ok := n.Right.(TypedNode); ok {
					if tright.Type.Equal(Int) || tright.Type.Equal(String) {
						return TypedNode{Node: n, Type: tright.Type}
					}
					panic("TODO: invalid types being added. when traits are added, this will be fixed.")
				}
			}
			return n
		case lexer.Minus:
		case lexer.Times:
		case lexer.Divide:
		case lexer.Remainder:
		case lexer.LeftShift:
		case lexer.RightShift:
		case lexer.And, lexer.Caret, lexer.Exponentiation:
			n.Left = r.Check(n.Left, Int)
			n.Right = r.Check(n.Right, Int)
			if IsTyped(n.Left) && IsTyped(n.Right) {
				return TypedNode{Node: n, Type: n.Left.(TypedNode).Type}
			}
			return n
		case lexer.Or:
		case lexer.LogicalAnd, lexer.LogicalOr:
			n.Left = r.Check(n.Left, Bool)
			n.Right = r.Check(n.Right, Bool)
			if IsTyped(n.Left) && IsTyped(n.Right) {
				return TypedNode{Node: n, Type: n.Left.(TypedNode).Type}
			}
			return n
		case lexer.Equals:
			// this is for assignment
			// TODO: ensure that let bindings are immutable and you can't assign
			n.Left = r.Infer(n.Left)
			if tleft, ok := n.Left.(TypedNode); ok {
				n.Right = r.Check(n.Right, tleft.Type)
				if IsTyped(n.Right) {
					return TypedNode{Node: n, Type: Unit}
				}
			}
			return n
		case lexer.LogicalEquals, lexer.NotEquals:
			n.Left = r.Infer(n.Left)
			if tleft, ok := n.Left.(TypedNode); ok {
				n.Right = r.Check(n.Right, tleft.Type)
				if IsTyped(n.Right) {
					return TypedNode{Node: n, Type: Bool}
				}
			}
			return n
		case lexer.LessThan, lexer.LessThanEquals, lexer.GreaterThan, lexer.GreaterThanEquals:
			// both sides must be numbers
			n.Left = r.Check(n.Left, Int)
			n.Right = r.Check(n.Right, Int)
			if IsTyped(n.Left) && IsTyped(n.Right) {
				return TypedNode{Node: n, Type: Bool}
			}
			return n
		case lexer.LeftArrow:
		case lexer.Colon:
		}

		// tRight := r.Infer(env, n.Right)
		// switch n.Op {
		// case DotDot, Plus, Minus, Times, Divide, Remainder, LeftShift, RightShift, And, Or, Caret, LogicalAnd, LogicalOr, LogicalEquals, NotEquals, Equals, LessThan, LessThanEquals, GreaterThan, GreaterThanEquals, LeftArrow, Exponentiation, Colon:
		// }
	case parser.Number:
		// TODO: Number trait
		return TypedNode{Node: n, Type: Int}
	case parser.Stmt:
		n.Stmt = r.Infer(n.Stmt)
		return TypedNode{Node: n, Type: Unit}
	case parser.LetDecl:
		n.Rhs = r.Infer(n.Rhs)
		rtyp, ok := n.Rhs.(TypedNode)
		if !ok {
			return n
		}
		switch des := n.Destructure.(type) {
		case Var:
			id := des.OriginalIdent.(parser.Ident).Name.Data
			n.Destructure = TypedNode{Node: des, Type: rtyp.Type}
			des.Env.SetType(id, rtyp.Type)
		}
		return n
	case Var:
		id := n.OriginalIdent.(parser.Ident).Name.Data
		def, _ := n.Env.LookupLocal(id)
		if def.Type == nil {
			return n
		}
		return TypedNode{Node: n, Type: def.Type}
		// ignore tuples for now
		// no let-gen or inferred function signatures for now
		// get type annotation
		// rhsEnv := env.AddScope()
		// switch des := n.Destructure.(type) {
		// case parser.Ident:
		// 	id := des.Name.Data
		// 	n.Rhs = r.Infer(rhsEnv, n.Rhs)
		// 	if _, ok := env.LookupLocal(id); ok {
		// 		n.Destructure = parser.Illegal{Node: des, Msg: fmt.Sprintf("redefinition of %s", id)}
		// 	} else {
		// 		if trhs, ok := n.Rhs.(TypedNode); ok {
		// 			n.Destructure = TypedNode{Node: Var{OriginalIdent: des, Env: env}, Type: trhs.Type}
		// 		} else {
		// 			n.Destructure = parser.Illegal{Node: des, Msg: "rhs does not have a valid type"}
		// 		}
		// 		if id != "_" {
		// 			env.AddSymbol(id, NewDefinition(n.Destructure, des, NotForward))
		// 		}
		// 	}
		// }
		// return n
		// case parser.Ident:
		// 	def, ok := env.LookupStack(n.Name.Data)
		// 	if ok {
		// 		return def.Def
		// 	}
		// 	return UnresolvedIdent{OriginalIdent: n, Env: env}
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

func (r *Resolver) Check(node parser.Node, t Type) parser.Node {
	tnode := r.Infer(node)
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
