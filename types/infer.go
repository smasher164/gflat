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
			// need traits
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
		case lexer.Minus, lexer.Times, lexer.LeftShift, lexer.RightShift, lexer.Remainder, lexer.Divide, lexer.Or, lexer.And, lexer.Caret, lexer.Exponentiation:
			n.Left = r.Check(n.Left, Int)
			n.Right = r.Check(n.Right, Int)
			if IsTyped(n.Left) && IsTyped(n.Right) {
				return TypedNode{Node: n, Type: n.Left.(TypedNode).Type}
			}
			return n
		case lexer.Pipe:
		case lexer.QuestionPipe:
		// need traits, but for now, just check that the rhs is a function
		case lexer.LogicalAnd, lexer.LogicalOr:
			n.Left = r.Check(n.Left, Bool)
			n.Right = r.Check(n.Right, Bool)
			if IsTyped(n.Left) && IsTyped(n.Right) {
				return TypedNode{Node: n, Type: n.Left.(TypedNode).Type}
			}
			return n
		case lexer.Assign:
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
			// ascription

		}
	case parser.Number:
		// TODO: Number trait
		return TypedNode{Node: n, Type: Int}
	case parser.Stmt:
		n.Stmt = r.Infer(n.Stmt)
		return TypedNode{Node: n, Type: Unit}
	case parser.LetDecl:
		n.Rhs = r.Infer(n.Rhs)
		typedRHS, ok := n.Rhs.(TypedNode)
		if !ok {
			return n
		}
		switch des := n.Destructure.(type) {
		case Var:
			id := des.OriginalIdent.(parser.Ident).Name.Data
			n.Destructure = TypedNode{Node: des, Type: typedRHS.Type}
			des.Env.SetType(id, typedRHS.Type)
		case parser.TypeAnnotation:
			// build a type from the type annotation
			typ := r.TypeFromSyntax(des.Type)
			if typedRHS, ok := r.Check(typedRHS, typ).(TypedNode); ok {
				if v, ok := des.Destructure.(Var); ok {
					// only handle vars for now
					id := v.OriginalIdent.(parser.Ident).Name.Data
					n.Destructure = TypedNode{Node: v, Type: typedRHS.Type}
					v.Env.SetType(id, typedRHS.Type)
				}
			}
		}
		return n
	case Var:
		id := n.OriginalIdent.(parser.Ident).Name.Data
		def, _ := n.Env.LookupLocal(id)
		if def.Type == nil {
			return n
		}
		return TypedNode{Node: n, Type: def.Type}
	case parser.BasicString:
		return TypedNode{Node: n, Type: String}
	case parser.Tuple:
		var tuple Tuple
		for _, e := range n.Elements {
			var field Field
			if elem, ok := e.(parser.CommaElement); ok {
				if binExp, ok := elem.X.(parser.BinaryExpr); ok && binExp.Op.Type == lexer.Assign {
					if id, ok := binExp.Left.(Var); ok {
						field.Name = getID(id)
						inferredField := r.Infer(binExp.Right)
						if fieldType, ok := inferredField.(TypedNode); ok {
							field.Type = fieldType.Type
							field.Env = id.Env
							sym := field.Env.symbols[field.Name]
							sym.Type = field.Type
							field.Env.symbols[field.Name] = sym
							tuple.Fields = append(tuple.Fields, field)
							continue
						}
					}
				}
				if it, ok := r.Infer(elem.X).(TypedNode); ok {
					field.Type = it.Type
					tuple.Fields = append(tuple.Fields, field)
				} else {
					field.Type = InvalidType{Msg: "invalid type in field"}
					tuple.Fields = append(tuple.Fields, field)
				}
			}
		}
		return TypedNode{Node: n, Type: tuple}
	case parser.Function:
	case parser.LetFunction:
		// no let-gen or inferred function signatures for now
		// get type annotation
	}
	panic(fmt.Sprintf("unimplemented: %T", n))
}

func (r *Resolver) TypeFromSyntax(node parser.Node) Type {
	switch node := node.(type) {
	case TypeName:
		id := getID(node)
		// look up type name in env?
		if def, ok := node.Env.LookupStack(id); ok {
			return def.Type
		}
		return InvalidType{Msg: fmt.Sprintf("unknown type %s", id)}
		// return NodedType{OriginalNode: node, Type: }
	}
	panic("unimplemented")
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
	if t1, ok := t1.(Array); ok {
		if t2, ok := t2.(Array); ok {
			if t1.Length == t2.Length {
				return r.Unify(t1.Element, t2.Element)
			}
			return false
		}
	}
	if t1, ok := t1.(Slice); ok {
		if t2, ok := t2.(Slice); ok {
			return r.Unify(t1.Element, t2.Element)
		}
	}
	if tapp1, ok := t1.(TypeApplication); ok {
		if tapp2, ok := t2.(TypeApplication); ok {
			if len(tapp1.Elements) != len(tapp2.Elements) {
				return false
			}
			for i := range tapp1.Elements {
				if !r.Unify(tapp1.Elements[i], tapp2.Elements[i]) {
					return false
				}
			}
			return true
		}
	}
	/*
		// TODO: support type schemes, like type applications.
		Sum
		Tuple
		Map
		Forall
		TypeApplication
	*/
	return false
}
