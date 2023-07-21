package types

import (
	"fmt"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
)

var (
	_ parser.Node = Var{}
	// _ parser.Node = FieldName{}
	_ parser.Node = TypeName{}
	_ parser.Node = UnresolvedIdent{}
	_ parser.Node = PackageName{}
	_ parser.Node = ResolvedPackage{}
	_ parser.Node = Cons{}
	_ parser.Node = ResolvedTypeArg{}
	_ parser.Node = UnresolvedTypeArg{}
	_ parser.Node = TypedNode{}
	// _ parser.Node = ResolvedNode{}
)

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

/*
type SomeNode interface {
	parser.Node
	Type() Type
	Env() *Env
	Underlying() parser.Node
}

type ResolvedNode interface {
	parser.Node
	Type() Type
	Env() *Env
	Underlying() parser.Node
}
*/

type ResolvedNode interface {
	parser.Node
	Env() *Env
}

// Var is a variable Node that points into a Scope object.
type Var struct {
	parser.Ident
}

func (v Var) ASTString(depth int) string {
	return fmt.Sprintf("Var: %s", v.Ident.ASTString(depth))
}

// type FieldName struct {
// 	OriginalIdent parser.Node
// 	// Reference to environment here.
// 	Env *Env
// }

// func (v FieldName) ASTString(depth int) string {
// 	return fmt.Sprintf("FieldName: %s", v.OriginalIdent.ASTString(depth))
// }

// func (v FieldName) LeadingTrivia() []lexer.Token {
// 	return v.OriginalIdent.LeadingTrivia()
// }

// func (v FieldName) Span() lexer.Span {
// 	return v.OriginalIdent.Span()
// }

type TypeName struct {
	parser.Ident
}

func (v TypeName) ASTString(depth int) string {
	return fmt.Sprintf("TypeName: %s", v.Ident.ASTString(depth))
}

// UnresolvedIdent is an identifier that has not been resolved yet.
type UnresolvedIdent struct {
	parser.Ident
}

func (v UnresolvedIdent) ASTString(depth int) string {
	return fmt.Sprintf("UnresolvedIdent: %s", v.Ident.ASTString(depth))
}

type PackageName struct {
	parser.Ident
}

func (v PackageName) ASTString(depth int) string {
	return fmt.Sprintf("PackageName: %s", v.Ident.ASTString(depth))
}

type ResolvedPackage struct {
	OriginalPackage parser.Node

	Path string
	// Reference to environment here.
	Env *Env
}

func (v ResolvedPackage) ASTString(depth int) string {
	return fmt.Sprintf("ResolvedPackage: %s", v.OriginalPackage.ASTString(depth))
}

func (v ResolvedPackage) LeadingTrivia() []lexer.Token {
	return v.OriginalPackage.LeadingTrivia()
}

func (v ResolvedPackage) Span() lexer.Span {
	return v.OriginalPackage.Span()
}

// Cons is a constructor Node that points into a Scope object.
type Cons struct {
	parser.Ident
}

func (c Cons) ASTString(depth int) string {
	return fmt.Sprintf("Cons: %s", c.Ident.ASTString(depth))
}

type ResolvedTypeArg struct {
	OriginalTypeVar parser.Node
	// Reference to environment here.
	Env *Env
}

func (v ResolvedTypeArg) ASTString(depth int) string {
	return fmt.Sprintf("TypeVar: %s", v.OriginalTypeVar.ASTString(depth))
}

func (v ResolvedTypeArg) LeadingTrivia() []lexer.Token {
	return v.OriginalTypeVar.LeadingTrivia()
}

func (v ResolvedTypeArg) Span() lexer.Span {
	return v.OriginalTypeVar.Span()
}

type UnresolvedTypeArg struct {
	OriginalTypeVar parser.Node
	// Reference to environment here.
	Env *Env
}

func (v UnresolvedTypeArg) ASTString(depth int) string {
	return fmt.Sprintf("UnresolvedTypeVar: %s", v.OriginalTypeVar.ASTString(depth))
}

func (v UnresolvedTypeArg) LeadingTrivia() []lexer.Token {
	return v.OriginalTypeVar.LeadingTrivia()
}

func (v UnresolvedTypeArg) Span() lexer.Span {
	return v.OriginalTypeVar.Span()
}

// func getID(n parser.Node) string {
// 	switch n := n.(type) {
// 	case parser.Ident:
// 		return n.Name.Data
// 	case Var:
// 		return getID(n.OriginalIdent)
// 	case TypeName:
// 		return getID(n.OriginalIdent)
// 	case Cons:
// 		return getID(n.OriginalIdent)
// 	case PackageName:
// 		return getID(n.OriginalIdent)
// 	case UnresolvedIdent:
// 		return getID(n.OriginalIdent)
// 	}
// 	panic("unreachable")
// }
