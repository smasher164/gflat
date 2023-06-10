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
