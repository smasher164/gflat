package names

import (
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
)

// Var is a Node that points into a Scope object.
type Var struct {
	OriginalIdent parser.Node
	// Reference to environment here.
}

func (v Var) ASTString(depth int) string {
	return v.OriginalIdent.ASTString(depth) // do we want to print the scope?
}

func (v Var) LeadingTrivia() []lexer.Token {
	return v.OriginalIdent.LeadingTrivia()
}

func (v Var) Span() lexer.Span {
	return v.OriginalIdent.Span()
}

var _ parser.Node = Var{}

type Env struct {
	parent *Env
}

func (e *Env) AddScope() *Env {
	return &Env{parent: e}
}

var Universe = &Env{
	parent: nil,
}

// Replace ident with var that points to table?
// That means we need a var node.
// We'd like it to be a pointer to a Scope.
func Resolve(n parser.Node) parser.Node {
	return resolve(Universe.AddScope(), n)
}

func resolve(env *Env, n parser.Node) parser.Node {
	switch n := n.(type) {
	case parser.BinaryExpr:
	case parser.Stmt:
	case parser.File:
		n.Body = resolve(env.addScope(), n.Body)
		return n
	case parser.Ident:
	case parser.Illegal:
	case parser.Block:
	case parser.EmptyExpr:
	case parser.TypeAnnotation:
	case parser.TupleParam:
	case parser.FunctionSignature:
	case parser.Param:
	case parser.Arrow:
	case parser.LetFunction:
	case parser.Function:
	case parser.TupleElement:
	case parser.Tuple:
	case parser.LetDecl:
	case parser.VarDecl:
	case parser.IfHeader:
	case parser.If:
	case parser.IfElse:
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
	case parser.PostfixExpr:
	case parser.SelectorExpr:
	case parser.PatternCase:
	case parser.IfMatch:
	case parser.StringPart:
	case parser.String:
	case parser.IndexExpr:
	case parser.ImportDecl:
	case parser.ImportDeclPackage:
	case parser.Where:
	case parser.ImplDecl:
	case parser.ArrayType:
	}
	return n
}
