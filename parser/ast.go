package parser

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/smasher164/gflat/lexer"
)

type Node interface {
	LeadingTrivia() []lexer.Token
	Span() lexer.Span
	ASTString(depth int) string
}

var (
	_ Node = BinaryExpr{}
	_ Node = &PackageDecl{}
	_ Node = Stmt{}
	_ Node = File{}
	_ Node = Ident{}
	_ Node = Illegal{}
	_ Node = Block{}
	_ Node = EmptyExpr{}
	_ Node = TypeAnnotation{}
	_ Node = TupleParam{}
	_ Node = Function{}
	_ Node = TupleElement{}
	_ Node = Tuple{}
	_ Node = LetDecl{}
	_ Node = VarDecl{}
	_ Node = IfHeader{}
	_ Node = If{}
	_ Node = IfElse{}
	_ Node = TypeDecl{}
	_ Node = Number{}
	_ Node = NamedTypeParameter{}
	_ Node = NamedTypeArgument{}
	_ Node = NamedType{}
	_ Node = SumType{}
	_ Node = SumTypeElement{}
	_ Node = FunctionType{}
	_ Node = Field{}
	_ Node = PrefixExpr{}
	_ Node = CallExpr{}
	_ Node = PostfixExpr{}
	_ Node = SelectorExpr{}
	_ Node = PatternCase{}
	_ Node = IfMatch{}
	_ Node = StringPart{}
	_ Node = String{}
)

func spanOf(n any) lexer.Span {
	if n == nil {
		return lexer.Span{}
	}
	switch n := n.(type) {
	case Node:
		return n.Span()
	case []Node:
		if len(n) > 0 {
			return lexer.Span{
				Start: spanOf(n[0]).Start,
				End:   spanOf(n[len(n)-1]).End,
			}
		}
	}
	return lexer.Span{}
}

func leadingTriviaOf(n Node) []lexer.Token {
	if n == nil {
		return nil
	}
	return n.LeadingTrivia()
}

type BinaryExpr struct {
	Left  Node
	Op    lexer.Token
	Right Node
}

func (be BinaryExpr) ASTString(depth int) string {
	return fmt.Sprintf(
		"BinaryExpr\n%sLeft: %s\n%sOp: %s\n%sRight: %s",
		indent(depth+1),
		be.Left.ASTString(depth+1), indent(depth+1),
		be.Op, indent(depth+1),
		be.Right.ASTString(depth+1))
}

func (be BinaryExpr) Span() lexer.Span {
	return spanOf(be.Left).Add(spanOf(be.Right))
}

func (be BinaryExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(be.Left)
}

type PackageDecl struct {
	Package lexer.Token
	Name    Node
}

func (p *PackageDecl) ASTString(depth int) string {
	if p == nil {
		return "nil"
	}
	return fmt.Sprintf(
		"PackageDecl\n%sPackage: %s\n%sName: %s",
		indent(depth+1),
		p.Package, indent(depth+1),
		p.Name.ASTString(depth+1))
}

func (p *PackageDecl) Span() lexer.Span {
	return p.Package.Span.Add(spanOf(p.Name))
}

func (p *PackageDecl) LeadingTrivia() []lexer.Token {
	return p.Package.LeadingTrivia
}

type File struct {
	PackageDecl    *PackageDecl
	Body           Node
	trailingTrivia []lexer.Token
}

func indent(depth int) string {
	return fmt.Sprintf("%*s", depth*2, "")
}

type Stmt struct {
	Stmt      Node
	Semicolon lexer.Token
}

func (s Stmt) ASTString(depth int) string {
	return fmt.Sprintf(
		"Stmt\n%s%s\n%sSemicolon: %s",
		indent(depth+1),
		s.Stmt.ASTString(depth+1), indent(depth+1),
		s.Semicolon)
}

func (s Stmt) LeadingTrivia() []lexer.Token {
	if trivia := leadingTriviaOf(s.Stmt); trivia != nil {
		return trivia
	} else {
		return s.Semicolon.LeadingTrivia
	}
}

func (s Stmt) Span() lexer.Span {
	return spanOf(s.Stmt).Add(s.Semicolon.Span)
}

func (f File) ASTString(depth int) string {
	return fmt.Sprintf(
		"%sFile\n%sPackageDecl: %s\n%sBody: %s\n%sTrailingTrivia: %v",
		indent(depth), indent(depth+1),
		f.PackageDecl.ASTString(depth+1), indent(depth+1),
		f.Body.ASTString(depth+1), indent(depth+1),
		f.trailingTrivia)
}

func (f File) LeadingTrivia() []lexer.Token {
	if trivia := leadingTriviaOf(f.PackageDecl); trivia != nil {
		return trivia
	} else {
		return leadingTriviaOf(f.Body)
	}
}

func (f File) Span() lexer.Span {
	return spanOf(f.PackageDecl).Add(spanOf(f.Body))
}

func (f File) TrailingTrivia() []lexer.Token {
	return f.trailingTrivia
}

type Ident struct {
	Name lexer.Token
}

func (id Ident) ASTString(depth int) string {
	return id.Name.String()
}

func (id Ident) LeadingTrivia() []lexer.Token {
	return id.Name.LeadingTrivia
}

func (id Ident) Span() lexer.Span {
	return id.Name.Span
}

type Illegal struct {
	leadingTrivia []lexer.Token
	span          lexer.Span
	Node          Node
	Msg           string
}

func (i Illegal) ASTString(depth int) string {
	if i.Node != nil {
		return fmt.Sprintf(
			"Illegal\n%sleadingTrivia: %v\n%sspan: %s\n%sNode: %s\n%sMsg: %q",
			indent(depth+1),
			i.leadingTrivia, indent(depth+1),
			i.span, indent(depth+1),
			i.Node.ASTString(depth+1), indent(depth+1),
			i.Msg)
	}
	return fmt.Sprintf(
		"Illegal\n%sleadingTrivia: %v\n%sspan: %s\n%sMsg: %q",
		indent(depth+1),
		i.leadingTrivia, indent(depth+1),
		i.span, indent(depth+1),
		i.Msg)
}

func (i Illegal) LeadingTrivia() []lexer.Token {
	if len(i.leadingTrivia) > 0 {
		return i.leadingTrivia
	}
	return leadingTriviaOf(i.Node)
}

func (i Illegal) Span() lexer.Span {
	return i.span.Add(spanOf(i.Node))
}

type Block struct {
	LeftBrace  lexer.Token
	Body       []Node
	RightBrace lexer.Token
}

func printNodeSlice(depth int, nodes []Node) string {
	if len(nodes) == 0 {
		return "[]"
	}
	s := fmt.Sprintf("[\n%s", indent(depth+1))
	for _, n := range nodes {
		s += fmt.Sprintf("%s\n%s", n.ASTString(depth+1), indent(depth+1))
	}
	s += "]"
	return s
}

func (b Block) ASTString(depth int) string {
	return fmt.Sprintf(
		"Block\n%sLeftBrace: %s\n%sBody: %s\n%sRightBrace: %s",
		indent(depth+1),
		b.LeftBrace, indent(depth+1),
		printNodeSlice(depth+1, b.Body), indent(depth+1),
		b.RightBrace)
}

func (b Block) LeadingTrivia() []lexer.Token {
	if b.LeftBrace.Type == lexer.LeftBrace {
		return b.LeftBrace.LeadingTrivia
	}
	if len(b.Body) > 0 {
		return leadingTriviaOf(b.Body[0])
	}
	return b.RightBrace.LeadingTrivia
}

func (b Block) Span() lexer.Span {
	return b.LeftBrace.Span.Add(spanOf(b.Body)).Add(b.RightBrace.Span)
}

func PrintAST(root Node) {
	fmt.Println(root.ASTString(0))
}

type EmptyExpr struct{}

func (e EmptyExpr) ASTString(_ int) string {
	return "EmptyExpr"
}

func (e EmptyExpr) LeadingTrivia() []lexer.Token {
	return nil
}

func (e EmptyExpr) Span() lexer.Span {
	return lexer.Span{}
}

type TypeAnnotation struct {
	Destructure Node
	Colon       lexer.Token
	Type        Node
}

func (t TypeAnnotation) ASTString(depth int) string {
	return fmt.Sprintf(
		"TypeAnnotation\n%sDestructure: %s\n%sColon: %s\n%sType: %s",
		indent(depth+1),
		t.Destructure.ASTString(depth+1), indent(depth+1),
		t.Colon, indent(depth+1),
		t.Type.ASTString(depth+1))
}

func (t TypeAnnotation) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(t.Destructure)
}

func (t TypeAnnotation) Span() lexer.Span {
	return spanOf(t.Destructure).Add(t.Colon.Span).Add(spanOf(t.Type))
}

type TupleParam struct {
	Name  Ident
	Colon lexer.Token
	Type  Node
}

func (n TupleParam) LeadingTrivia() []lexer.Token {
	return n.Name.LeadingTrivia()
}

func (n TupleParam) Span() lexer.Span {
	return n.Name.Span().Add(n.Colon.Span).Add(spanOf(n.Type))
}

func (n TupleParam) ASTString(depth int) string {
	return fmt.Sprintf(
		"TupleParam\n%sName: %s\n%sColon: %s\n%sType: %s",
		indent(depth+1),
		n.Name.ASTString(depth+1), indent(depth+1),
		n.Colon, indent(depth+1),
		n.Type.ASTString(depth+1))
}

// type Function struct {
// 	Fun   lexer.Token
// 	Arg   Node
// 	Colon lexer.Token
// 	Type  Node
// 	Arrow lexer.Token
// 	Body  Node
// }

type FunctionSignature struct {
	Param  Node
	Arrows []Node
}

func (n FunctionSignature) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(n.Param)
}

func (n FunctionSignature) Span() lexer.Span {
	return spanOf(n.Param).Add(spanOf(n.Arrows))
}

func (n FunctionSignature) ASTString(depth int) string {
	return fmt.Sprintf(
		"FunctionSignature\n%sParam: %s\n%sArrows: %s",
		indent(depth+1),
		n.Param.ASTString(depth+1), indent(depth+1),
		printNodeSlice(depth+1, n.Arrows))
}

type Param struct {
	Name  Ident
	Colon lexer.Token
	Type  Node
}

func (n Param) LeadingTrivia() []lexer.Token {
	return n.Name.LeadingTrivia()
}

func (n Param) Span() lexer.Span {
	return n.Name.Span().Add(n.Colon.Span).Add(spanOf(n.Type))
}

func (n Param) ASTString(depth int) string {
	if n.Type == nil {
		return fmt.Sprintf(
			"Param\n%sName: %s",
			indent(depth+1),
			n.Name.ASTString(depth+1))
	}
	return fmt.Sprintf(
		"Param\n%sName: %s\n%sColon: %s\n%sType: %s",
		indent(depth+1),
		n.Name.ASTString(depth+1), indent(depth+1),
		n.Colon, indent(depth+1),
		n.Type.ASTString(depth+1))
}

type Arrow struct {
	Arrow lexer.Token
	Type  Node
}

func (n Arrow) LeadingTrivia() []lexer.Token {
	return n.Arrow.LeadingTrivia
}

func (n Arrow) Span() lexer.Span {
	return n.Arrow.Span.Add(spanOf(n.Type))
}

func (n Arrow) ASTString(depth int) string {
	return fmt.Sprintf(
		"Arrow\n%sArrow: %s\n%sType: %s",
		indent(depth+1),
		n.Arrow, indent(depth+1),
		n.Type.ASTString(depth+1))
}

type Function struct {
	Fun       lexer.Token
	Name      *Ident
	Signature FunctionSignature
	Equals    lexer.Token
	Body      Node
}

func (f Function) LeadingTrivia() []lexer.Token {
	return f.Fun.LeadingTrivia
}

func (f Function) Span() lexer.Span {
	return f.Fun.Span.Add(spanOf(f.Body))
}

func (f Function) ASTString(depth int) string {
	if f.Name != nil {
		return fmt.Sprintf(
			"Function\n%sFun: %s\n%sName: %s\n%sSignature: %s\n%sEquals: %s\n%sBody: %s",
			indent(depth+1), f.Fun, indent(depth+1), f.Name.ASTString(depth+1), indent(depth+1), f.Signature.ASTString(depth+1), indent(depth+1), f.Equals, indent(depth+1), f.Body.ASTString(depth+1))
	}
	return fmt.Sprintf(
		"Function\n%sFun: %s\n%sSignature: %s\n%sEquals: %s\n%sBody: %s",
		indent(depth+1), f.Fun, indent(depth+1), f.Signature.ASTString(depth+1),
		indent(depth+1), f.Equals, indent(depth+1), f.Body.ASTString(depth+1))
}

type TupleElement struct {
	X     Node
	Comma lexer.Token
}

func (t TupleElement) ASTString(depth int) string {
	return fmt.Sprintf(
		"TupleElement\n%sX: %s\n%sComma: %s",
		indent(depth+1),
		t.X.ASTString(depth+1), indent(depth+1),
		t.Comma)
}

func (t TupleElement) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(t.X)
}

func (t TupleElement) Span() lexer.Span {
	return spanOf(t.X).Add(t.Comma.Span)
}

type Tuple struct {
	LeftParen  lexer.Token
	Elements   []Node
	RightParen lexer.Token
}

func (t Tuple) LeadingTrivia() []lexer.Token {
	return t.LeftParen.LeadingTrivia
}

func (t Tuple) Span() lexer.Span {
	return t.LeftParen.Span.Add(t.RightParen.Span)
}

func (t Tuple) ASTString(depth int) string {
	return fmt.Sprintf(
		"Tuple\n%sLeftParen: %s\n%sElements: %s\n%sRightParen: %s",
		indent(depth+1),
		t.LeftParen, indent(depth+1),
		printNodeSlice(depth+1, t.Elements), indent(depth+1),
		t.RightParen)
}

type LetDecl struct {
	Let         lexer.Token
	Destructure Node
	Equals      lexer.Token
	Rhs         Node
}

func (l LetDecl) LeadingTrivia() []lexer.Token {
	return l.Let.LeadingTrivia
}

func (l LetDecl) Span() lexer.Span {
	return l.Let.Span.Add(spanOf(l.Rhs))
}

func (l LetDecl) ASTString(depth int) string {
	return fmt.Sprintf(
		"LetDecl\n%sLet: %s\n%sDestructure: %s\n%sEquals: %s\n%sRhs: %s",
		indent(depth+1),
		l.Let, indent(depth+1),
		l.Destructure.ASTString(depth+1), indent(depth+1),
		l.Equals, indent(depth+1),
		l.Rhs.ASTString(depth+1))
}

type VarDecl struct {
	Var         lexer.Token
	Destructure Node
	Equals      lexer.Token
	Rhs         Node
}

func (v VarDecl) LeadingTrivia() []lexer.Token {
	return v.Var.LeadingTrivia
}

func (v VarDecl) Span() lexer.Span {
	return v.Var.Span.Add(spanOf(v.Rhs))
}

func (v VarDecl) ASTString(depth int) string {
	return fmt.Sprintf(
		"VarDecl\n%sVar: %s\n%sDestructure: %s\n%sEquals: %s\n%sRhs: %s",
		indent(depth+1),
		v.Var, indent(depth+1),
		v.Destructure.ASTString(depth+1), indent(depth+1),
		v.Equals, indent(depth+1),
		v.Rhs.ASTString(depth+1))
}

type IfHeader struct {
	If   lexer.Token
	Cond Node
}

func (i IfHeader) LeadingTrivia() []lexer.Token {
	return i.If.LeadingTrivia
}

func (i IfHeader) Span() lexer.Span {
	return i.If.Span.Add(spanOf(i.Cond))
}

func (i IfHeader) ASTString(depth int) string {
	return fmt.Sprintf(
		"IfHeader\n%sIf: %s\n%sCond: %s",
		indent(depth+1),
		i.If, indent(depth+1),
		i.Cond.ASTString(depth+1))
}

type If struct {
	IfHeader IfHeader
	Body     Node
}

func (i If) LeadingTrivia() []lexer.Token {
	return i.IfHeader.LeadingTrivia()
}

func (i If) Span() lexer.Span {
	return i.IfHeader.Span().Add(spanOf(i.Body))
}

func (i If) ASTString(depth int) string {
	return fmt.Sprintf(
		"If\n%sIfHeader: %s\n%sBody: %s",
		indent(depth+1),
		i.IfHeader.ASTString(depth+1), indent(depth+1),
		i.Body.ASTString(depth+1))
}

type IfElse struct {
	IfHeader IfHeader
	Body     Node
	Else     lexer.Token
	ElseBody Node
}

func (i IfElse) LeadingTrivia() []lexer.Token {
	return i.IfHeader.LeadingTrivia()
}

func (i IfElse) Span() lexer.Span {
	return i.IfHeader.Span().Add(spanOf(i.Body)).Add(i.Else.Span).Add(spanOf(i.ElseBody))
}

func (i IfElse) ASTString(depth int) string {
	return fmt.Sprintf(
		"IfElse\n%sIfHeader: %s\n%sBody: %s\n%sElse: %s\n%sElseBody: %s",
		indent(depth+1),
		i.IfHeader.ASTString(depth+1), indent(depth+1),
		i.Body.ASTString(depth+1), indent(depth+1),
		i.Else, indent(depth+1),
		i.ElseBody.ASTString(depth+1))
}

type TypeDecl struct {
	Type       lexer.Token
	Name       Node
	TypeParams []Node
	Equal      lexer.Token
	Body       Node
}

func (t TypeDecl) LeadingTrivia() []lexer.Token {
	return t.Type.LeadingTrivia
}

func (t TypeDecl) Span() lexer.Span {
	return t.Type.Span.Add(spanOf(t.Body))
}

func (t TypeDecl) ASTString(depth int) string {
	return fmt.Sprintf(
		"TypeDecl\n%sType: %s\n%sName: %s\n%sTypeParams: %s\n%sEqual: %s\n%sBody: %s",
		indent(depth+1),
		t.Type, indent(depth+1),
		t.Name.ASTString(depth+1), indent(depth+1),
		printNodeSlice(depth+1, t.TypeParams), indent(depth+1),
		t.Equal, indent(depth+1),
		t.Body.ASTString(depth+1))
}

type Number struct {
	Lit lexer.Token
}

// IsNat reports whether the number literal is a composed of ascii digits only.
func (n Number) IsNat() bool {
	return strings.IndexFunc(n.Lit.Data, func(r rune) bool {
		return !unicode.IsDigit(r)
	}) == -1
}

func (n Number) LeadingTrivia() []lexer.Token {
	return n.Lit.LeadingTrivia
}

func (n Number) Span() lexer.Span {
	return n.Lit.Span
}

func (n Number) ASTString(depth int) string {
	return fmt.Sprintf("Number %s", n.Lit)
}

type NamedTypeParameter struct {
	SingleQuote lexer.Token
	Name        Node
}

func (n NamedTypeParameter) LeadingTrivia() []lexer.Token {
	return n.SingleQuote.LeadingTrivia
}

func (n NamedTypeParameter) Span() lexer.Span {
	return n.SingleQuote.Span.Add(spanOf(n.Name))
}

func (n NamedTypeParameter) ASTString(depth int) string {
	return fmt.Sprintf("NamedTypeParameter\n%sSingleQuote: %s\n%sName: %s", indent(depth+1), n.SingleQuote, indent(depth+1), n.Name.ASTString(depth+1))
}

type NamedTypeArgument struct {
	SingleQuote lexer.Token
	Name        Node
}

func (n NamedTypeArgument) LeadingTrivia() []lexer.Token {
	return n.SingleQuote.LeadingTrivia
}

func (n NamedTypeArgument) Span() lexer.Span {
	return n.SingleQuote.Span.Add(spanOf(n.Name))
}

func (n NamedTypeArgument) ASTString(depth int) string {
	return fmt.Sprintf("NamedTypeArgument\n%sSingleQuote: %s\n%sName: %s", indent(depth+1), n.SingleQuote, indent(depth+1), n.Name.ASTString(depth+1))
}

type NamedType struct {
	Name Node
	Args []Node
}

func (n NamedType) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(n.Name)
}

func (n NamedType) Span() lexer.Span {
	return spanOf(n.Name).Add(spanOf(n.Args))
}

func (n NamedType) ASTString(depth int) string {
	return fmt.Sprintf("NamedType\n%sName: %s\n%sArgs: %s", indent(depth+1), n.Name.ASTString(depth+1), indent(depth+1), printNodeSlice(depth+1, n.Args))
}

type SumType struct {
	Elements []Node
}

func (s SumType) LeadingTrivia() []lexer.Token {
	if len(s.Elements) == 0 {
		return nil
	}
	return leadingTriviaOf(s.Elements[0])
}

func (s SumType) Span() lexer.Span {
	return spanOf(s.Elements)
}

func (s SumType) ASTString(depth int) string {
	return fmt.Sprintf("SumType\n%sElements: %s", indent(depth+1), printNodeSlice(depth+1, s.Elements))
}

type SumTypeElement struct {
	Or   lexer.Token
	Name Node
	Type Node
}

func (s SumTypeElement) LeadingTrivia() []lexer.Token {
	return s.Or.LeadingTrivia
}

func (s SumTypeElement) Span() lexer.Span {
	return s.Or.Span.Add(spanOf(s.Name)).Add(spanOf(s.Type))
}

func (s SumTypeElement) ASTString(depth int) string {
	if s.Type == nil {
		return fmt.Sprintf("SumTypeElement\n%sOr: %s\n%sName: %s", indent(depth+1), s.Or, indent(depth+1), s.Name.ASTString(depth+1))
	}
	return fmt.Sprintf("SumTypeElement\n%sOr: %s\n%sName: %s\n%sType: %s", indent(depth+1), s.Or, indent(depth+1), s.Name.ASTString(depth+1), indent(depth+1), s.Type.ASTString(depth+1))
}

type ForallType struct {
	TypeArg Node
	Period  lexer.Token
	Type    Node
}

func (f ForallType) LeadingTrivia() []lexer.Token {
	return f.TypeArg.LeadingTrivia()
}

func (f ForallType) Span() lexer.Span {
	return spanOf(f.TypeArg).Add(spanOf(f.Period)).Add(spanOf(f.Type))
}

func (f ForallType) ASTString(depth int) string {
	return fmt.Sprintf("ForallType\n%sTypeArg: %s\n%sPeriod: %s\n%sType: %s", indent(depth+1), f.TypeArg.ASTString(depth+1), indent(depth+1), f.Period, indent(depth+1), f.Type.ASTString(depth+1))
}

type FunctionType struct {
	Fun    lexer.Token
	Param  Node
	Arrows []Node
}

func (f FunctionType) LeadingTrivia() []lexer.Token {
	return f.Fun.LeadingTrivia
}

func (f FunctionType) Span() lexer.Span {
	return f.Fun.Span.Add(spanOf(f.Param)).Add(spanOf(f.Arrows))
}

func (f FunctionType) ASTString(depth int) string {
	return fmt.Sprintf(
		"FunctionType\n%sFun: %s\n%sParam: %s\n%sArrows: %s",
		indent(depth+1), f.Fun, indent(depth+1), f.Param.ASTString(depth+1),
		indent(depth+1), printNodeSlice(depth+1, f.Arrows))
}

type Field struct {
	Name    Node
	Colon   lexer.Token
	Type    Node
	Equals  lexer.Token
	Default Node
}

func (f Field) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(f.Name)
}

func (f Field) Span() lexer.Span {
	return spanOf(f.Name).Add(spanOf(f.Type))
}

func (f Field) ASTString(depth int) string {
	return fmt.Sprintf("Field\n%sName: %s\n%sColon: %s\n%sType: %s", indent(depth+1), f.Name.ASTString(depth+1), indent(depth+1), f.Colon, indent(depth+1), f.Type.ASTString(depth+1))
}

type PrefixExpr struct {
	Op lexer.Token
	X  Node
}

func (p PrefixExpr) LeadingTrivia() []lexer.Token {
	return p.Op.LeadingTrivia
}

func (p PrefixExpr) Span() lexer.Span {
	return p.Op.Span.Add(spanOf(p.X))
}

func (p PrefixExpr) ASTString(depth int) string {
	return fmt.Sprintf("PrefixExpr\n%sOp: %s\n%sX: %s", indent(depth+1), p.Op, indent(depth+1), p.X.ASTString(depth+1))
}

type CallExpr struct {
	Elements []Node
}

func (c CallExpr) LeadingTrivia() []lexer.Token {
	if len(c.Elements) == 0 {
		return nil
	}
	return leadingTriviaOf(c.Elements[0])
}

func (c CallExpr) Span() lexer.Span {
	return spanOf(c.Elements)
}

func (c CallExpr) ASTString(depth int) string {
	return fmt.Sprintf("CallExpr\n%sElements: %s", indent(depth+1), printNodeSlice(depth+1, c.Elements))
}

type PostfixExpr struct {
	X  Node
	Op lexer.Token
}

func (p PostfixExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(p.X)
}

func (p PostfixExpr) Span() lexer.Span {
	return spanOf(p.X).Add(p.Op.Span)
}

func (p PostfixExpr) ASTString(depth int) string {
	return fmt.Sprintf("PostfixExpr\n%sX: %s\n%sOp: %s", indent(depth+1), p.X.ASTString(depth+1), indent(depth+1), p.Op)
}

type SelectorExpr struct {
	X      Node
	Period lexer.Token
	Name   Node
}

func (s SelectorExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(s.X)
}

func (s SelectorExpr) Span() lexer.Span {
	return spanOf(s.X).Add(spanOf(s.Name))
}

func (s SelectorExpr) ASTString(depth int) string {
	return fmt.Sprintf("SelectorExpr\n%sX: %s\n%sPeriod: %s\n%sName: %s", indent(depth+1), s.X.ASTString(depth+1), indent(depth+1), s.Period, indent(depth+1), s.Name.ASTString(depth+1))
}

type PatternCase struct {
	Or      lexer.Token
	Pattern Node
	Guard   Node
	Arrow   lexer.Token
	Expr    Node
	Comma   lexer.Token
}

func (p PatternCase) LeadingTrivia() []lexer.Token {
	return p.Or.LeadingTrivia
}

func (p PatternCase) Span() lexer.Span {
	if p.Comma.Type == lexer.Comma {
		return p.Or.Span.Add(p.Comma.Span)
	}
	return p.Or.Span.Add(spanOf(p.Expr))
}

func (p PatternCase) ASTString(depth int) string {
	if p.Guard == nil {
		return fmt.Sprintf(
			"PatternCase\n%sOr: %s\n%sPattern: %s\n%sArrow: %s\n%sExpr: %s\n%sComma: %s", indent(depth+1), p.Or, indent(depth+1),
			p.Pattern.ASTString(depth+1), indent(depth+1), p.Arrow, indent(depth+1),
			p.Expr.ASTString(depth+1), indent(depth+1), p.Comma)
	}
	return fmt.Sprintf("PatternCase\n%sOr: %s\n%sPattern: %s\n%sGuard: %s\n%sArrow: %s\n%sExpr: %s\n%sComma: %s", indent(depth+1), p.Or, indent(depth+1), p.Pattern.ASTString(depth+1), indent(depth+1), p.Guard.ASTString(depth+1), indent(depth+1), p.Arrow, indent(depth+1), p.Expr.ASTString(depth+1), indent(depth+1), p.Comma)
}

type IfMatch struct {
	IfHeader Node
	Cases    []Node
}

func (i IfMatch) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(i.IfHeader)
}

func (i IfMatch) Span() lexer.Span {
	return spanOf(i.IfHeader).Add(spanOf(i.Cases))
}

func (i IfMatch) ASTString(depth int) string {
	return fmt.Sprintf(
		"IfMatch\n%sIfHeader: %s\n%sCases: %s", indent(depth+1), i.IfHeader.ASTString(depth+1), indent(depth+1),
		printNodeSlice(depth+1, i.Cases))
}

type StringPart struct {
	Lit lexer.Token
}

func (s StringPart) LeadingTrivia() []lexer.Token {
	return s.Lit.LeadingTrivia
}

func (s StringPart) Span() lexer.Span {
	return s.Lit.Span
}

func (s StringPart) ASTString(depth int) string {
	return fmt.Sprintf("StringPart %s", s.Lit)
}

type String struct {
	Parts []Node
}

func (s String) LeadingTrivia() []lexer.Token {
	if len(s.Parts) == 0 {
		return nil
	}
	return leadingTriviaOf(s.Parts[0])
}

func (s String) Span() lexer.Span {
	return spanOf(s.Parts)
}

func (s String) ASTString(depth int) string {
	return fmt.Sprintf("String\n%sParts: %s", indent(depth+1), printNodeSlice(depth+1, s.Parts))
}

/*
let (x, y) : T



AnnotatedDestructure{
	Destructure: Tuple{
		LeftParen: "(",
		Elements: []TupleElement{
			TupleElement{
				X: Ident{Name: "x"},
				Comma: ",",
			},
			TupleElement{
				X: Ident{Name: "y"},
			},

		},
		RightParen: ")",
	},
	Colon: ":",
	Type: Ident{Name: "T"},
}
*/
