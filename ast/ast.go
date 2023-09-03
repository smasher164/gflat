package ast

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
	_ Node = (*BinaryExpr)(nil)
	_ Node = (*Stmt)(nil)
	_ Node = (*File)(nil)
	_ Node = (*Ident)(nil)
	_ Node = (*Illegal)(nil)
	_ Node = (*Block)(nil)
	_ Node = (*EmptyExpr)(nil)
	_ Node = (*TypeAnnotation)(nil)
	_ Node = (*FunctionSignature)(nil)
	_ Node = (*Arrow)(nil)
	_ Node = (*LetFunction)(nil)
	_ Node = (*Function)(nil)
	_ Node = (*CommaElement)(nil)
	_ Node = (*Tuple)(nil)
	_ Node = (*LetDecl)(nil)
	_ Node = (*VarDecl)(nil)
	_ Node = (*IfHeader)(nil)
	_ Node = (*If)(nil)
	_ Node = (*IfElse)(nil)
	_ Node = (*TypeDecl)(nil)
	_ Node = (*Number)(nil)
	_ Node = (*Array)(nil)
	_ Node = (*TypeArg)(nil)
	_ Node = (*SumType)(nil)
	_ Node = (*SumTypeElement)(nil)
	_ Node = (*ForallType)(nil)
	_ Node = (*FunctionType)(nil)
	_ Node = (*Field)(nil)
	_ Node = (*PrefixExpr)(nil)
	_ Node = (*CallExpr)(nil)
	_ Node = (*PostfixExpr)(nil)
	_ Node = (*SelectorExpr)(nil)
	_ Node = (*PatternCase)(nil)
	_ Node = (*IfMatch)(nil)
	_ Node = (*BasicString)(nil)
	_ Node = (*InterpolatedString)(nil)
	_ Node = (*IndexExpr)(nil)
	_ Node = (*ImportDecl)(nil)
	_ Node = (*As)(nil)
	// _ Node = (*ImportDeclPackage)(nil)
	_ Node = (*ImplDecl)(nil)
	_ Node = (*ArrayType)(nil)
	_ Node = (*NillableType)(nil)
	_ Node = (*Package)(nil)
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

func (be *BinaryExpr) ASTString(depth int) string {
	return fmt.Sprintf(
		"BinaryExpr\n%sLeft: %s\n%sOp: %s\n%sRight: %s",
		indent(depth+1),
		be.Left.ASTString(depth+1), indent(depth+1),
		be.Op, indent(depth+1),
		be.Right.ASTString(depth+1))
}

func (be *BinaryExpr) Span() lexer.Span {
	return spanOf(be.Left).Add(spanOf(be.Right))
}

func (be *BinaryExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(be.Left)
}

type File struct {
	Filename       string
	Package        lexer.Token
	PackageName    *Ident
	Body           *Block
	trailingTrivia []lexer.Token
	Imports        map[string]struct{}
	Env            *Env
}

func (f *File) SetTrailingTrivia(tt []lexer.Token) {
	f.trailingTrivia = tt
}

func (f *File) ASTString(depth int) string {
	if f.Package.Type == lexer.Package {
		return fmt.Sprintf(
			"File\n%sFilename: %s\n%sPackage: %s\n%sPackageName: %s\n%sBody: %s\n%sTrailingTrivia: %v\n%sImports: %v\n%sEnv:\n%v",
			indent(depth+1), f.Filename,
			indent(depth+1),
			f.Package, indent(depth+1),
			f.PackageName.ASTString(depth+1), indent(depth+1),
			f.Body.ASTString(depth+1), indent(depth+1),
			f.trailingTrivia, indent(depth+1),
			f.Imports, indent(depth+1), indentLines(depth+2, f.Env.String()))
	}
	return fmt.Sprintf(
		"File\n%sFilename: %s\n%sBody: %s\n%sTrailingTrivia: %v\n%sImports: %v",
		indent(depth+1), f.Filename,
		indent(depth+1),
		f.Body.ASTString(depth+1), indent(depth+1),
		f.trailingTrivia, indent(depth+1),
		f.Imports)
}

func (f *File) LeadingTrivia() []lexer.Token {
	if f.Package.Type == lexer.Package {
		return f.Package.LeadingTrivia
	}
	return leadingTriviaOf(f.Body)
}

func (f *File) Span() lexer.Span {
	return spanOf(f.Package).Add(spanOf(f.Body))
}

func (f *File) TrailingTrivia() []lexer.Token {
	return f.trailingTrivia
}

func indent(depth int) string {
	return fmt.Sprintf("%*s", depth*2, "")
}

func indentLines(depth int, input string) string {
	s := indent(depth) + strings.ReplaceAll(input, "\n", "\n"+indent(depth))
	// fmt.Println(s)
	return s
}

type Stmt struct {
	Stmt       Node
	Terminator lexer.Token
}

func (s *Stmt) ASTString(depth int) string {
	return fmt.Sprintf(
		"Stmt\n%s%s\n%sTerminator: %s",
		indent(depth+1),
		s.Stmt.ASTString(depth+1), indent(depth+1),
		s.Terminator)
}

func (s *Stmt) LeadingTrivia() []lexer.Token {
	if trivia := leadingTriviaOf(s.Stmt); trivia != nil {
		return trivia
	} else {
		return s.Terminator.LeadingTrivia
	}
}

func (s *Stmt) Span() lexer.Span {
	return spanOf(s.Stmt).Add(s.Terminator.Span)
}

type Package struct {
	Name         string
	PackageFiles []*File
	ScriptFile   *File
	Imports      map[string]struct{}
	Env          *Env
}

func (p *Package) ASTString(depth int) string {
	if p.ScriptFile == nil {
		return fmt.Sprintf("Package\n%sName: %s\n%sPackageFiles: %s\n%sImports: %v\n%sEnv:\n%v",
			indent(depth+1),
			p.Name, indent(depth+1),
			printFileSlice(depth+1, p.PackageFiles), indent(depth+1),
			p.Imports, indent(depth+1), indentLines(depth+2, p.Env.String()))
	}
	return fmt.Sprintf(
		"Package\n%sName: %s\n%sPackageFiles: %s\n%sScriptFile: %s\n%sImports: %v",
		indent(depth+1),
		p.Name, indent(depth+1),
		printFileSlice(depth+1, p.PackageFiles), indent(depth+1),
		p.ScriptFile.ASTString(depth+1), indent(depth+1),
		p.Imports)
}

func (p *Package) LeadingTrivia() []lexer.Token {
	return nil
}

func (p *Package) Span() lexer.Span {
	return lexer.Span{}
}

type Ident struct {
	Name lexer.Token
}

func (id *Ident) ASTString(depth int) string {
	return id.Name.String()
}

func (id *Ident) LeadingTrivia() []lexer.Token {
	return id.Name.LeadingTrivia
}

func (id *Ident) Span() lexer.Span {
	return id.Name.Span
}

type Illegal struct {
	leadingTrivia []lexer.Token
	span          lexer.Span
	Node          Node
	Msg           string
}

func (ill *Illegal) SetLeadingTrivia(tt []lexer.Token) {
	ill.leadingTrivia = tt
}

func (ill *Illegal) SetSpan(span lexer.Span) {
	ill.span = span
}

func (i *Illegal) ASTString(depth int) string {
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

func (i *Illegal) LeadingTrivia() []lexer.Token {
	if len(i.leadingTrivia) > 0 {
		return i.leadingTrivia
	}
	return leadingTriviaOf(i.Node)
}

func (i *Illegal) Span() lexer.Span {
	return i.span.Add(spanOf(i.Node))
}

type Block struct {
	LeftBrace  lexer.Token
	Body       []Node
	RightBrace lexer.Token
}

func printCaseSlice(depth int, nodes []*PatternCase) string {
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

func printFileSlice(depth int, nodes []*File) string {
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

func (b *Block) ASTString(depth int) string {
	return fmt.Sprintf(
		"Block\n%sLeftBrace: %s\n%sBody: %s\n%sRightBrace: %s",
		indent(depth+1),
		b.LeftBrace, indent(depth+1),
		printNodeSlice(depth+1, b.Body), indent(depth+1),
		b.RightBrace)
}

func (b *Block) LeadingTrivia() []lexer.Token {
	if b.LeftBrace.Type == lexer.LeftBrace {
		return b.LeftBrace.LeadingTrivia
	}
	if len(b.Body) > 0 {
		return leadingTriviaOf(b.Body[0])
	}
	return b.RightBrace.LeadingTrivia
}

func (b *Block) Span() lexer.Span {
	return b.LeftBrace.Span.Add(spanOf(b.Body)).Add(b.RightBrace.Span)
}

func PrintAST(root Node) {
	fmt.Println(root.ASTString(0))
}

type EmptyExpr struct{}

func (e *EmptyExpr) ASTString(_ int) string {
	return "EmptyExpr"
}

func (e *EmptyExpr) LeadingTrivia() []lexer.Token {
	return nil
}

func (e *EmptyExpr) Span() lexer.Span {
	return lexer.Span{}
}

type TypeAnnotation struct {
	Destructure Node
	Colon       lexer.Token
	Type        Node
}

func (t *TypeAnnotation) ASTString(depth int) string {
	if t.Type == nil {
		return fmt.Sprintf(
			"TypeAnnotation\n%sDestructure: %s\n%sColon: %s",
			indent(depth+1),
			t.Destructure.ASTString(depth+1), indent(depth+1),
			t.Colon)
	}
	return fmt.Sprintf(
		"TypeAnnotation\n%sDestructure: %s\n%sColon: %s\n%sType: %s",
		indent(depth+1),
		t.Destructure.ASTString(depth+1), indent(depth+1),
		t.Colon, indent(depth+1),
		t.Type.ASTString(depth+1))
}

func (t *TypeAnnotation) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(t.Destructure)
}

func (t *TypeAnnotation) Span() lexer.Span {
	return spanOf(t.Destructure).Add(t.Colon.Span).Add(spanOf(t.Type))
}

type FunctionSignature struct {
	Param  Node
	Arrows []Node
	With   lexer.Token
	Clause Node
}

func (n *FunctionSignature) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(n.Param)
}

func (n *FunctionSignature) Span() lexer.Span {
	return spanOf(n.Param).Add(spanOf(n.Arrows)).Add(n.With.Span).Add(spanOf(n.Clause))
}

func (n *FunctionSignature) ASTString(depth int) string {
	if n.With.Type == lexer.With {
		return fmt.Sprintf(
			"FunctionSignature\n%sParam: %s\n%sArrows: %s\n%sWith: %s\n%sClause: %s",
			indent(depth+1), n.Param.ASTString(depth+1), indent(depth+1),
			printNodeSlice(depth+1, n.Arrows), indent(depth+1),
			n.With, indent(depth+1), n.Clause.ASTString(depth+1))
	}
	return fmt.Sprintf(
		"FunctionSignature\n%sParam: %s\n%sArrows: %s",
		indent(depth+1),
		n.Param.ASTString(depth+1), indent(depth+1),
		printNodeSlice(depth+1, n.Arrows))
}

type Arrow struct {
	Arrow lexer.Token
	Type  Node
}

func (n *Arrow) LeadingTrivia() []lexer.Token {
	return n.Arrow.LeadingTrivia
}

func (n *Arrow) Span() lexer.Span {
	return n.Arrow.Span.Add(spanOf(n.Type))
}

func (n *Arrow) ASTString(depth int) string {
	return fmt.Sprintf(
		"Arrow\n%sArrow: %s\n%sType: %s",
		indent(depth+1),
		n.Arrow, indent(depth+1),
		n.Type.ASTString(depth+1))
}

type LetFunction struct {
	Let        lexer.Token
	TypeParams []Node
	Name       *Ident
	Signature  *FunctionSignature
	Equals     lexer.Token
	Body       Node
}

func (f *LetFunction) LeadingTrivia() []lexer.Token {
	return f.Let.LeadingTrivia
}

func (f *LetFunction) Span() lexer.Span {
	return f.Let.Span.Add(spanOf(f.Signature)).Add(spanOf(f.Body))
}

func (f *LetFunction) ASTString(depth int) string {
	// optional fields: TypeParams, Body
	buf := new(strings.Builder)
	buf.WriteString(fmt.Sprintf("LetFunction\n%sLet: %s\n", indent(depth+1), f.Let))
	if len(f.TypeParams) > 0 {
		buf.WriteString(fmt.Sprintf("%sTypeParams: %s\n", indent(depth+1), printNodeSlice(depth+1, f.TypeParams)))
	}
	buf.WriteString(fmt.Sprintf("%sName: %s\n", indent(depth+1), f.Name.ASTString(depth+1)))
	buf.WriteString(fmt.Sprintf("%sSignature: %s\n", indent(depth+1), f.Signature.ASTString(depth+1)))
	if f.Body != nil {
		buf.WriteString(fmt.Sprintf("%sEquals: %s\n%sBody: %s", indent(depth+1), f.Equals, indent(depth+1), f.Body.ASTString(depth+1)))
	}
	return buf.String()
}

type Function struct {
	Fun        lexer.Token
	TypeParams []Node
	Name       Node
	Signature  Node
	FatArrow   lexer.Token
	Body       Node
}

func (f *Function) LeadingTrivia() []lexer.Token {
	return f.Fun.LeadingTrivia
}

func (f *Function) Span() lexer.Span {
	return f.Fun.Span.Add(spanOf(f.Signature)).Add(spanOf(f.Body))
}

func (f *Function) ASTString(depth int) string {
	// optional fields: TypeParams, Name, Body
	buf := new(strings.Builder)
	buf.WriteString(fmt.Sprintf("Function\n%sFun: %s\n", indent(depth+1), f.Fun))
	if len(f.TypeParams) > 0 {
		buf.WriteString(fmt.Sprintf("%sTypeParams: %s\n", indent(depth+1), printNodeSlice(depth+1, f.TypeParams)))
	}
	if f.Name != nil {
		buf.WriteString(fmt.Sprintf("%sName: %s\n", indent(depth+1), f.Name.ASTString(depth+1)))
	}
	buf.WriteString(fmt.Sprintf("%sSignature: %s\n", indent(depth+1), f.Signature.ASTString(depth+1)))
	if f.Body != nil {
		buf.WriteString(fmt.Sprintf("%sFatArrow: %s\n%sBody: %s", indent(depth+1), f.FatArrow, indent(depth+1), f.Body.ASTString(depth+1)))
	}
	return buf.String()
}

type CommaElement struct {
	X     Node
	Comma lexer.Token
}

func (t *CommaElement) ASTString(depth int) string {
	return fmt.Sprintf(
		"CommaElement\n%sX: %s\n%sComma: %s",
		indent(depth+1),
		t.X.ASTString(depth+1), indent(depth+1),
		t.Comma)
}

func (t *CommaElement) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(t.X)
}

func (t *CommaElement) Span() lexer.Span {
	return spanOf(t.X).Add(t.Comma.Span)
}

type Array struct {
	LeftBracket  lexer.Token
	Elements     []Node
	RightBracket lexer.Token
}

func (a *Array) LeadingTrivia() []lexer.Token {
	return a.LeftBracket.LeadingTrivia
}

func (a *Array) Span() lexer.Span {
	return a.LeftBracket.Span.Add(a.RightBracket.Span)
}

func (a *Array) ASTString(depth int) string {
	return fmt.Sprintf(
		"Array\n%sLeftBracket: %s\n%sElements: %s\n%sRightBracket: %s",
		indent(depth+1),
		a.LeftBracket, indent(depth+1),
		printNodeSlice(depth+1, a.Elements), indent(depth+1),
		a.RightBracket)
}

type Tuple struct {
	LeftParen  lexer.Token
	Elements   []Node
	RightParen lexer.Token
}

func (t *Tuple) LeadingTrivia() []lexer.Token {
	return t.LeftParen.LeadingTrivia
}

func (t *Tuple) Span() lexer.Span {
	return t.LeftParen.Span.Add(t.RightParen.Span)
}

func (t *Tuple) ASTString(depth int) string {
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

func (l *LetDecl) LeadingTrivia() []lexer.Token {
	return l.Let.LeadingTrivia
}

func (l *LetDecl) Span() lexer.Span {
	return l.Let.Span.Add(spanOf(l.Destructure)).Add(spanOf(l.Rhs))
}

func (l *LetDecl) ASTString(depth int) string {
	if l.Equals.Type != lexer.Assign {
		return fmt.Sprintf(
			"LetDecl\n%sLet: %s\n%sDestructure: %s",
			indent(depth+1),
			l.Let, indent(depth+1),
			l.Destructure.ASTString(depth+1))
	}
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

func (v *VarDecl) LeadingTrivia() []lexer.Token {
	return v.Var.LeadingTrivia
}

func (v *VarDecl) Span() lexer.Span {
	return v.Var.Span.Add(spanOf(v.Rhs))
}

func (v *VarDecl) ASTString(depth int) string {
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

func (i *IfHeader) LeadingTrivia() []lexer.Token {
	return i.If.LeadingTrivia
}

func (i *IfHeader) Span() lexer.Span {
	return i.If.Span.Add(spanOf(i.Cond))
}

func (i *IfHeader) ASTString(depth int) string {
	return fmt.Sprintf(
		"IfHeader\n%sIf: %s\n%sCond: %s",
		indent(depth+1),
		i.If, indent(depth+1),
		i.Cond.ASTString(depth+1))
}

type If struct {
	IfHeader *IfHeader
	Body     Node
}

func (i *If) LeadingTrivia() []lexer.Token {
	return i.IfHeader.LeadingTrivia()
}

func (i *If) Span() lexer.Span {
	return i.IfHeader.Span().Add(spanOf(i.Body))
}

func (i *If) ASTString(depth int) string {
	return fmt.Sprintf(
		"If\n%sIfHeader: %s\n%sBody: %s",
		indent(depth+1),
		i.IfHeader.ASTString(depth+1), indent(depth+1),
		i.Body.ASTString(depth+1))
}

type IfElse struct {
	IfHeader *IfHeader
	Body     Node
	Else     lexer.Token
	ElseBody Node
}

func (i *IfElse) LeadingTrivia() []lexer.Token {
	return i.IfHeader.LeadingTrivia()
}

func (i *IfElse) Span() lexer.Span {
	return i.IfHeader.Span().Add(spanOf(i.Body)).Add(i.Else.Span).Add(spanOf(i.ElseBody))
}

func (i *IfElse) ASTString(depth int) string {
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
	Name       *Ident
	TypeParams []Node
	With       lexer.Token
	Clause     Node
	Equal      lexer.Token
	Body       Node
}

func (t *TypeDecl) LeadingTrivia() []lexer.Token {
	return t.Type.LeadingTrivia
}

func (t *TypeDecl) Span() lexer.Span {
	return t.Type.Span.Add(spanOf(t.TypeParams)).Add(spanOf(t.Clause)).Add(spanOf(t.Body))
}

func (t *TypeDecl) ASTString(depth int) string {
	// optional fields: TypeParams, Clause, Body
	// none, 1, 1, 1, 2, 2, 2, 3
	// i might try builder for this actually
	buf := new(strings.Builder)
	fmt.Fprintf(buf, "TypeDecl\n%sType: %s\n%sName: %s", indent(depth+1), t.Type, indent(depth+1), t.Name.ASTString(depth+1))
	if len(t.TypeParams) > 0 {
		fmt.Fprintf(buf, "\n%sTypeParams: %s", indent(depth+1), printNodeSlice(depth+1, t.TypeParams))
	}
	if t.With.Type == lexer.With {
		fmt.Fprintf(buf, "\n%sWith: %s\n%sClause: %s", indent(depth+1), t.With, indent(depth+1), t.Clause.ASTString(depth+1))
	}
	if t.Equal.Type == lexer.Assign {
		fmt.Fprintf(buf, "\n%sEqual: %s\n%sBody: %s", indent(depth+1), t.Equal, indent(depth+1), t.Body.ASTString(depth+1))
	}
	return buf.String()
}

type Number struct {
	Lit lexer.Token
}

// IsNat reports whether the number literal is a composed of ascii digits only.
func (n *Number) IsNat() bool {
	return strings.IndexFunc(n.Lit.Data, func(r rune) bool {
		return !unicode.IsDigit(r)
	}) == -1
}

func (n *Number) LeadingTrivia() []lexer.Token {
	return n.Lit.LeadingTrivia
}

func (n *Number) Span() lexer.Span {
	return n.Lit.Span
}

func (n *Number) ASTString(depth int) string {
	return fmt.Sprintf("Number %s", n.Lit)
}

type TypeArg struct {
	TypeArg lexer.Token
}

func (n *TypeArg) LeadingTrivia() []lexer.Token {
	return n.TypeArg.LeadingTrivia
}

func (n *TypeArg) Span() lexer.Span {
	return n.TypeArg.Span
}

func (n *TypeArg) ASTString(depth int) string {
	return n.TypeArg.String()
}

type SumType struct {
	Elements []Node
}

func (s *SumType) LeadingTrivia() []lexer.Token {
	if len(s.Elements) == 0 {
		return nil
	}
	return leadingTriviaOf(s.Elements[0])
}

func (s *SumType) Span() lexer.Span {
	return spanOf(s.Elements)
}

func (s *SumType) ASTString(depth int) string {
	return fmt.Sprintf("SumType\n%sElements: %s", indent(depth+1), printNodeSlice(depth+1, s.Elements))
}

type SumTypeElement struct {
	Or   lexer.Token
	Name *Ident
	Type Node
}

func (s *SumTypeElement) LeadingTrivia() []lexer.Token {
	return s.Or.LeadingTrivia
}

func (s *SumTypeElement) Span() lexer.Span {
	return s.Or.Span.Add(spanOf(s.Name)).Add(spanOf(s.Type))
}

func (s *SumTypeElement) ASTString(depth int) string {
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

func (f *ForallType) LeadingTrivia() []lexer.Token {
	return f.TypeArg.LeadingTrivia()
}

func (f *ForallType) Span() lexer.Span {
	return spanOf(f.TypeArg).Add(spanOf(f.Period)).Add(spanOf(f.Type))
}

func (f *ForallType) ASTString(depth int) string {
	return fmt.Sprintf("ForallType\n%sTypeArg: %s\n%sPeriod: %s\n%sType: %s", indent(depth+1), f.TypeArg.ASTString(depth+1), indent(depth+1), f.Period, indent(depth+1), f.Type.ASTString(depth+1))
}

type FunctionType struct {
	Fun    lexer.Token
	Param  Node
	Arrows []Node
}

func (f *FunctionType) LeadingTrivia() []lexer.Token {
	return f.Fun.LeadingTrivia
}

func (f *FunctionType) Span() lexer.Span {
	return f.Fun.Span.Add(spanOf(f.Param)).Add(spanOf(f.Arrows))
}

func (f *FunctionType) ASTString(depth int) string {
	return fmt.Sprintf(
		"FunctionType\n%sFun: %s\n%sParam: %s\n%sArrows: %s",
		indent(depth+1), f.Fun, indent(depth+1), f.Param.ASTString(depth+1),
		indent(depth+1), printNodeSlice(depth+1, f.Arrows))
}

type Field struct {
	Name    *Ident
	Colon   lexer.Token
	Type    Node
	Equals  lexer.Token
	Default Node
}

func (f *Field) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(f.Name)
}

func (f *Field) Span() lexer.Span {
	return spanOf(f.Name).Add(spanOf(f.Type))
}

func (f *Field) ASTString(depth int) string {
	if f.Default != nil {
		return fmt.Sprintf("Field\n%sName: %s\n%sColon: %s\n%sType: %s\n%sEquals: %s\n%sDefault: %s", indent(depth+1), f.Name.ASTString(depth+1), indent(depth+1), f.Colon, indent(depth+1), f.Type.ASTString(depth+1), indent(depth+1), f.Equals, indent(depth+1), f.Default.ASTString(depth+1))
	}
	return fmt.Sprintf("Field\n%sName: %s\n%sColon: %s\n%sType: %s", indent(depth+1), f.Name.ASTString(depth+1), indent(depth+1), f.Colon, indent(depth+1), f.Type.ASTString(depth+1))
}

type PrefixExpr struct {
	Op lexer.Token
	X  Node
}

func (p *PrefixExpr) LeadingTrivia() []lexer.Token {
	return p.Op.LeadingTrivia
}

func (p *PrefixExpr) Span() lexer.Span {
	return p.Op.Span.Add(spanOf(p.X))
}

func (p *PrefixExpr) ASTString(depth int) string {
	return fmt.Sprintf("PrefixExpr\n%sOp: %s\n%sX: %s", indent(depth+1), p.Op, indent(depth+1), p.X.ASTString(depth+1))
}

type CallExpr struct {
	Elements []Node
}

func (c *CallExpr) LeadingTrivia() []lexer.Token {
	if len(c.Elements) == 0 {
		return nil
	}
	return leadingTriviaOf(c.Elements[0])
}

func (c *CallExpr) Span() lexer.Span {
	return spanOf(c.Elements)
}

func (c *CallExpr) ASTString(depth int) string {
	return fmt.Sprintf("CallExpr\n%sElements: %s", indent(depth+1), printNodeSlice(depth+1, c.Elements))
}

type PostfixExpr struct {
	X  Node
	Op lexer.Token
}

func (p *PostfixExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(p.X)
}

func (p *PostfixExpr) Span() lexer.Span {
	return spanOf(p.X).Add(p.Op.Span)
}

func (p *PostfixExpr) ASTString(depth int) string {
	return fmt.Sprintf("PostfixExpr\n%sX: %s\n%sOp: %s", indent(depth+1), p.X.ASTString(depth+1), indent(depth+1), p.Op)
}

type SelectorExpr struct {
	X      Node
	Period lexer.Token
	Name   *Ident
}

func (s *SelectorExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(s.X)
}

func (s *SelectorExpr) Span() lexer.Span {
	return spanOf(s.X).Add(spanOf(s.Name))
}

func (s *SelectorExpr) ASTString(depth int) string {
	return fmt.Sprintf("SelectorExpr\n%sX: %s\n%sPeriod: %s\n%sName: %s", indent(depth+1), s.X.ASTString(depth+1), indent(depth+1), s.Period, indent(depth+1), s.Name.ASTString(depth+1))
}

type PatternCase struct {
	Or      lexer.Token
	Pattern Node
	Guard   *IfHeader
	Arrow   lexer.Token
	Expr    Node
	Comma   lexer.Token
}

func (p *PatternCase) LeadingTrivia() []lexer.Token {
	return p.Or.LeadingTrivia
}

func (p *PatternCase) Span() lexer.Span {
	if p.Comma.Type == lexer.Comma {
		return p.Or.Span.Add(p.Comma.Span)
	}
	return p.Or.Span.Add(spanOf(p.Expr))
}

func (p *PatternCase) ASTString(depth int) string {
	if p.Guard == nil {
		return fmt.Sprintf(
			"PatternCase\n%sOr: %s\n%sPattern: %s\n%sArrow: %s\n%sExpr: %s\n%sComma: %s", indent(depth+1), p.Or, indent(depth+1),
			p.Pattern.ASTString(depth+1), indent(depth+1), p.Arrow, indent(depth+1),
			p.Expr.ASTString(depth+1), indent(depth+1), p.Comma)
	}
	return fmt.Sprintf("PatternCase\n%sOr: %s\n%sPattern: %s\n%sGuard: %s\n%sArrow: %s\n%sExpr: %s\n%sComma: %s", indent(depth+1), p.Or, indent(depth+1), p.Pattern.ASTString(depth+1), indent(depth+1), p.Guard.ASTString(depth+1), indent(depth+1), p.Arrow, indent(depth+1), p.Expr.ASTString(depth+1), indent(depth+1), p.Comma)
}

type IfMatch struct {
	IfHeader   *IfHeader
	LeftBrace  lexer.Token
	Cases      []*PatternCase
	RightBrace lexer.Token
}

func (i *IfMatch) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(i.IfHeader)
}

func (i *IfMatch) Span() lexer.Span {
	return spanOf(i.IfHeader).Add(spanOf(i.Cases)).Add(spanOf(i.RightBrace))
}

func (i *IfMatch) ASTString(depth int) string {
	if i.LeftBrace.Type == lexer.LeftBrace {
		return fmt.Sprintf(
			"IfMatch\n%sIfHeader: %s\n%sLeftBrace: %s\n%sCases: %s\n%sRightBrace: %s", indent(depth+1), i.IfHeader.ASTString(depth+1), indent(depth+1),
			i.LeftBrace, indent(depth+1), printCaseSlice(depth+1, i.Cases), indent(depth+1), i.RightBrace)
	}
	return fmt.Sprintf(
		"IfMatch\n%sIfHeader: %s\n%sCases: %s", indent(depth+1), i.IfHeader.ASTString(depth+1), indent(depth+1),
		printCaseSlice(depth+1, i.Cases))
}

type BasicString struct {
	Lit lexer.Token
}

func (s *BasicString) LeadingTrivia() []lexer.Token {
	return s.Lit.LeadingTrivia
}

func (s *BasicString) Span() lexer.Span {
	return s.Lit.Span
}

func (s *BasicString) ASTString(depth int) string {
	return fmt.Sprintf("BasicString %s", s.Lit)
}

type InterpolatedString struct {
	Parts []Node
}

func (s *InterpolatedString) LeadingTrivia() []lexer.Token {
	if len(s.Parts) == 0 {
		return nil
	}
	return leadingTriviaOf(s.Parts[0])
}

func (s *InterpolatedString) Span() lexer.Span {
	return spanOf(s.Parts)
}

func (s *InterpolatedString) ASTString(depth int) string {
	return fmt.Sprintf("InterpolatedString\n%sParts: %s", indent(depth+1), printNodeSlice(depth+1, s.Parts))
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

type IndexExpr struct {
	X             Node
	LeftBracket   lexer.Token
	IndexElements []Node
	RightBracket  lexer.Token
}

func (i *IndexExpr) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(i.X)
}

func (i *IndexExpr) Span() lexer.Span {
	return spanOf(i.X).Add(i.RightBracket.Span)
}

func (i *IndexExpr) ASTString(depth int) string {
	return fmt.Sprintf("IndexExpr\n%sX: %s\n%sLeftBracket: %s\n%sIndexElements: %s\n%sRightBracket: %s", indent(depth+1), i.X.ASTString(depth+1), indent(depth+1), i.LeftBracket, indent(depth+1), printNodeSlice(depth+1, i.IndexElements), indent(depth+1), i.RightBracket)
}

type ImportDecl struct {
	Import lexer.Token
	Decl   Node
}

func (i *ImportDecl) LeadingTrivia() []lexer.Token {
	return i.Import.LeadingTrivia
}

func (i *ImportDecl) Span() lexer.Span {
	return i.Import.Span.Add(spanOf(i.Decl))
}

func (i *ImportDecl) ASTString(depth int) string {
	return fmt.Sprintf("ImportDecl\n%sImport: %s\n%sDecl: %s", indent(depth+1), i.Import, indent(depth+1), i.Decl.ASTString(depth+1))
}

type ImportPath struct {
	Lit         lexer.Token
	PackageName string
}

func (i *ImportPath) LeadingTrivia() []lexer.Token {
	return i.Lit.LeadingTrivia
}

func (i *ImportPath) Span() lexer.Span {
	return i.Lit.Span
}

func (i *ImportPath) ASTString(depth int) string {
	return i.Lit.String()
}

type As struct {
	X     Node
	As    lexer.Token
	Alias *Ident
}

func (a *As) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(a.X)
}

func (a *As) Span() lexer.Span {
	return spanOf(a.X).Add(spanOf(a.Alias))
}

func (a *As) ASTString(depth int) string {
	return fmt.Sprintf("As\n%sX: %s\n%sAs: %s\n%sAlias: %s", indent(depth+1), a.X.ASTString(depth+1), indent(depth+1), a.As, indent(depth+1), a.Alias.ASTString(depth+1))
}

// type ImportAlias struct {
// 	Path  *ImportPath
// 	As    lexer.Token
// 	Alias *Ident
// }

// type ImportDot struct {
// 	Path *ImportPath
// 	Dot  lexer.Token
// 	Def  Node
// }

// type ImportParen struct {
// 	Path    *ImportPath
// 	DefList Node
// }

// type ImportDeclPackage struct {
// 	Path *ImportPath

// 	// as, . ()
// 	// Binding Node
// 	// Equals  lexer.Token
// 	// Path    *BasicString
// }

// func (i *ImportDeclPackage) LeadingTrivia() []lexer.Token {
// 	if i.Binding != nil {
// 		return leadingTriviaOf(i.Binding)
// 	}
// 	return leadingTriviaOf(i.Path)
// }

// func (i *ImportDeclPackage) Span() lexer.Span {
// 	if i.Binding != nil {
// 		return spanOf(i.Binding).Add(i.Equals.Span).Add(spanOf(i.Path))
// 	}
// 	return spanOf(i.Path)
// }

// func (i *ImportDeclPackage) ASTString(depth int) string {
// 	if i.Binding != nil {
// 		return fmt.Sprintf("ImportDeclPackage\n%sBinding: %s\n%sEquals: %s\n%sPath: %s", indent(depth+1), i.Binding.ASTString(depth+1), indent(depth+1), i.Equals, indent(depth+1), i.Path.ASTString(depth+1))
// 	}
// 	return fmt.Sprintf("ImportDeclPackage\n%sPath: %s", indent(depth+1), i.Path.ASTString(depth+1))
// }

type ImplDecl struct {
	Impl   lexer.Token
	Name   Node
	Args   Node
	With   lexer.Token
	Clause Node
	Equals lexer.Token
	Body   Node
}

func (i *ImplDecl) LeadingTrivia() []lexer.Token {
	return i.Impl.LeadingTrivia
}

func (i *ImplDecl) Span() lexer.Span {
	return i.Impl.Span.Add(spanOf(i.Args)).Add(spanOf(i.Clause)).Add(spanOf(i.Body))
}

func (i *ImplDecl) ASTString(depth int) string {
	// body can be nil and clause can be nil
	// so we have the case where nothing is nil
	// the case where body is nil
	// the case where clause is nil
	// the case where both are nil
	if i.Body != nil && i.Clause != nil {
		return fmt.Sprintf("ImplDecl\n%sImpl: %s\n%sName: %s\n%sArgs: %s\n%sWith: %s\n%sClause: %s\n%sEquals: %s\n%sBody: %s", indent(depth+1), i.Impl, indent(depth+1), i.Name.ASTString(depth+1), indent(depth+1), i.Args.ASTString(depth+1), indent(depth+1), i.With, indent(depth+1), i.Clause.ASTString(depth+1), indent(depth+1), i.Equals, indent(depth+1), i.Body.ASTString(depth+1))
	}
	if i.Body != nil {
		// this implies that With and Clause don't exist
		return fmt.Sprintf("ImplDecl\n%sImpl: %s\n%sName: %s\n%sArgs: %s\n%sEquals: %s\n%sBody: %s", indent(depth+1), i.Impl, indent(depth+1), i.Name.ASTString(depth+1), indent(depth+1), i.Args.ASTString(depth+1), indent(depth+1), i.Equals, indent(depth+1), i.Body.ASTString(depth+1))
	}
	if i.Clause != nil {
		// this implies that Equals and Body don't exist
		return fmt.Sprintf("ImplDecl\n%sImpl: %s\n%sName: %s\n%sArgs: %s\n%sWith: %s\n%sClause: %s", indent(depth+1), i.Impl, indent(depth+1), i.Name.ASTString(depth+1), indent(depth+1), i.Args.ASTString(depth+1), indent(depth+1), i.With, indent(depth+1), i.Clause.ASTString(depth+1))
	}
	// this implies that With, Clause, Equals, and Body don't exist
	return fmt.Sprintf("ImplDecl\n%sImpl: %s\n%sName: %s\n%sArgs: %s", indent(depth+1), i.Impl, indent(depth+1), i.Name.ASTString(depth+1), indent(depth+1), i.Args.ASTString(depth+1))
}

type ArrayType struct {
	LeftBracket  lexer.Token
	Length       lexer.Token
	RightBracket lexer.Token
	Type         Node
}

func (a *ArrayType) LeadingTrivia() []lexer.Token {
	return a.LeftBracket.LeadingTrivia
}

func (a *ArrayType) Span() lexer.Span {
	return a.LeftBracket.Span.Add(spanOf(a.Type))
}

func (a *ArrayType) ASTString(depth int) string {
	if a.Length.Type != lexer.Number {
		return fmt.Sprintf("ArrayType\n%sLeftBracket: %s\n%sRightBracket: %s\n%sType: %s", indent(depth+1), a.LeftBracket, indent(depth+1), a.RightBracket, indent(depth+1), a.Type.ASTString(depth+1))
	}
	return fmt.Sprintf("ArrayType\n%sLeftBracket: %s\n%sLength: %s\n%sRightBracket: %s\n%sType: %s", indent(depth+1), a.LeftBracket, indent(depth+1), a.Length, indent(depth+1), a.RightBracket, indent(depth+1), a.Type.ASTString(depth+1))
}

type NillableType struct {
	Type         Node
	QuestionMark lexer.Token
}

func (n *NillableType) LeadingTrivia() []lexer.Token {
	return leadingTriviaOf(n.Type)
}

func (n *NillableType) Span() lexer.Span {
	return spanOf(n.Type).Add(n.QuestionMark.Span)
}

func (n *NillableType) ASTString(depth int) string {
	return fmt.Sprintf("NillableType\n%sType: %s\n%sQuestionMark: %s", indent(depth+1), n.Type.ASTString(depth+1), indent(depth+1), n.QuestionMark)
}
