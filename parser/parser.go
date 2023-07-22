package parser

import (
	"fmt"
	"io/fs"
	"strconv"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/lexer"
	"golang.org/x/exp/maps"
)

const debug = true

type parser struct {
	shouldInsertAfter  bool
	afterToksToCheck   []lexer.TokenType
	shouldInsertBefore bool
	beforeToksToCheck  []lexer.TokenType
	l                  Lexer
	tok                lexer.Token
	buf                []lexer.Token // rework this when you need to start backtracking.
	indent             int
	imports            map[string]struct{}
}

type Lexer interface {
	Next() lexer.Token
	ShouldInsertBefore(bool, []lexer.TokenType)
	ShouldInsertAfter(bool, []lexer.TokenType)
}

func (p *parser) shouldInsertDelimAfter(status bool, ttypes ...lexer.TokenType) {
	p.shouldInsertAfter = status
	p.afterToksToCheck = ttypes
	p.l.ShouldInsertAfter(status, ttypes)
}

func (p *parser) shouldInsertDelimBefore(status bool, ttypes ...lexer.TokenType) {
	p.shouldInsertBefore = status
	p.beforeToksToCheck = ttypes
	p.l.ShouldInsertBefore(status, ttypes)
}

func (p *parser) trace(msg string) func() {
	if debug {
		fmt.Printf("%*s%s\n", p.indent*2, "", msg)
		p.indent++
		return func() {
			p.indent--
		}
	}
	return func() {}
}

func ParseFile(fsys fs.FS, filename string) (ast.Node, error) {
	l, err := lexer.NewLexer(fsys, filename)
	if err != nil {
		return nil, err
	}
	return (&parser{l: l, imports: make(map[string]struct{})}).parseFile(), nil
}

func ParsePackage(fsys fs.FS, scriptFile string, filenames ...string) (ast.Node, error) {
	var pkg ast.Package
	pkg.Imports = make(map[string]struct{})
	var first error
	for _, filename := range filenames {
		file, err := ParseFile(fsys, filename)
		if err != nil {
			if first == nil {
				first = err
			}
		} else {
			if file, ok := file.(*ast.File); ok {
				file.Filename = filename
				if filename == scriptFile {
					if file.Package.Type == lexer.Package {
						return nil, fmt.Errorf("script file cannot be a package file")
					}
					pkg.ScriptFile = file
				} else {
					pkg.PackageFiles = append(pkg.PackageFiles, file)
					name := file.PackageName.Name.Data
					if pkg.Name == "" {
						pkg.Name = name
					} else {
						if pkg.Name != name {
							return nil, fmt.Errorf("package name mismatch: %s != %s", pkg.Name, name)
						}
					}
				}
				maps.Copy(pkg.Imports, file.Imports)
			}
		}
	}
	if len(pkg.PackageFiles) == 0 && pkg.ScriptFile.Filename == "" {
		return nil, fmt.Errorf("no .gf files specified")
	}
	if len(pkg.PackageFiles) > 0 && pkg.Name == "" {
		return nil, fmt.Errorf("package name not found")
	}
	return &pkg, first
}

func (p *parser) next() {
	if len(p.buf) > 0 {
		p.tok = p.buf[0]
		p.buf = p.buf[1:]
		return
	}
	p.tok = p.l.Next()
}

func (p *parser) peek() lexer.Token {
	if len(p.buf) == 0 {
		p.buf = append(p.buf, p.l.Next())
	}
	return p.buf[0]
}

func (p *parser) peek2() lexer.Token {
	if len(p.buf) < 2 {
		p.buf = append(p.buf, p.l.Next())
	}
	return p.buf[1]
}

func (p *parser) parseFile() *ast.File {
	defer p.trace("parseFile")()
	p.next()
	f := &ast.File{}
	if p.tok.Type == lexer.Package {
		f.Package = p.tok
		p.next()
		if p.tok.Type == lexer.Ident {
			f.PackageName = &ast.Ident{Name: p.tok}
		} else {
			panic("expected identifier after package")
			// do I need to give it a span?
			// f.PackageName = Illegal{Msg: "expected identifier after package"}
		}
		p.next()
	}
	if p.tok.Type == lexer.LineTerminator {
		p.next()
	}
	f.Body = p.parseBody(f.Package.Type == lexer.Package, lexer.EOF)
	if p.tok.Type == lexer.EOF {
		f.SetTrailingTrivia(p.tok.LeadingTrivia)
	}
	f.Imports = p.imports
	return f
}

// func (p *parser) parseLetFun() ast.Node {
// 	defer p.trace("parseLetFun")()
// 	var letFun LetFunction
// 	for p.tok.Type == lexer.TypeArg {
// 		letFun.TypeParams = append(letFun.TypeParams, p.parseNamedTypeArgument())
// 	}
// 	if p.tok.Type == lexer.Ident {
// }

// TODO: rework LetDecl to handle function and :=
// LetDecl = "let" RestDecl
// TOOD: add let f x = x syntax
func (p *parser) parseLetDecl() ast.Node {
	defer p.trace("parseLetDecl")()
	letTok := p.tok
	p.next()
	letFun := new(ast.LetFunction)
	if p.tok.Type == lexer.TypeArg {
		for p.tok.Type == lexer.TypeArg {
			letFun.TypeParams = append(letFun.TypeParams, p.parseNamedTypeArgument())
		}
		if p.tok.Type != lexer.Ident {
			panic("expected identifier after type arguments")
		}
		switch p.peek().Type {
		case lexer.Ident, lexer.LeftParen:
		default:
			panic("expected identifier or ( after function name")
		}
	}
	if p.tok.Type == lexer.Ident {
		switch p.peek().Type {
		case lexer.Ident, lexer.LeftParen:
			letFun.Name = &ast.Ident{Name: p.tok}
			p.next()
			letFun.Signature = p.parseFunctionSignature()
			if p.tok.Type == lexer.LineTerminator && p.peek().Type == lexer.LeftBrace {
				p.next()
			} else if p.tok.Type == lexer.Assign {
				letFun.Equals = p.tok
				p.next()
			} else if p.tok.Type != lexer.LeftBrace {
				if p.tok.Type == lexer.FatArrow {
					panic("expected = or { after function signature")
				}
				return letFun // accept forward declaration
				// panic("expected = or { after function signature") // handle forward declaration
			}
			letFun.Body = p.parseExpr()
			return letFun
		}
	}
	decl := &ast.LetDecl{Let: letTok}
	decl.Destructure = p.parseDestructure()
	if isIllegal(decl.Destructure) {
		return decl.Destructure
	}
	if p.tok.Type != lexer.Assign {
		if p.tok.Type == lexer.FatArrow {
			panic("expected = or { after function signature")
		}
		return decl
		// return Illegal{ast.Node: decl.Destructure, Msg: "expected = after let declaration"}
	}
	decl.Equals = p.tok
	p.next()
	decl.Rhs = p.parseExpr()
	return decl
}

func isIllegal(n ast.Node) bool {
	_, ok := n.(*ast.Illegal)
	return ok
}

// VarDecl = "var" RestDecl
func (p *parser) parseVarDecl() ast.Node {
	defer p.trace("parseVarDecl")()
	decl := &ast.VarDecl{Var: p.tok}
	p.next()
	decl.Destructure, decl.Equals, decl.Rhs = p.restDecl()
	if isIllegal(decl.Destructure) {
		return decl.Destructure
	}
	return decl
}

// RestDecl = Destructure "=" Expr
func (p *parser) restDecl() (lhs ast.Node, equals lexer.Token, rhs ast.Node) {
	defer p.trace("restDecl")()
	lhs = p.parseDestructure()
	if isIllegal(lhs) {
		return lhs, lexer.Token{}, nil
	}
	if p.tok.Type != lexer.Assign {
		return &ast.Illegal{Node: lhs, Msg: "expected = after let declaration"}, lexer.Token{}, nil
	}
	equals = p.tok
	p.next()
	rhs = p.parseExpr()
	return lhs, equals, rhs
}

// signature and function type are different. difference between function signature and function type is that types are optional in signatures
// and a function signature must have a parameter for the first argument, except if it's of unit type.
// If '=' is not a typeclass constraint, then it must be a default value. That's a property of tuple types though.
// fun ident
// fun ident: TypeBody
// fun TupleType
// fun ident -> TypeBody
// fun ident: TypeBody -> TypeBody
// fun TupleType -> TypeBody
// fun .. -> .. ->
func (p *parser) parseFunctionSignature() *ast.FunctionSignature {
	defer p.trace("parseFunctionSignature")()
	var fun ast.FunctionSignature
	if p.tok.Type == lexer.Ident {
		var param ast.TypeAnnotation
		param.Destructure = &ast.Ident{Name: p.tok}
		p.next()
		if p.tok.Type == lexer.Colon {
			param.Colon = p.tok
			p.next()
			param.Type = p.parseTypeBody(false, false)
		}
		fun.Param = &param
	} else if p.tok.Type == lexer.LeftParen {
		// TODO: is this too general for function declarations?
		// It permits fun foo (bool)
		fun.Param = p.parseTupleType()
	} else {
		panic("expected ident or tuple type for function signature")
	}
	for p.tok.Type == lexer.RightArrow {
		var arrow ast.Arrow
		arrow.Arrow = p.tok
		p.next()
		arrow.Type = p.parseTypeBody(false, false)
		fun.Arrows = append(fun.Arrows, &arrow)
	}
	if p.tok.Type == lexer.With {
		fun.With = p.tok
		p.next()
		fun.Clause = p.parseWithClause()
	}
	return &fun
}

func (p *parser) parseFun(topLevel bool) ast.Node {
	defer p.trace("parseFun")()
	fun := ast.Function{Fun: p.tok}
	p.next()
	for p.tok.Type == lexer.TypeArg {
		fun.TypeParams = append(fun.TypeParams, p.parseNamedTypeArgument())
	}
	if p.tok.Type == lexer.Ident {
		switch p.peek().Type {
		case lexer.Ident, lexer.LeftParen:
			fun.Name = &ast.Ident{Name: p.tok}
			p.next()
		}
	}
	if topLevel && fun.Name == nil {
		panic("expected function name at top level")
	}
	fun.Signature = p.parseFunctionSignature()
	if p.tok.Type == lexer.LineTerminator && p.peek().Type == lexer.LeftBrace {
		p.next()
	} else if p.tok.Type == lexer.FatArrow {
		fun.FatArrow = p.tok
		p.next()
	} else if p.tok.Type != lexer.LeftBrace {
		if p.tok.Type == lexer.Assign {
			panic("expected => or { after function signature")
		}
		// handle forward declarations
		return &fun
		// panic("expected => or { after function signature")
	}
	fun.Body = p.parseExpr()
	return &fun
}

func (p *parser) parseOperand() ast.Node {
	defer p.trace("parseOperand")()
	switch tok := p.tok; tok.Type {
	case lexer.Fun:
		return p.parseFun(false)
	case lexer.If:
		return p.parseIf()
	case lexer.LeftParen:
		return p.parseTuple()
	case lexer.LeftBrace:
		p.next()
		block := p.parseBody(false, lexer.RightBrace)
		if p.tok.Type != lexer.RightBrace {
			panic("expected }")
		}
		block.LeftBrace = tok
		block.RightBrace = p.tok
		p.next()
		return block
	case lexer.Ident:
		p.next()
		return &ast.Ident{Name: tok}
	case lexer.Number:
		p.next()
		return &ast.Number{Lit: tok}
	case lexer.StringBeg, lexer.String:
		return p.parseString()
	case lexer.LeftBracket:
		return p.parseArray()
	}
	res := ast.Illegal{Msg: "expected operand"}
	res.SetSpan(p.tok.Span)
	p.next()
	return &res
}

func (p *parser) parseString() ast.Node {
	defer p.trace("parseString")()
	switch tok := p.tok; tok.Type {
	case lexer.StringBeg:
		var str ast.InterpolatedString
		str.Parts = append(str.Parts, &ast.BasicString{Lit: p.tok})
		p.next()
		for p.tok.Type != lexer.StringEnd && p.tok.Type != lexer.EOF {
			str.Parts = append(str.Parts, p.parseExpr())
			if p.tok.Type == lexer.StringPart {
				str.Parts = append(str.Parts, &ast.BasicString{Lit: p.tok})
				p.next()
			} else if p.tok.Type != lexer.StringEnd {
				panic("expected string part or end")
			}
		}
		if p.tok.Type != lexer.StringEnd {
			panic("expected string end")
		}
		str.Parts = append(str.Parts, &ast.BasicString{Lit: p.tok})
		p.next()
		return &str
	case lexer.String:
		p.next()
		return &ast.BasicString{Lit: tok}
	}
	panic("expected string")
}

func (p *parser) parseIndexExpr(x ast.Node) ast.Node {
	defer p.trace("parseIndexExpr")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var index ast.IndexExpr
	index.X = x
	index.LeftBracket = p.tok
	p.next()
	for p.tok.Type != lexer.RightBracket && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseExpr()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		index.IndexElements = append(index.IndexElements, &elem)
	}
	if p.tok.Type != lexer.RightBracket {
		panic("missing right bracket")
	}
	index.RightBracket = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &index
}

func (p *parser) parsePrimaryExpr() ast.Node {
	defer p.trace("parsePrimaryExpr")()
	x := p.parseOperand()
L:
	for {
		switch op := p.tok; {
		case op.IsPostfixOp():
			// if it's DotDot and the next token is the start of an expression, just return the x
			if op.Type == lexer.DotDot && p.peek().BeginsPrefixExpr() {
				break L
			} else {
				p.next()
				x = &ast.PostfixExpr{X: x, Op: op}
			}
		case op.Type == lexer.Period:
			p.next()
			if p.tok.Type != lexer.Ident {
				panic("expected ident after .")
			}
			id := p.tok
			p.next()
			x = &ast.SelectorExpr{X: x, Period: op, Name: &ast.Ident{Name: id}}
		case op.Type == lexer.LeftBracket && len(p.tok.LeadingTrivia) == 0:
			// greedily consume index expression. if leading trivia is present, it's an array literal
			x = p.parseIndexExpr(x)
		default:
			break L
		}
	}
	return x
}

func (p *parser) parsePrefixExpr() ast.Node {
	defer p.trace("parsePrefixExpr")()
	if p.tok.IsPrefixOp() {
		op := p.tok
		p.next()
		return &ast.PrefixExpr{Op: op, X: p.parsePrefixExpr()}
	}
	return p.parsePrimaryExpr()
}

func (p *parser) parseCallExpr() ast.Node {
	defer p.trace("parseCallExpr")()
	exprs := []ast.Node{p.parsePrefixExpr()}
	for p.tok.BeginsArgumentExpr() {
		exprs = append(exprs, p.parsePrefixExpr())
	}
	if len(exprs) == 1 {
		return exprs[0]
	}
	return &ast.CallExpr{Elements: exprs}
}

func (p *parser) parseBinaryExpr(minPrec int) ast.Node {
	defer p.trace("parseBinaryExpr")()
	res := p.parseCallExpr()
	for p.tok.IsBinaryOp() && p.tok.Prec() >= minPrec {
		op := p.tok
		p.next()
		nextMinPrec := op.Prec()
		if op.IsLeftAssoc() {
			nextMinPrec++
		}
		var rhs ast.Node
		switch op.Type {
		case lexer.Colon:
			rhs = p.parseTypeBody(false, false)
		default:
			rhs = p.parseBinaryExpr(nextMinPrec)
		}
		res = filterBinExp(&ast.BinaryExpr{Left: res, Op: op, Right: rhs})
	}
	return res
}

func getBinExp(x ast.Node) *ast.BinaryExpr {
	switch x := x.(type) {
	case *ast.BinaryExpr:
		return x
	case *ast.Illegal:
		return getBinExp(x.Node)
	default:
		return nil
	}
}

func filterBinExp(x ast.Node) ast.Node {
	top, ok := x.(*ast.BinaryExpr)
	if !ok {
		return x
	}
	if leftbe := getBinExp(top.Left); leftbe != nil {
		if !leftbe.Op.BinaryInteroperable(top.Op) {
			return &ast.Illegal{
				Node: x,
				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", leftbe.Op.Type, top.Op.Type),
			}
		}
	}
	if rightbe := getBinExp(top.Right); rightbe != nil {
		if !rightbe.Op.BinaryInteroperable(top.Op) {
			return &ast.Illegal{
				Node: x,
				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", top.Op.Type, rightbe.Op.Type),
			}
		}
	}
	return x
}

func (p *parser) parseExpr() ast.Node {
	defer p.trace("parseExpr")()
	return p.parseBinaryExpr(lexer.MinPrec)
}

// Destructure =
//
//	| ident
//	| Destructure ":" TypeBody
//	| "(" Destructure ")"
//	| "(" Destructure "," Destructure ")";
func (p *parser) parseDestructure() ast.Node {
	defer p.trace("parseDestructure")()
	var des ast.Node
	switch p.tok.Type {
	case lexer.Ident:
		des = &ast.Ident{Name: p.tok}
		p.next()
	case lexer.LeftParen:
		des = p.parseTupleDestructure()
	default:
		panic("missing comma or right paren")
	}
	if p.tok.Type == lexer.Colon {
		colon := p.tok
		p.next()
		typ := p.parseTypeBody(false, false)
		des = &ast.TypeAnnotation{
			Destructure: des,
			Colon:       colon,
			Type:        typ,
		}
	}
	return des
}

// TupleDestructure = "(" Destructure ")"
// TupleDestructure = "(" Destructure "," Destructure ")"
func (p *parser) parseTupleDestructure() ast.Node {
	defer p.trace("parseTupleDestructure")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	leftParen := p.tok
	p.next()
	var elems []ast.Node
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseDestructure()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		} else if p.tok.Type != lexer.RightParen {
			panic("missing comma or right paren")
		}
		elems = append(elems, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	rightParen := p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &ast.Tuple{
		LeftParen:  leftParen,
		Elements:   elems,
		RightParen: rightParen,
	}
}

// IfHeader = "if" "(" Expr ")"
// IfElse = IfHeader Expr [ "else" Expr ]
// IfMatch = IfHeader ( "|" Pattern "=>" Expr )+
// If = IfElse | IfMatch
func (p *parser) parseIf() ast.Node {
	defer p.trace("parseIf")()
	ifHeader := ast.IfHeader{If: p.tok}
	p.next()
	if p.tok.Type != lexer.LeftParen {
		panic("missing left paren")
	}
	ifHeader.Cond = p.parseTuple()
	if p.tok.Type == lexer.LineTerminator {
		p.next()
	}
	if p.tok.Type == lexer.Or || (p.tok.Type == lexer.LeftBrace && p.peek().Type == lexer.Or) {
		return p.parseIfMatch(ifHeader)
	}
	body := p.parseExpr()
	if p.tok.Type == lexer.Else {
		elseTok := p.tok
		p.next()
		elseBody := p.parseExpr()
		return &ast.IfElse{
			IfHeader: &ifHeader,
			Body:     body,
			Else:     elseTok,
			ElseBody: elseBody,
		}
	}
	return &ast.If{
		IfHeader: &ifHeader,
		Body:     body,
	}
}

func (p *parser) parseIfMatch(ifHeader ast.IfHeader) ast.Node {
	defer p.trace("parseIfMatch")()
	var ifMatch ast.IfMatch
	var setRbrace bool
	if p.tok.Type == lexer.LeftBrace {
		ifMatch.LeftBrace = p.tok
		setRbrace = true
		p.next()
	}
	var cases []ast.Node
	for p.tok.Type == lexer.Or {
		c := p.parseCase(&ifMatch, setRbrace)
		cases = append(cases, c)
		if ifMatch.RightBrace.Type == lexer.RightBrace {
			break
		}
	}
	// if p.tok.Type == lexer.RightBrace {
	// 	ifMatch.RightBrace = p.tok
	// 	p.next()
	// }
	ifMatch.IfHeader = &ifHeader
	ifMatch.Cases = cases
	return &ifMatch
}

func (p *parser) parseCase(ifMatch *ast.IfMatch, setRbrace bool) ast.Node {
	defer p.trace("parseCase")()
	var patCase ast.PatternCase
	patCase.Or = p.tok
	p.next()
	patCase.Pattern = p.parsePattern()
	if p.tok.Type == lexer.If {
		patCase.Guard = p.parseGuard()
	}
	if p.tok.Type != lexer.FatArrow {
		panic("missing arrow")
	}
	patCase.Arrow = p.tok
	p.next()
	p.shouldInsertDelimAfter(true, lexer.Or)
	patCase.Expr = p.parseExpr()
	// right brace should be checked here.
	if setRbrace && p.tok.Type == lexer.RightBrace {
		ifMatch.RightBrace = p.tok
		p.next()
	}
	p.shouldInsertDelimBefore(true, lexer.Comma)
	if p.tok.Type == lexer.Comma {
		// fmt.Println("comma") // insert line terminator after?
		patCase.Comma = p.tok
		p.next()
	}
	p.shouldInsertDelimAfter(false)
	p.shouldInsertDelimBefore(false)
	if p.tok.Type == lexer.LineTerminator && ifMatch.RightBrace.Type != lexer.RightBrace {
		if p.peek().Type == lexer.Or {
			p.next()
		}
	}
	return &patCase
}

func (p *parser) parseGuard() ast.Node {
	defer p.trace("parseGuard")()
	guard := ast.IfHeader{If: p.tok}
	p.next()
	guard.Cond = p.parseTuple()
	return &guard
}

func (p *parser) parseTag() ast.Node {
	defer p.trace("parseTag")()
	var tag ast.Node = &ast.Ident{Name: p.tok}
	p.next()
	for p.tok.Type == lexer.Period {
		period := p.tok
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("missing identifier")
		}
		ident := ast.Ident{Name: p.tok}
		p.next()
		tag = &ast.SelectorExpr{
			X:      tag,
			Period: period,
			Name:   &ident,
		}
	}
	return tag
}

// Pattern = Literal | Ident | TuplePattern
// | Pattern "|" Pattern | Constructor [ Pattern ]
// | Pattern ":" TypeBody
// NestedPattern = Pattern | Ident "=" Expr
// TuplePattern = "(" { NestedPattern [ "," ] } ")"
func (p *parser) parsePattern() ast.Node {
	defer p.trace("parsePattern")()
	var tag ast.Node
	var pat ast.Node
	if p.tok.Type == lexer.Ident {
		tag = p.parseTag()
	}
	switch p.tok.Type {
	case lexer.Ident:
		pat = &ast.Ident{Name: p.tok}
		p.next()
	case lexer.Number:
		pat = &ast.Number{Lit: p.tok}
		p.next()
	case lexer.StringBeg, lexer.String:
		pat = p.parseString()
	case lexer.LeftParen:
		pat = p.parseTuplePattern()
	}
	if tag != nil {
		if pat != nil {
			pat = &ast.CallExpr{
				Elements: []ast.Node{tag, pat},
			}
		} else {
			pat = tag
		}
	}
	if p.tok.Type == lexer.Colon {
		colon := p.tok
		p.next()
		typ := p.parseTypeBody(false, false) // do we want to allow patterns like Map with ('k = int)
		pat = &ast.TypeAnnotation{
			Destructure: pat,
			Colon:       colon,
			Type:        typ, // is the type part of the patten allowed to introduce a new binding here?
		}
	}
	if p.tok.Type == lexer.Or {
		or := p.tok
		p.next()
		pat = &ast.BinaryExpr{
			Left:  pat,
			Op:    or,
			Right: p.parsePattern(),
		}
	}
	return pat
}

func (p *parser) parseTuplePattern() ast.Node {
	defer p.trace("parseTuplePattern")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple ast.Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		if p.tok.Type == lexer.Ident && p.peek().Type == lexer.Assign {
			id := p.tok
			p.next()
			elem.X = &ast.BinaryExpr{
				Left:  &ast.Ident{Name: id},
				Op:    p.tok,
				Right: p.parseExpr(),
			}
		} else {
			elem.X = p.parsePattern()
		}
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &tuple
}

func (p *parser) parseArray() ast.Node {
	defer p.trace("parseArray")()
	// Should we support a => b syntax for array elements?
	// or maybe a: b syntax?
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var arr ast.Array
	arr.LeftBracket = p.tok
	p.next()
	for p.tok.Type != lexer.RightBracket && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseExpr()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		arr.Elements = append(arr.Elements, &elem)
	}
	if p.tok.Type != lexer.RightBracket {
		panic("missing right bracket")
	}
	arr.RightBracket = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &arr
}

func (p *parser) parseTuple() ast.Node {
	defer p.trace("parseTuple")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple ast.Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseExpr()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &tuple
}

// An Import tuple is a tuple with idents, where each ident can be an IndexExpr,
// and the contents of the index contains comma-delimited idents.
func (p *parser) parseImportTuple() ast.Node {
	defer p.trace("parseImportTuple")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple ast.Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		if p.tok.Type != lexer.Ident {
			panic("expected identifier")
		}
		elem.X = &ast.Ident{Name: p.tok}
		p.next()
		for p.tok.Type == lexer.LeftBracket {
			elem.X = p.parseImportIndexExpr(elem.X)
		}
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &tuple
}

func (p *parser) parseImportIndexExpr(x ast.Node) ast.Node {
	defer p.trace("parseImportIndexExpr")()
	var index ast.IndexExpr
	index.X = x
	index.LeftBracket = p.tok
	p.next()
	for p.tok.Type != lexer.RightBracket && p.tok.Type != lexer.EOF {
		if p.tok.Type != lexer.Ident {
			panic("expected identifier")
		}
		var elem ast.CommaElement
		elem.X = &ast.Ident{Name: p.tok}
		p.next()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		index.IndexElements = append(index.IndexElements, &elem)
	}
	if p.tok.Type != lexer.RightBracket {
		panic("missing right bracket")
	}
	index.RightBracket = p.tok
	p.next()
	return &index
}

func (p *parser) parseTypeDecl() ast.Node {
	defer p.trace("parseTypeDecl")()
	var typeDecl ast.TypeDecl
	typeDecl.Type = p.tok
	p.next()
	if p.tok.Type != lexer.Ident {
		panic("missing identifier")
	}
	typeDecl.Name = &ast.Ident{Name: p.tok}
	p.next()
	for p.tok.Type == lexer.TypeArg {
		typeDecl.TypeParams = append(typeDecl.TypeParams, p.parseNamedTypeArgument())
	}
	if p.tok.Type == lexer.With {
		typeDecl.With = p.tok
		p.next()
		typeDecl.Clause = p.parseWithClause()
	}
	if p.tok.Type != lexer.Assign {
		// allow forward declarations
		return &typeDecl
	}
	typeDecl.Equal = p.tok
	p.next()
	typeDecl.Body = p.parseTypeBody(true, false)
	return &typeDecl
}

// like a type parameter, but with foralls allowed, and no top-level restriction
func (p *parser) parseTupleTypeConstraint(parseAssignment bool) ast.Node {
	defer p.trace("parseTupleTypeConstraint")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	if p.peek().Type == lexer.TypeArg && p.peek2().Type == lexer.Assign {
		if !parseAssignment {
			panic("unexpected assignment")
		}
		// assignment
		var tuple ast.Tuple
		tuple.LeftParen = p.tok
		p.next()
		for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
			var elem ast.CommaElement
			typeArg := p.parseNamedTypeArgument()
			if p.tok.Type != lexer.Assign {
				panic("missing equals in type assignment")
			}
			equals := p.tok
			p.next()
			elem.X = &ast.BinaryExpr{typeArg, equals, p.parseTypeBody(false, false)}
			if p.tok.Type == lexer.Comma {
				elem.Comma = p.tok
				p.next()
			}
			tuple.Elements = append(tuple.Elements, &elem)
		}
		if p.tok.Type != lexer.RightParen {
			panic("missing right paren")
		}
		tuple.RightParen = p.tok
		p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
		p.next()
		return &tuple
	} else {
		p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
		return p.parseTupleType()
	}
}

// 'a or '1
func (p *parser) parseNamedTypeArgument() ast.Node {
	defer p.trace("parseNamedTypeArgument")()
	if p.tok.Type != lexer.TypeArg {
		panic("expected type argument")
	}
	typeArg := p.tok
	p.next()
	return &ast.TypeArg{
		TypeArg: typeArg,
	}
}

func (p *parser) parseTypeApplication(name ast.Node) ast.Node {
	defer p.trace("parseTypeApplication")()
	var typeApp ast.CallExpr
	if name == nil {
		name = p.parseTypeName()
	}
	typeApp.Elements = append(typeApp.Elements, name)
	for beginsAnonType(p.tok.Type) {
		// berry stay mad
		typeApp.Elements = append(typeApp.Elements, p.parseTypeBody(false, true)) // assignments are allowed on things that are applied
	}
	if len(typeApp.Elements) == 1 {
		return typeApp.Elements[0]
	}
	return &typeApp
}

// Type Name can be a SelectorExpr or type parameter, since we could be accessing a type from another package.
func (p *parser) parseTypeName() ast.Node {
	defer p.trace("parseTypeName")()
	if p.tok.Type != lexer.Ident {
		panic("missing identifier")
	}
	var typeName ast.Node = &ast.Ident{Name: p.tok}
	p.next()
	for p.tok.Type == lexer.Period {
		period := p.tok
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("missing identifier")
		}
		ident := ast.Ident{Name: p.tok}
		p.next()
		typeName = &ast.SelectorExpr{
			X:      typeName,
			Period: period,
			Name:   &ident,
		}
	}
	return typeName
}

func (p *parser) parseArrayType() ast.Node {
	defer p.trace("parseArrayType")()
	var array ast.ArrayType
	array.LeftBracket = p.tok
	p.next()
	if p.tok.Type == lexer.Number {
		array.Length = p.tok
		p.next()
	}
	if p.tok.Type != lexer.RightBracket {
		panic("missing right bracket")
	}
	array.RightBracket = p.tok
	p.next()
	array.Type = p.parseTypeBody(false, false)
	return &array
}

func (p *parser) parseTypeBodyWithoutQuestionMark(parseSumType, parseAssignment bool) ast.Node {
	defer p.trace("parseTypeBodyWithoutQuestionMark")()
	// parse forall
	if p.tok.Type == lexer.TypeArg {
		var forall ast.ForallType
		typeArg := p.parseNamedTypeArgument()
		if p.tok.Type == lexer.Period {
			forall.TypeArg = typeArg
			forall.Period = p.tok
			p.next()
			forall.Type = p.parseTypeBody(parseSumType, false) // you can't have an assignment immediately inside a forall
			return &forall
		}
		if beginsAnonType(p.tok.Type) {
			return p.parseTypeApplication(typeArg)
		}
		return typeArg
	}
	switch p.tok.Type {
	case lexer.LeftParen:
		if parseAssignment {
			return p.parseTupleTypeConstraint(parseAssignment)
			// ttc := p.parseTupleTypeConstraint(parseAssignment)
			// if beginsAnonType(p.tok.Type) {
			// 	return p.parseTypeApplication(ttc)
			// } else {
			// 	return ttc
			// }
		}
		return p.parseTupleType()
	case lexer.LeftBracket:
		// Is this allowed inside a type constraint or assignment? I think so.
		// A list type looks like [N]'t, where 't is the element type, and N is an optional constant length.
		// maybe we pass parseConstraint down?
		return p.parseArrayType()
	case lexer.Ident:
		return p.parseTypeApplication(nil)
	case lexer.Fun:
		// if parseConstraint {
		// 	// TODO: i think this is allowed in a type constraint
		// 	panic("function types are not allowed in type constraints")
		// }
		// maybe we pass parseConstraint down?
		return p.parseFunctionType()
	case lexer.Or:
		if parseAssignment {
			panic("sum types are not allowed in type assignments")
		}
		if !parseSumType {
			panic("sum types are only allowed in type declarations")
		}
		return p.parseSumType()
	default:
		panic("missing type body")
	}
}

// TODO: TypeBody should have one sum case, and if it sees a |, it should parse more cases.
// TODO: simplify with clauses to just be a comma-delimited list of type applications of named types and type variables inside tuples. oh and foralls.
func (p *parser) parseTypeBody(parseSumType, parseAssignment bool) ast.Node {
	defer p.trace("parseTypeBody")()
	t := p.parseTypeBodyWithoutQuestionMark(parseSumType, parseAssignment)
	// parse nillable type
	if p.tok.Type == lexer.QuestionMark {
		question := p.tok
		p.next()
		return &ast.NillableType{
			Type:         t,
			QuestionMark: question,
		}
	}
	return t
}

func (p *parser) parseWithClauseTuple() ast.Node {
	defer p.trace("parseWithClauseTuple")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple ast.Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseWithClause()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &tuple
}

func (p *parser) parseWithClauseApplication(name ast.Node) ast.Node {
	defer p.trace("parseWithClauseApplication")()
	var app ast.CallExpr
	app.Elements = append(app.Elements, name)
	for beginsWithElem(p.tok.Type) {
		app.Elements = append(app.Elements, p.parseWithClause())
	}
	return &app
}

// TODO: should trait constraints allow the TraitName ('a = 'b) syntax?
// i.e. should trait constraints allow type assignments?
func (p *parser) parseWithClause() ast.Node {
	defer p.trace("parseWithClause")()
	// A with clause can have
	// - Named types
	// - Type variables
	// - Type applications
	// - Tuples
	// - Foralls
	if p.tok.Type == lexer.TypeArg {
		var forall ast.ForallType
		typeArg := p.parseNamedTypeArgument()
		if p.tok.Type == lexer.Period {
			forall.TypeArg = typeArg
			forall.Period = p.tok
			p.next()
			forall.Type = p.parseWithClause()
			return &forall
		}
		if beginsWithElem(p.tok.Type) {
			return p.parseWithClauseApplication(typeArg)
		}
		return typeArg
	}
	switch p.tok.Type {
	case lexer.LeftParen:
		ttc := p.parseWithClauseTuple()
		if beginsWithElem(p.tok.Type) {
			return p.parseWithClauseApplication(ttc)
		}
		return ttc
	case lexer.Ident:
		name := p.parseTypeName()
		if beginsWithElem(p.tok.Type) {
			return p.parseWithClauseApplication(name)
		}
		return name
	default:
		panic("missing type body")
	}
}

func (p *parser) parseSumType() ast.Node {
	defer p.trace("parseSumType")()
	var sum ast.SumType
	for p.tok.Type == lexer.Or {
		var sumElem ast.SumTypeElement
		sumElem.Or = p.tok
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("missing identifier")
		}
		sumElem.Name = &ast.Ident{Name: p.tok}
		p.next()
		if beginsAnonType(p.tok.Type) {
			sumElem.Type = p.parseTypeBody(false, false)
		}
		sum.Elements = append(sum.Elements, &sumElem)
	}
	return &sum
}

func beginsAnonType(ttype lexer.TokenType) bool {
	switch ttype {
	case lexer.LeftParen, lexer.TypeArg, lexer.Ident, lexer.Fun, lexer.LeftBracket:
		return true
	}
	return false
}

func beginsWithElem(ttype lexer.TokenType) bool {
	switch ttype {
	case lexer.LeftParen, lexer.TypeArg, lexer.Ident:
		return true
	}
	return false
}

// "=" is for default values.
func (p *parser) parseTupleType() ast.Node {
	defer p.trace("parseTupleType")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	lpar := p.tok
	p.next()
	var elems []ast.Node
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		p.shouldInsertDelimAfter(true, lexer.TypeArg)
		switch p.tok.Type {
		case lexer.Ident:
			// Could be a field name or a named type.
			if t := p.peek(); t.Type == lexer.Colon {
				// Field name.
				var field ast.Field
				field.Name = &ast.Ident{Name: p.tok}
				p.next()
				field.Colon = p.tok
				p.next()
				field.Type = p.parseTypeBody(false, false)
				if p.tok.Type == lexer.Assign {
					field.Equals = p.tok
					p.next()
					field.Default = p.parseExpr()
				}
				elem.X = &field
			} else {
				elem.X = p.parseTypeBody(false, false)
			}
		default:
			elem.X = p.parseTypeBody(false, false)
		}
		p.shouldInsertDelimAfter(false)
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		if p.tok.Type == lexer.LineTerminator {
			p.next()
		}
		elems = append(elems, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	rpar := p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &ast.Tuple{
		LeftParen:  lpar,
		Elements:   elems,
		RightParen: rpar,
	}
}

func (p *parser) parseFunctionType() ast.Node {
	defer p.trace("parseFunctionType")()
	var fun ast.FunctionType
	fun.Fun = p.tok
	p.next()
	fun.Param = p.parseTypeBody(false, false)
	for p.tok.Type == lexer.RightArrow {
		var arrow ast.Arrow
		arrow.Arrow = p.tok
		p.next()
		arrow.Type = p.parseTypeBody(false, false)
		fun.Arrows = append(fun.Arrows, &arrow)
	}
	return &fun
}

func (p *parser) parseImportPath() *ast.BasicString {
	defer p.trace("parseImportPath")()
	path := p.parseString()
	if path, ok := path.(*ast.BasicString); ok {
		return path
	}
	panic("invalid import path")
}

func (p *parser) parseImportDeclPackage() ast.Node {
	defer p.trace("parseImportDeclPackage")()
	var importDecl ast.ImportDeclPackage
	switch p.tok.Type {
	case lexer.String:
		path := p.parseImportPath()
		importDecl = ast.ImportDeclPackage{Path: path}
	case lexer.Ident:
		alias := p.tok
		p.next()
		if p.tok.Type != lexer.Assign {
			panic("missing equals")
		}
		equals := p.tok
		p.next()
		if p.tok.Type != lexer.String {
			panic("missing string")
		}
		path := p.parseImportPath()
		importDecl = ast.ImportDeclPackage{
			Binding: &ast.Ident{Name: alias},
			Equals:  equals,
			Path:    path,
		}
	case lexer.LeftParen:
		// tuple of identifiers
		// tup := p.parseIdentTuple()
		tup := p.parseImportTuple()
		if p.tok.Type != lexer.Assign {
			panic("missing equals")
		}
		equals := p.tok
		p.next()
		if p.tok.Type != lexer.String {
			panic("missing string")
		}
		path := p.parseImportPath()
		importDecl = ast.ImportDeclPackage{
			Binding: tup,
			Equals:  equals,
			Path:    path,
		}
	default:
		panic("invalid import declaration")
	}
	// unquote import path
	// TODO: make this more robust
	importPath, err := strconv.Unquote(importDecl.Path.Lit.Data)
	if err != nil {
		panic(fmt.Errorf("invalid import path: %v", err))
	}
	p.imports[importPath] = struct{}{}

	return &importDecl
}

func (p *parser) parseImportDeclBlock() ast.Node {
	// essentially a tuple of import declarations
	defer p.trace("parseImportDeclBlock")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple ast.Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem ast.CommaElement
		elem.X = p.parseImportDeclPackage()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		if p.tok.Type == lexer.LineTerminator {
			p.next()
		}
		tuple.Elements = append(tuple.Elements, &elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return &tuple
}

func (p *parser) parseImportDecl() ast.Node {
	// import "github.com/someone/math"
	// import alias = "github.com/someone/math"
	// import Sqrt, Abs from "github.com/someone/math"
	// import (
	// 	"github.com/someone/math"
	// 	alias = "github.com/someone/math"
	// 	Sqrt, Abs from "github.com/someone/math"
	// )
	defer p.trace("parseImportDecl")()
	var importDecl ast.ImportDecl
	importDecl.Import = p.tok
	p.next()
	if p.tok.Type == lexer.LeftParen {
		switch p.peek().Type {
		case lexer.Ident:
			switch p.peek2().Type {
			case lexer.Comma, lexer.RightParen, lexer.LeftBracket:
				importDecl.Package = p.parseImportDeclPackage()
				return &importDecl
			}
		}
		importDecl.Package = p.parseImportDeclBlock()
		return &importDecl
	}
	importDecl.Package = p.parseImportDeclPackage()
	return &importDecl
}

func (p *parser) parseImplDecl() ast.Node {
	defer p.trace("parseImplDecl")()
	impl := ast.ImplDecl{Impl: p.tok}
	p.next()
	impl.Name = p.parseTypeName()
	impl.Args = p.parseTypeBody(false, false)
	if p.tok.Type == lexer.With {
		impl.With = p.tok
		p.next()
		impl.Clause = p.parseWithClause()
	}
	if p.tok.Type == lexer.Assign {
		impl.Equals = p.tok
		p.next()
		impl.Body = p.parseExpr()
	}
	return &impl
}

func (p *parser) parseExprOrStmt() ast.Node {
	defer p.trace("parseExprOrStmt")()
	switch p.tok.Type {
	case lexer.Import:
		return p.parseImportDecl()
	case lexer.Let:
		return p.parseLetDecl()
	case lexer.Var:
		return p.parseVarDecl()
	case lexer.Type:
		return p.parseTypeDecl()
	case lexer.Semicolon, lexer.LineTerminator:
		return &ast.EmptyExpr{}
	default:
		return p.parseExpr()
	}
}

func (p *parser) parseTopLevelDeclaration() ast.Node {
	defer p.trace("parseTopLevelDeclaration")()
	switch p.tok.Type {
	case lexer.Import:
		// TODO: imports should only be at the top?
		return p.parseImportDecl()
	case lexer.Let:
		return p.parseLetDecl()
	case lexer.Type:
		return p.parseTypeDecl()
	case lexer.Impl:
		return p.parseImplDecl()
	case lexer.Semicolon, lexer.LineTerminator:
		return &ast.EmptyExpr{}
	case lexer.Fun:
		return p.parseFun(true)
	}
	panic("invalid top level declaration")
}

func (p *parser) parseBody(topLevel bool, until lexer.TokenType) *ast.Block {
	defer p.trace("parseBody")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var body []ast.Node
	for p.tok.Type != until && p.tok.Type != lexer.EOF {
		var n ast.Node
		if topLevel {
			n = p.parseTopLevelDeclaration()
		} else {
			n = p.parseExprOrStmt()
		}
		if p.tok.Type == lexer.Semicolon || p.tok.Type == lexer.LineTerminator {
			n = &ast.Stmt{
				Stmt:       n,
				Terminator: p.tok,
			}
			p.next()
		}
		body = append(body, n)
	}
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	return &ast.Block{Body: body}
}
