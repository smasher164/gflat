package parser

import (
	"fmt"

	"github.com/smasher164/gflat/lexer"
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

// func (p *parser) tok() lexer.Token {
// 	return p.buf[p.tokIndex]
// }

// func (p *parser) leadingTrivia() []lexer.Token {
// 	return p.buf[p.leadingTriviaIndex:p.tokIndex]
// }

// func (p *parser) next() {
// 	if len(p.buf) > 0 && p.tok().Type == lexer.EOF {
// 		return
// 	}
// 	var t lexer.Token
// 	li := len(p.buf)
// 	for t = p.l.Next(); t.Type == lexer.Whitespace || t.Type == lexer.SingleLineComment; t = p.l.Next() {
// 		p.buf = append(p.buf, t)
// 	}
// 	ti := len(p.buf)
// 	p.buf = append(p.buf, t)
// 	p.leadingTriviaIndex = li
// 	p.tokIndex = ti
// }

// func (p *parser) parseFile() File {
// 	var f File
// 	p.next()
// 	f.leadingTrivia = p.leadingTrivia()
// 	if t := p.tok(); t.Type == lexer.Package {
// 		f.PackageDecl = new(PackageDecl)
// 		f.PackageDecl.Package = t
// 		p.next()
// 		t := p.tok()
// 		trivia := p.leadingTrivia()
// 		if t.Type == lexer.Ident {
// 			f.PackageDecl.Name = Ident{
// 				IsNode: IsNode{
// 					leadingTrivia: trivia,
// 				},
// 				Name: t,
// 			}
// 		} else {
// 			f.PackageDecl.Name = Illegal{
// 				IsNode: IsNode{
// 					leadingTrivia: trivia,
// 				},
// 				Msg: "expected identifier after package",
// 			}
// 		}
// 	}
// 	return f
// }

func ParseFile(l Lexer) Node {
	return (&parser{l: l}).parseFile()
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

func (p *parser) parseFile() (f File) {
	defer p.trace("parseFile")()
	p.next()
	if p.tok.Type == lexer.Package {
		f.Package = p.tok
		p.next()
		if p.tok.Type == lexer.Ident {
			f.PackageName = Ident{Name: p.tok}
		} else {
			// do I need to give it a span?
			f.PackageName = Illegal{Msg: "expected identifier after package"}
		}
		p.next()
	}
	if p.tok.Type == lexer.LineTerminator {
		p.next()
	}
	f.Body = p.parseBody(lexer.EOF)
	if p.tok.Type == lexer.EOF {
		f.trailingTrivia = p.tok.LeadingTrivia
	}
	return f
}

// TODO: rework LetDecl to handle function and :=
// LetDecl = "let" RestDecl
// TOOD: add let f x = x syntax
func (p *parser) parseLetDecl() Node {
	defer p.trace("parseLetDecl")()
	letTok := p.tok
	p.next()
	if p.tok.Type == lexer.Ident {
		switch p.peek().Type {
		case lexer.Ident, lexer.LeftParen:
			var letFun LetFunction
			letFun.Name = Ident{Name: p.tok}
			p.next()
			letFun.Signature = p.parseFunctionSignature()
			if p.tok.Type == lexer.LineTerminator && p.peek().Type == lexer.LeftBrace {
				p.next()
			} else if p.tok.Type == lexer.Equals {
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
	decl := LetDecl{Let: letTok}
	decl.Destructure = p.parseDestructure()
	if isIllegal(decl.Destructure) {
		return decl.Destructure
	}
	if p.tok.Type != lexer.Equals {
		if p.tok.Type == lexer.FatArrow {
			panic("expected = or { after function signature")
		}
		return decl
		// return Illegal{Node: decl.Destructure, Msg: "expected = after let declaration"}
	}
	decl.Equals = p.tok
	p.next()
	decl.Rhs = p.parseExpr()
	return decl
}

func isIllegal(n Node) bool {
	_, ok := n.(Illegal)
	return ok
}

// VarDecl = "var" RestDecl
func (p *parser) parseVarDecl() Node {
	defer p.trace("parseVarDecl")()
	decl := VarDecl{Var: p.tok}
	p.next()
	decl.Destructure, decl.Equals, decl.Rhs = p.restDecl()
	if isIllegal(decl.Destructure) {
		return decl.Destructure
	}
	return decl
}

// RestDecl = Destructure "=" Expr
func (p *parser) restDecl() (lhs Node, equals lexer.Token, rhs Node) {
	defer p.trace("restDecl")()
	lhs = p.parseDestructure()
	if isIllegal(lhs) {
		return lhs, lexer.Token{}, nil
	}
	if p.tok.Type != lexer.Equals {
		return Illegal{Node: lhs, Msg: "expected = after let declaration"}, lexer.Token{}, nil
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
func (p *parser) parseFunctionSignature() FunctionSignature {
	defer p.trace("parseFunctionSignature")()
	var fun FunctionSignature
	if p.tok.Type == lexer.Ident {
		var param Param
		param.Name = Ident{Name: p.tok}
		p.next()
		if p.tok.Type == lexer.Colon {
			param.Colon = p.tok
			p.next()
			param.Type = p.parseTypeBodyWithoutWhere(false, false, false)
		}
		fun.Param = param
	} else if p.tok.Type == lexer.LeftParen {
		fun.Param = p.parseTupleType()
	} else {
		panic("expected ident or tuple type for function signature")
	}
	for p.tok.Type == lexer.RightArrow {
		var arrow Arrow
		arrow.Arrow = p.tok
		p.next()
		arrow.Type = p.parseTypeBodyWithoutWhere(false, false, false)
		fun.Arrows = append(fun.Arrows, arrow)
	}
	if p.tok.Type == lexer.Where {
		fun.Where = p.tok
		p.next()
		fun.Clause = p.parseTypeBody(false, true, false)
	}
	return fun
}

func (p *parser) parseFun() Node {
	defer p.trace("parseFun")()
	fun := Function{Fun: p.tok}
	p.next()
	if p.tok.Type == lexer.Ident {
		switch p.peek().Type {
		case lexer.Ident, lexer.LeftParen:
			fun.Name = Ident{Name: p.tok}
			p.next()
		}
	}
	fun.Signature = p.parseFunctionSignature()
	if p.tok.Type == lexer.LineTerminator && p.peek().Type == lexer.LeftBrace {
		p.next()
	} else if p.tok.Type == lexer.FatArrow {
		fun.FatArrow = p.tok
		p.next()
	} else if p.tok.Type != lexer.LeftBrace {
		if p.tok.Type == lexer.Equals {
			panic("expected => or { after function signature")
		}
		// handle forward declarations
		return fun
		// panic("expected => or { after function signature")
	}
	fun.Body = p.parseExpr()
	return fun
}

func (p *parser) parseOperand() Node {
	defer p.trace("parseOperand")()
	switch tok := p.tok; tok.Type {
	case lexer.Fun:
		return p.parseFun()
	case lexer.If:
		return p.parseIf()
	case lexer.LeftParen:
		return p.parseTuple()
	// case lexer.LeftBracket:
	// 	panic("TODO: parseIndex")
	case lexer.LeftBrace:
		p.next()
		block := p.parseBody(lexer.RightBrace)
		if p.tok.Type != lexer.RightBrace {
			panic("expected }")
		}
		block.LeftBrace = tok
		block.RightBrace = p.tok
		p.next()
		return block
	case lexer.Ident:
		p.next()
		return Ident{Name: tok}
	case lexer.Number:
		p.next()
		return Number{Lit: tok}
	case lexer.StringBeg, lexer.String:
		return p.parseString()
	}
	res := Illegal{span: p.tok.Span, Msg: "expected operand"}
	p.next()
	return res
}

func (p *parser) parseString() Node {
	defer p.trace("parseString")()
	switch tok := p.tok; tok.Type {
	case lexer.StringBeg:
		var str String
		str.Parts = append(str.Parts, StringPart{Lit: p.tok})
		p.next()
		for p.tok.Type != lexer.StringEnd && p.tok.Type != lexer.EOF {
			str.Parts = append(str.Parts, p.parseExpr())
			if p.tok.Type == lexer.StringPart {
				str.Parts = append(str.Parts, StringPart{Lit: p.tok})
				p.next()
			} else if p.tok.Type != lexer.StringEnd {
				panic("expected string part or end")
			}
		}
		if p.tok.Type != lexer.StringEnd {
			panic("expected string end")
		}
		str.Parts = append(str.Parts, StringPart{Lit: p.tok})
		p.next()
		return str
	case lexer.String:
		p.next()
		return String{Parts: []Node{StringPart{Lit: tok}}}
	}
	panic("expected string")
}

func (p *parser) parsePrimaryExpr() Node {
	defer p.trace("parsePrimaryExpr")()
	x := p.parseOperand()
	switch op := p.tok; {
	case op.IsPostfixOp():
		// if it's DotDot and the next token is the start of an expression, just return the x
		if op.Type == lexer.DotDot && p.peek().BeginsPrefixExpr() {
			return x
		}
		p.next()
		return PostfixExpr{X: x, Op: op}
	case op.Type == lexer.Period:
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("expected ident after .")
		}
		id := p.tok
		p.next()
		return SelectorExpr{X: x, Period: op, Name: Ident{Name: id}}
	case op.Type == lexer.LeftBracket:
		shouldInsert := p.shouldInsertAfter
		toksToCheck := p.afterToksToCheck
		p.shouldInsertDelimAfter(false)
		p.next()
		var index IndexExpr
		i := p.parseExpr()
		if p.tok.Type != lexer.RightBracket {
			panic("expected ]")
		}
		index.LeftBracket = p.tok
		index.X = x
		index.Index = i
		index.RightBracket = p.tok
		p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
		p.next()
		return index
	}
	return x
}

func (p *parser) parsePrefixExpr() Node {
	defer p.trace("parsePrefixExpr")()
	if p.tok.IsPrefixOp() {
		op := p.tok
		p.next()
		return PrefixExpr{Op: op, X: p.parsePrefixExpr()}
	}
	return p.parsePrimaryExpr()
}

func (p *parser) parseCallExpr() Node {
	defer p.trace("parseCallExpr")()
	exprs := []Node{p.parsePrefixExpr()}
	for p.tok.BeginsArgumentExpr() {
		exprs = append(exprs, p.parsePrefixExpr())
	}
	if len(exprs) == 1 {
		return exprs[0]
	}
	return CallExpr{Elements: exprs}
}

func (p *parser) parseBinaryExpr(minPrec int) Node {
	defer p.trace("parseBinaryExpr")()
	res := p.parseCallExpr()
	for p.tok.IsBinaryOp() && p.tok.Prec() >= minPrec {
		op := p.tok
		p.next()
		nextMinPrec := op.Prec()
		if op.IsLeftAssoc() {
			nextMinPrec++
		}
		var rhs Node
		switch op.Type {
		case lexer.Colon:
			rhs = p.parseTypeBody(false, false, false)
		case lexer.Where:
			rhs = p.parseWhereClause()
		default:
			rhs = p.parseBinaryExpr(nextMinPrec)
		}
		res = filterBinExp(BinaryExpr{Left: res, Op: op, Right: rhs})
	}
	return res
}

// func (p *parser) parseTypeAssignment() Node {
// 	defer p.trace("parseTypeAssignment")()
// 	if p.tok.Type != lexer.SingleQuote {
// 		return p.parseTypeBody(false, false)
// 	}
// 	typeArg := p.parseNamedTypeArgument()
// 	if p.tok.Type != lexer.Equals {
// 		var forall ForallType
// 		if p.tok.Type == lexer.Period {
// 			forall.TypeArg = typeArg
// 			forall.Period = p.tok
// 			p.next()
// 			forall.Type = p.parseTypeBody(false, false)
// 			return forall
// 		}
// 		return p.parseTypeApplication(typeArg, false)
// 	}
// 	var ta BinaryExpr
// 	ta.Left = typeArg
// 	ta.Op = p.tok
// 	p.next()
// 	ta.Right = p.parseTypeBody(false, false)
// 	return ta
// }

// parses the expression where clause where you can specialize types in an expression.
// e.g. foo (2, 3) where 'a = int or foo (2, 3) where ('a = int, 'b = int)
func (p *parser) parseWhereClause() Node {
	defer p.trace("parseWhereClause")()
	return p.parseTypeBody(false, false, true)

	// if p.tok.Type == lexer.SingleQuote {
	// 	return p.parseTypeAssignment()
	// }
	// if p.tok.Type != lexer.LeftParen {
	// 	panic("expected (")
	// }
	// shouldInsert := p.shouldInsertAfter
	// toksToCheck := p.afterToksToCheck
	// p.shouldInsertDelimAfter(false)
	// var tuple Tuple
	// tuple.LeftParen = p.tok
	// p.next()
	// for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
	// 	var elem TupleElement
	// 	elem.X = p.parseTypeAssignment()
	// 	if p.tok.Type == lexer.Comma {
	// 		elem.Comma = p.tok
	// 		p.next()
	// 	}
	// 	tuple.Elements = append(tuple.Elements, elem)
	// }
	// if p.tok.Type != lexer.RightParen {
	// 	panic("missing right paren")
	// }
	// tuple.RightParen = p.tok
	// p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	// p.next()
	// return tuple
}

func getBinExp(x Node) *BinaryExpr {
	switch x := x.(type) {
	case BinaryExpr:
		return &x
	case Illegal:
		return getBinExp(x.Node)
	default:
		return nil
	}
}

func filterBinExp(x Node) Node {
	top, ok := x.(BinaryExpr)
	if !ok {
		return x
	}
	if leftbe := getBinExp(top.Left); leftbe != nil {
		if !leftbe.Op.BinaryInteroperable(top.Op) {
			return Illegal{
				Node: x,
				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", leftbe.Op.Type, top.Op.Type),
			}
		}
	}
	if rightbe := getBinExp(top.Right); rightbe != nil {
		if !rightbe.Op.BinaryInteroperable(top.Op) {
			return Illegal{
				Node: x,
				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", top.Op.Type, rightbe.Op.Type),
			}
		}
	}
	return x
}

// filterBinExp checks that x is a BinaryExpr, and if so
// whether the left and right subexpressions are interoperable.
// if not, it wraps it in an Illegal.
// invalid binary expressions are
// func filterBinExp(x Node) Node {
// 	top, ok := x.(BinaryExpr)
// 	if !ok {
// 		return x
// 	}
// 	if leftbe, ok := top.Left.(BinaryExpr); ok {
// 		if !leftbe.Op.BinaryInteroperable(top.Op) {
// 			return Illegal{
// 				Node: x,
// 				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", leftbe.Op.Type, top.Op.Type),
// 			}
// 		}
// 	}
// 	if rightbe, ok := top.Right.(BinaryExpr); ok {
// 		if !rightbe.Op.BinaryInteroperable(top.Op) {
// 			return Illegal{
// 				Node: x,
// 				Msg:  fmt.Sprintf("cannot mix operators %q and %q in binary expression, consider adding parentheses", top.Op.Type, rightbe.Op.Type),
// 			}
// 		}
// 	}
// 	return x
// }

func (p *parser) parseExpr() Node {
	defer p.trace("parseExpr")()
	return p.parseBinaryExpr(lexer.MinPrec)
}

// Destructure =
//
//	| ident
//	| Destructure ":" TypeBody
//	| "(" Destructure ")"
//	| "(" Destructure "," Destructure ")";
func (p *parser) parseDestructure() Node {
	defer p.trace("parseDestructure")()
	var des Node
	switch p.tok.Type {
	case lexer.Ident:
		des = Ident{Name: p.tok}
		p.next()
	case lexer.LeftParen:
		des = p.parseTupleDestructure()
	default:
		panic("missing comma or right paren")
	}
	if p.tok.Type == lexer.Colon {
		colon := p.tok
		p.next()
		typ := p.parseTypeBody(false, false, false)
		des = TypeAnnotation{
			Destructure: des,
			Colon:       colon,
			Type:        typ,
		}
	}
	return des
}

// TupleDestructure = "(" Destructure ")"
// TupleDestructure = "(" Destructure "," Destructure ")"
func (p *parser) parseTupleDestructure() Node {
	defer p.trace("parseTupleDestructure")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	leftParen := p.tok
	p.next()
	var elems []Node
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		elem.X = p.parseDestructure()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		} else if p.tok.Type != lexer.RightParen {
			panic("missing comma or right paren")
		}
		elems = append(elems, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	rightParen := p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return Tuple{
		LeftParen:  leftParen,
		Elements:   elems,
		RightParen: rightParen,
	}
}

// IfHeader = "if" "(" Expr ")"
// IfElse = IfHeader Expr [ "else" Expr ]
// IfMatch = IfHeader ( "|" Pattern "=>" Expr )+
// If = IfElse | IfMatch
func (p *parser) parseIf() Node {
	defer p.trace("parseIf")()
	ifHeader := IfHeader{If: p.tok}
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
		return IfElse{
			IfHeader: ifHeader,
			Body:     body,
			Else:     elseTok,
			ElseBody: elseBody,
		}
	}
	return If{
		IfHeader: ifHeader,
		Body:     body,
	}
}

func (p *parser) parseIfMatch(ifHeader IfHeader) Node {
	defer p.trace("parseIfMatch")()
	var ifMatch IfMatch
	var setRbrace bool
	if p.tok.Type == lexer.LeftBrace {
		ifMatch.LeftBrace = p.tok
		setRbrace = true
		p.next()
	}
	var cases []Node
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
	ifMatch.IfHeader = ifHeader
	ifMatch.Cases = cases
	return ifMatch
}

func (p *parser) parseCase(ifMatch *IfMatch, setRbrace bool) Node {
	defer p.trace("parseCase")()
	var patCase PatternCase
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
	return patCase
}

func (p *parser) parseGuard() Node {
	defer p.trace("parseGuard")()
	guard := IfHeader{If: p.tok}
	p.next()
	guard.Cond = p.parseTuple()
	return guard
}

// Pattern = Literal | Ident | TuplePattern
// | Pattern "|" Pattern | Constructor [ Pattern ]
// | Pattern ":" TypeBody
// NestedPattern = Pattern | Ident "=" Expr
// TuplePattern = "(" { NestedPattern [ "," ] } ")"
func (p *parser) parsePattern() Node {
	defer p.trace("parsePattern")()
	var tag Node
	var pat Node
	if p.tok.Type == lexer.Ident {
		tag = Ident{Name: p.tok}
		p.next()
	}
	switch p.tok.Type {
	case lexer.Ident:
		pat = Ident{Name: p.tok}
		p.next()
	case lexer.Number:
		pat = Number{Lit: p.tok}
		p.next()
	case lexer.StringBeg, lexer.String:
		pat = p.parseString()
	case lexer.LeftParen:
		pat = p.parseTuplePattern()
	}
	if tag != nil {
		if pat != nil {
			pat = CallExpr{
				Elements: []Node{tag, pat},
			}
		} else {
			pat = tag
		}
	}
	if p.tok.Type == lexer.Colon {
		colon := p.tok
		p.next()
		typ := p.parseTypeBody(false, false, false)
		pat = TypeAnnotation{
			Destructure: pat,
			Colon:       colon,
			Type:        typ,
		}
	}
	if p.tok.Type == lexer.Or {
		or := p.tok
		p.next()
		pat = BinaryExpr{
			Left:  pat,
			Op:    or,
			Right: p.parsePattern(),
		}
	}
	return pat
}

func (p *parser) parseTuplePattern() Node {
	defer p.trace("parseTuplePattern")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		if p.tok.Type == lexer.Ident && p.peek().Type == lexer.Equals {
			id := p.tok
			p.next()
			elem.X = BinaryExpr{
				Left:  Ident{Name: id},
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
		tuple.Elements = append(tuple.Elements, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return tuple
}

func (p *parser) parseTuple() Node {
	defer p.trace("parseTuple")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		elem.X = p.parseExpr()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return tuple
}

func (p *parser) parseIdentTuple() Node {
	defer p.trace("parseIdentTuple")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		if p.tok.Type != lexer.Ident {
			panic("expected identifier")
		}
		elem.X = Ident{Name: p.tok}
		p.next()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return tuple
}

// func (p *parser) parseIfMatch() []IfMatchCase {
// 	var cases []IfMatchCase
// 	for p.tok.Type == lexer.Or {
// 		cases = append(cases, p.parseIfMatchCase())
// 	}
// 	return cases
// }

// func (p *parser) parseIfMatchCase() IfMatchCase {
// 	or := p.tok
// 	p.next()
// 	pattern := p.parsePattern()
// 	if p.tok.Type != lexer.FatArrow {
// 		// missing fat arrow
// 	}
// 	arrow := p.tok
// 	p.next()
// 	body := p.parseExpr()
// 	return IfMatchCase{
// 		Or:    or,
// 		Pat:   pattern,
// 		Arrow: arrow,
// 		Body:  body,
// 	}
// }

/*
TypeDecl = "type" ident [ { TypeParameter } ] "=" TypeBody
TypeParameter = NamedTypeParameter | "(" ConstrainedTypeParameter [ "," ] { ConstrainedTypeParameter [ "," ] } ")"
NamedTypeParameter = "'" ( nat | ident )
ConstrainedTypeParameter = NamedTypeParameter [ ":" NamedType ]

NamedType = ident [ { TypeArguments } ]
TypeArguments = TypeArgument | "(" TypeArgument [ "," ] { TypeArgument [ "," ] } ")"
TypeArgument = NamedTypeParameter | ConstrainedTypeParameter |  NamedType

FunctionType = "fun" TypeBody ":" TypeBody

SumType = { "|" ident [ TupleType ]  }
TupleType = "(" [ TupleElement [ "," ] { TupleElement [ "," ] } ] ")"
TupleElement = [ ident ":" ] TypeBody

TypeBody = SumType | TupleType | NamedType | FunctionType
*/
func (p *parser) parseTypeDecl() Node {
	defer p.trace("parseTypeDecl")()

	typeTok := p.tok
	p.next()
	if p.tok.Type != lexer.Ident {
		panic("missing identifier")
	}
	typeName := Ident{Name: p.tok}
	p.next()
	var typeParams []Node
	for p.tok.Type == lexer.Ident || p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.TypeArg {
		typeParams = append(typeParams, p.parseTypeParameter(true))
	}
	if p.tok.Type != lexer.Equals {
		// allow forward declarations
		return TypeDecl{
			Type:       typeTok,
			Name:       typeName,
			TypeParams: typeParams,
		}
	}
	equals := p.tok
	p.next()
	typeBody := p.parseTypeBody(true, false, false)
	return TypeDecl{
		Type:       typeTok,
		Name:       typeName,
		TypeParams: typeParams,
		Equal:      equals,
		Body:       typeBody,
	}
}

// like a type parameter, but with foralls allowed, and no top-level restriction
func (p *parser) parseTupleTypeConstraint(parseAssignment bool) Node {
	defer p.trace("parseTupleTypeConstraint")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	if p.peek().Type == lexer.TypeArg && p.peek2().Type == lexer.Equals {
		if !parseAssignment {
			panic("unexpected assignment")
		}
		// assignment
		var tuple Tuple
		tuple.LeftParen = p.tok
		p.next()
		for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
			var elem TupleElement

			// 			equals := p.tok
			// p.next()
			// return BinaryExpr{typeArg, equals, p.parseTypeBody(parseSumType, parseConstraint, false)}
			// }

			// if p.tok.Type == lexer.Period {
			// 	forall.TypeArg = typeArg
			// 	forall.Period = p.tok
			// 	p.next()
			// 	forall.Type = p.parseTypeBody(parseSumType, parseConstraint, false)
			// 	return forall
			// }
			// if p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident || p.tok.Type == lexer.Fun {
			// 	return p.parseTypeApplication(typeArg, parseConstraint, parseAssignment)
			// }
			// return typeArg
			typeArg := p.parseNamedTypeArgument()
			if p.tok.Type != lexer.Equals {
				panic("missing equals in type assignment")
			}
			equals := p.tok
			p.next()
			elem.X = BinaryExpr{typeArg, equals, p.parseTypeBody(false, true, false)}

			// if p.tok.Type == lexer.SingleQuote {

			// 	if p.tok.Type == lexer.Period {
			// 		var forall ForallType
			// 		forall.TypeArg = typeArg
			// 		forall.Period = p.tok
			// 		p.next()
			// 		forall.Type = p.parseTypeBody(false, true, false)
			// 		elem.X = forall
			// 	} else if p.tok.Type == lexer.Equals {
			// 		equals := p.tok
			// 		p.next()
			// 		elem.X = BinaryExpr{typeArg, equals, p.parseTypeBody(false, true, false)}
			// 	} else if p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident || p.tok.Type == lexer.Fun {
			// 		elem.X = p.parseTypeApplication(typeArg, true, false)
			// 	} else {
			// 		elem.X = typeArg
			// 	}
			// } else {
			// 	elem.X = p.parseTypeBody(false, true, false)
			// }
			// elem.X = p.parseTypeBody(false, true, parseAssignment)
			if p.tok.Type == lexer.Comma {
				elem.Comma = p.tok
				p.next()
			}
			tuple.Elements = append(tuple.Elements, elem)
		}
		if p.tok.Type != lexer.RightParen {
			panic("missing right paren")
		}
		tuple.RightParen = p.tok
		p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
		p.next()
		return tuple
	} else {
		p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
		return p.parseTupleType()
	}
	//	defer p.trace("parseTupleTypeConstraint")()
	//	shouldInsert := p.shouldInsertAfter
	//	toksToCheck := p.afterToksToCheck
	//	p.shouldInsertDelimAfter(false)
	//	var tuple Tuple
	//	if expectParen {
	//		if p.tok.Type != lexer.LeftParen {
	//			panic("missing left paren")
	//		}
	//		tuple.LeftParen = p.tok
	//		p.next()
	//	}
	//	if p.tok.Type == lexer.SingleQuote {
	//		var forall ForallType
	//		typeArg := p.parseNamedTypeArgument()
	//		if p.tok.Type == lexer.Period {
	//			forall.TypeArg = typeArg
	//			forall.Period = p.tok
	//			p.next()
	//			forall.Type = p.parseTupleTypeConstraint(false)
	//			tuple.Elements = append(tuple.Elements, forall)
	//			goto parsedBody
	//		}
	//		var where TupleElement
	//		where.X = typeArg
	//		if p.tok.Type == lexer.Comma {
	//			where.Comma = p.tok
	//			p.next()
	//		}
	//		tuple.Elements = append(tuple.Elements, where)
	//	}
	//	for p.tok.Type == lexer.Ident || p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote {
	//		var where TupleElement
	//		switch p.tok.Type {
	//		case lexer.LeftParen:
	//			where.X = p.parseTupleTypeConstraint(true)
	//		case lexer.Ident:
	//			where.X = p.parseTypeName()
	//		case lexer.SingleQuote:
	//			where.X = p.parseNamedTypeArgument()
	//		default:
	//			panic("missing type body")
	//		}
	//		if p.tok.Type == lexer.Comma {
	//			where.Comma = p.tok
	//			p.next()
	//		}
	//		tuple.Elements = append(tuple.Elements, where)
	//	}
	//
	// parsedBody:
	//
	//	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	//	if expectParen {
	//		if p.tok.Type != lexer.RightParen {
	//			panic("missing right paren")
	//		}
	//		tuple.RightParen = p.tok
	//		p.next()
	//	}
	//	fmt.Println(p.tok)
	//	return tuple
}

// func (p *parser) parseTypeApplicationInConstraint(name Node) Node {
// 	defer p.trace("parseTypeApplication")()
// 	var namedType NamedType
// 	if name == nil {
// 		namedType.Name = p.parseTypeName()
// 	} else {
// 		namedType.Name = name
// 	}
// 	for p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident {
// 		switch p.tok.Type {
// 		case lexer.SingleQuote:
// 			namedType.Args = append(namedType.Args, p.parseNamedTypeArgument())
// 		case lexer.Ident:
// 			namedType.Args = append(namedType.Args, p.parseTypeName())
// 		case lexer.LeftParen:
// 			namedType.Args = append(namedType.Args, p.parseTupleTypeConstraint(true))
// 		}
// 	}
// 	return namedType
// }

func (p *parser) parseTypeParameter(topLevel bool) Node {
	defer p.trace("parseTypeParameter")()
	if p.tok.Type == lexer.TypeArg {
		return p.parseNamedTypeParameter()
	}
	if p.tok.Type == lexer.LeftParen {
		return p.parseTupleTypeParameter()
	}
	if p.tok.Type == lexer.Ident {
		if topLevel {
			panic("cannot have trait bound at the top level")
		} else {
			var typeApp TypeApplication
			typeApp.Elements = append(typeApp.Elements, p.parseTypeName())
			for p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.TypeArg || p.tok.Type == lexer.Ident {
				typeApp.Elements = append(typeApp.Elements, p.parseTypeParameter(false))
			}
			return typeApp
		}
	}
	panic("missing type parameter")
}

func (p *parser) parseTupleTypeParameter() Node {
	defer p.trace("parseTupleTypeParameter")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		elem.X = p.parseTypeParameter(false)
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		tuple.Elements = append(tuple.Elements, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return tuple
}

// 'a or '1
func (p *parser) parseNamedTypeParameter() Node {
	defer p.trace("parseNamedTypeParameter")()
	typeArg := p.tok
	p.next()
	return NamedTypeParameter{
		TypeParam: typeArg,
	}
}

func (p *parser) parseNamedTypeArgument() Node {
	defer p.trace("parseNamedTypeArgument")()
	typeArg := p.tok
	p.next()
	return NamedTypeArgument{
		TypeArg: typeArg,
	}
}

func (p *parser) parseTypeApplication(name Node, parseConstraint, parseAssignment bool) Node {
	defer p.trace("parseTypeApplication")()
	var typeApp TypeApplication
	if name == nil {
		name = p.parseTypeName()
	}
	typeApp.Elements = append(typeApp.Elements, name)
	for beginsAnonType(p.tok.Type) {
		if p.tok.Type == lexer.LeftParen {
			typeApp.Elements = append(typeApp.Elements, p.parseTypeBody(false, parseConstraint, parseAssignment))
		} else {
			typeApp.Elements = append(typeApp.Elements, p.parseTypeBody(false, parseConstraint, false))
		}
	}
	if len(typeApp.Elements) == 1 {
		return typeApp.Elements[0]
	}
	return typeApp
}

// Type Name can be a SelectorExpr or type parameter, since we could be accessing a type from another package.
func (p *parser) parseTypeName() Node {
	defer p.trace("parseTypeName")()
	if p.tok.Type != lexer.Ident {
		panic("missing identifier")
	}
	var typeName Node = Ident{Name: p.tok}
	p.next()
	for p.tok.Type == lexer.Period {
		period := p.tok
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("missing identifier")
		}
		ident := Ident{Name: p.tok}
		p.next()
		typeName = SelectorExpr{
			X:      typeName,
			Period: period,
			Name:   ident,
		}
	}
	return typeName
}

func (p *parser) parseArrayType(parseConstraint bool) Node {
	defer p.trace("parseArrayType")()
	var array ArrayType
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
	array.Type = p.parseTypeBody(false, parseConstraint, false)
	return array
}

func (p *parser) parseTypeBodyWithoutQuestionMark(parseSumType, parseConstraint, parseAssignment bool) Node {
	defer p.trace("parseTypeBodyWithoutQuestionMark")()
	// parse forall
	if p.tok.Type == lexer.TypeArg {
		var forall ForallType
		typeArg := p.parseNamedTypeArgument()
		// if p.tok.Type == lexer.Equals {
		// 	return typeArg
		// if !parseAssignment {
		// 	// return typeArg
		// 	if parseConstraint {
		// 		return typeArg
		// 	}
		// 	panic("a type assignment is not allowed here")
		// }
		// equals := p.tok
		// p.next()
		// return BinaryExpr{typeArg, equals, p.parseTypeBody(parseSumType, parseConstraint, false)}
		// }
		if p.tok.Type == lexer.Period {
			forall.TypeArg = typeArg
			forall.Period = p.tok
			p.next()
			forall.Type = p.parseTypeBody(parseSumType, parseConstraint, false)
			return forall
		}
		if beginsAnonType(p.tok.Type) {
			return p.parseTypeApplication(typeArg, parseConstraint, parseAssignment)
		}
		return typeArg
	}
	switch p.tok.Type {
	case lexer.LeftParen:
		if parseConstraint || parseAssignment {
			ttc := p.parseTupleTypeConstraint(parseAssignment)
			if beginsAnonType(p.tok.Type) {
				return p.parseTypeApplication(ttc, parseConstraint, parseAssignment)
			} else {
				return ttc
			}
		}
		return p.parseTupleType()
	case lexer.LeftBracket:
		// Is this allowed inside a type constraint or assignment? I think so.
		// A list type looks like [N]'t, where 't is the element type, and N is an optional constant length.
		// maybe we pass parseConstraint down?
		return p.parseArrayType(parseConstraint)
	case lexer.Ident:
		return p.parseTypeApplication(nil, parseConstraint, parseAssignment)
	case lexer.Fun:
		// if parseConstraint {
		// 	// TODO: i think this is allowed in a type constraint
		// 	panic("function types are not allowed in type constraints")
		// }
		// maybe we pass parseConstraint down?
		return p.parseFunctionType(parseConstraint)
	case lexer.Or:
		if parseAssignment {
			panic("sum types are not allowed in type assignments")
		}
		if !parseSumType {
			panic("sum types are only allowed in type declarations")
		}
		if parseConstraint {
			panic("sum types are not allowed in type constraints")
		}
		return p.parseSumType()
	default:
		panic("missing type body")
	}
}

func (p *parser) parseTypeBodyWithoutWhere(parseSumType, parseConstraint, parseAssignment bool) Node {
	t := p.parseTypeBodyWithoutQuestionMark(parseSumType, parseConstraint, parseAssignment)
	// parse nillable type
	if p.tok.Type == lexer.QuestionMark {
		question := p.tok
		p.next()
		return NillableType{
			Type:         t,
			QuestionMark: question,
		}
	}
	return t
}

// TODO: TypeBody should have one sum case, and if it sees a |, it should parse more cases.
func (p *parser) parseTypeBody(parseSumType, parseConstraint, parseAssignment bool) Node {
	defer p.trace("parseTypeBody")()
	tb := p.parseTypeBodyWithoutWhere(parseSumType, parseConstraint, parseAssignment)
	if p.tok.Type == lexer.Where {
		if parseAssignment {
			panic("where clauses are not allowed in type assignments")
		}
		where := p.tok
		p.next()
		// where clause is just a comma-delimited list of type bodies.
		// might have to restrict it to everything but sums
		// typeConstraint := p.parseTupleTypeConstraint(false)
		typeConstraint := p.parseTypeBody(false, true, false)
		return Where{
			TypeBody: tb,
			Where:    where,
			Clause:   typeConstraint,
		}
	}
	return tb
}

func (p *parser) parseSumType() Node {
	defer p.trace("parseSumType")()
	var sum SumType
	for p.tok.Type == lexer.Or {
		var sumElem SumTypeElement
		sumElem.Or = p.tok
		p.next()
		if p.tok.Type != lexer.Ident {
			panic("missing identifier")
		}
		sumElem.Name = Ident{Name: p.tok}
		p.next()
		if beginsAnonType(p.tok.Type) {
			sumElem.Type = p.parseTypeBody(false, false, false)
		}
		sum.Elements = append(sum.Elements, sumElem)
	}
	return sum
}

func beginsAnonType(ttype lexer.TokenType) bool {
	switch ttype {
	case lexer.LeftParen, lexer.TypeArg, lexer.Ident, lexer.Fun, lexer.LeftBracket:
		return true
	}
	return false
}

// "=" is for default values.
func (p *parser) parseTupleType() Node {
	defer p.trace("parseTupleType")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	lpar := p.tok
	p.next()
	var elems []Node
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		p.shouldInsertDelimAfter(true, lexer.TypeArg)
		switch p.tok.Type {
		case lexer.Ident:
			// Could be a field name or a named type.
			if t := p.peek(); t.Type == lexer.Colon {
				// Field name.
				var field Field
				field.Name = Ident{Name: p.tok}
				p.next()
				field.Colon = p.tok
				p.next()
				field.Type = p.parseTypeBody(false, false, false)
				if p.tok.Type == lexer.Equals {
					field.Equals = p.tok
					p.next()
					field.Default = p.parseExpr()
				}
				elem.X = field
			} else {
				elem.X = p.parseTypeBody(false, false, false)
			}
		default:
			elem.X = p.parseTypeBody(false, false, false)
		}
		p.shouldInsertDelimAfter(false)
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		if p.tok.Type == lexer.LineTerminator {
			p.next()
		}
		elems = append(elems, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	rpar := p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return Tuple{
		LeftParen:  lpar,
		Elements:   elems,
		RightParen: rpar,
	}
}

func (p *parser) parseFunctionType(parseConstraint bool) Node {
	defer p.trace("parseFunctionType")()
	var fun FunctionType
	fun.Fun = p.tok
	p.next()
	fun.Param = p.parseTypeBodyWithoutWhere(false, parseConstraint, false)
	for p.tok.Type == lexer.RightArrow {
		var arrow Arrow
		arrow.Arrow = p.tok
		p.next()
		arrow.Type = p.parseTypeBodyWithoutWhere(false, parseConstraint, false)
		fun.Arrows = append(fun.Arrows, arrow)
	}
	return fun
}

func (p *parser) parseImportDeclPackage() Node {
	defer p.trace("parseImportDeclPackage")()
	switch p.tok.Type {
	case lexer.String:
		path := p.parseString()
		return ImportDeclPackage{Path: path}
	case lexer.Ident:
		alias := p.tok
		p.next()
		if p.tok.Type != lexer.Equals {
			panic("missing equals")
		}
		equals := p.tok
		p.next()
		if p.tok.Type != lexer.String {
			panic("missing string")
		}
		path := p.parseString()
		return ImportDeclPackage{
			Binding: Ident{Name: alias},
			Equals:  equals,
			Path:    path,
		}
	case lexer.LeftParen:
		// tuple of identifiers
		tup := p.parseIdentTuple()
		if p.tok.Type != lexer.Equals {
			panic("missing equals")
		}
		equals := p.tok
		p.next()
		if p.tok.Type != lexer.String {
			panic("missing string")
		}
		path := p.parseString()
		return ImportDeclPackage{
			Binding: tup,
			Equals:  equals,
			Path:    path,
		}
	}
	panic("invalid import declaration")
}

func (p *parser) parseImportDeclBlock() Node {
	// essentially a tuple of import declarations
	defer p.trace("parseImportDeclBlock")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var tuple Tuple
	tuple.LeftParen = p.tok
	p.next()
	for p.tok.Type != lexer.RightParen && p.tok.Type != lexer.EOF {
		var elem TupleElement
		elem.X = p.parseImportDeclPackage()
		if p.tok.Type == lexer.Comma {
			elem.Comma = p.tok
			p.next()
		}
		if p.tok.Type == lexer.LineTerminator {
			p.next()
		}
		tuple.Elements = append(tuple.Elements, elem)
	}
	if p.tok.Type != lexer.RightParen {
		panic("missing right paren")
	}
	tuple.RightParen = p.tok
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	p.next()
	return tuple
}

func (p *parser) parseImportDecl() Node {
	// import "github.com/someone/math"
	// import alias = "github.com/someone/math"
	// import Sqrt, Abs from "github.com/someone/math"
	// import (
	// 	"github.com/someone/math"
	// 	alias = "github.com/someone/math"
	// 	Sqrt, Abs from "github.com/someone/math"
	// )
	defer p.trace("parseImportDecl")()
	var importDecl ImportDecl
	importDecl.Import = p.tok
	p.next()
	if p.tok.Type == lexer.LeftParen {
		switch p.peek().Type {
		case lexer.Ident:
			switch p.peek2().Type {
			case lexer.Comma, lexer.RightParen:
				importDecl.Package = p.parseImportDeclPackage()
				return importDecl
			}
		}
		importDecl.Package = p.parseImportDeclBlock()
		return importDecl
	}
	importDecl.Package = p.parseImportDeclPackage()
	return importDecl
}

func (p *parser) parseImplDecl() Node {
	defer p.trace("parseImplDecl")()
	impl := ImplDecl{Impl: p.tok}
	p.next()
	impl.Name = p.parseTypeName()
	impl.Args = p.parseTypeBodyWithoutWhere(false, false, true)
	if p.tok.Type == lexer.Where {
		impl.Where = p.tok
		p.next()
		impl.Clause = p.parseTypeBody(false, true, false)
	}
	if p.tok.Type == lexer.Equals {
		impl.Equals = p.tok
		p.next()
		impl.Body = p.parseExpr()
	}
	return impl
	// p.parseTypeBod
	// impl.Name = p.parseTypeName()
	// if p.tok.Type == lexer.Where {
	// 	impl.Where = p.tok
	// 	p.next()
	// 	impl.Body = p.parseBody(lexer.EOF)
	// 	return impl
	// }
	// if p.tok.Type != lexer.LeftBrace {
	// 	panic("missing left brace")
	// }
	// impl.LeftBrace = p.tok
	// p.next()
	// for p.tok.Type != lexer.RightBrace && p.tok.Type != lexer.EOF {
	// 	impl.Body = append(impl.Body, p.parseExprOrStmt())
	// }
	// if p.tok.Type != lexer.RightBrace {
	// 	panic("missing right brace")
	// }
	// impl.RightBrace = p.tok
	// p.next()
	// return impl
}

func (p *parser) parseExprOrStmt() Node {
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
	case lexer.Impl:
		return p.parseImplDecl()
	case lexer.If:
		return p.parseIf()
	case lexer.Semicolon, lexer.LineTerminator:
		return EmptyExpr{}
	default:
		return p.parseExpr()
	}
}

func (p *parser) parseBody(until lexer.TokenType) Block {
	defer p.trace("parseBody")()
	shouldInsert := p.shouldInsertAfter
	toksToCheck := p.afterToksToCheck
	p.shouldInsertDelimAfter(false)
	var body []Node
	for p.tok.Type != until && p.tok.Type != lexer.EOF {
		n := p.parseExprOrStmt()
		if p.tok.Type == lexer.Semicolon || p.tok.Type == lexer.LineTerminator {
			n = Stmt{
				Stmt:      n,
				Semicolon: p.tok,
			}
			p.next()
		}
		body = append(body, n)
	}
	p.shouldInsertDelimAfter(shouldInsert, toksToCheck...)
	return Block{Body: body}
}
