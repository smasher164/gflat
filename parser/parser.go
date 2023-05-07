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

func ParseFile(l Lexer) File {
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
		f.PackageDecl = &PackageDecl{Package: p.tok}
		p.next()
		if p.tok.Type == lexer.Ident {
			f.PackageDecl.Name = Ident{Name: p.tok}
		} else {
			// do I need to give it a span?
			f.PackageDecl.Name = Illegal{Msg: "expected identifier after package"}
		}
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
				panic("expected = or { after function signature")
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
		return Illegal{Node: decl.Destructure, Msg: "expected = after let declaration"}
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
			param.Type = p.parseTypeBody(false)
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
		arrow.Type = p.parseTypeBody(false)
		fun.Arrows = append(fun.Arrows, arrow)
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
	} else if p.tok.Type == lexer.Equals {
		fun.Equals = p.tok
		p.next()
	} else if p.tok.Type != lexer.LeftBrace {
		panic("expected = or { after function signature")
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
	for p.tok.BeginsPrefixExprExceptBinary() {
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
		if op.Type == lexer.Colon {
			rhs = p.parseTypeBody(false)
		} else {
			rhs = p.parseBinaryExpr(nextMinPrec)
		}
		res = filterBinExp(BinaryExpr{Left: res, Op: op, Right: rhs})
	}
	return res
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
		typ := p.parseTypeBody(false)
		des = TypeAnnotation{
			Destructure: des,
			Colon:       colon,
			Type:        typ,
		}
	}
	return des
}

// TupleDestructur	e = "(" Destructure ")"
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
	if p.tok.Type == lexer.Or {
		cases := p.parseIfMatch()
		return IfMatch{
			IfHeader: ifHeader,
			Cases:    cases,
		}
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

func (p *parser) parseIfMatch() []Node {
	defer p.trace("parseIfMatch")()
	var cases []Node
	for p.tok.Type == lexer.Or {
		cases = append(cases, p.parseCase())
	}
	return cases
}

func (p *parser) parseCase() Node {
	defer p.trace("parseCase")()
	var patCase PatternCase
	patCase.Or = p.tok
	p.next()
	patCase.Pattern = p.parsePattern()
	if p.tok.Type == lexer.If {
		patCase.Guard = p.parseGuard()
	}
	if p.tok.Type != lexer.RightArrow { // TODO: should we use a fat arrow here? | x : fun int -> int -> x is ambiguous.
		panic("missing arrow")
	}
	patCase.Arrow = p.tok
	p.next()
	p.shouldInsertDelimAfter(true, lexer.Or)
	patCase.Expr = p.parseExpr()
	p.shouldInsertDelimAfter(false)
	p.shouldInsertDelimBefore(true, lexer.Comma)
	if p.tok.Type == lexer.Comma {
		patCase.Comma = p.tok
		p.next()
	}
	p.shouldInsertDelimBefore(false)
	if p.tok.Type == lexer.LineTerminator {
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
		typ := p.parseTypeBody(false)
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
	for p.tok.Type == lexer.Ident || p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote {
		typeParams = append(typeParams, p.parseTypeParameter(true))
	}
	if p.tok.Type != lexer.Equals {
		panic("missing equals")
	}
	equals := p.tok
	p.next()
	typeBody := p.parseTypeBody(true)
	return TypeDecl{
		Type:       typeTok,
		Name:       typeName,
		TypeParams: typeParams,
		Equal:      equals,
		Body:       typeBody,
	}
}

func (p *parser) parseTypeParameter(topLevel bool) Node {
	defer p.trace("parseTypeParameter")()
	if p.tok.Type == lexer.SingleQuote {
		return p.parseNamedTypeParameter()
	}
	if p.tok.Type == lexer.LeftParen {
		return p.parseTupleTypeParameter()
	}
	if p.tok.Type == lexer.Ident {
		if topLevel {
			panic("cannot have trait bound at the top level")
		} else {
			var namedType NamedType
			namedType.Name = p.parseTypeName()
			for p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident {
				namedType.Args = append(namedType.Args, p.parseTypeParameter(false))
			}
			return namedType
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
	singleQuote := p.tok
	p.next()
	var name Node
	switch p.tok.Type {
	case lexer.Ident:
		name = Ident{Name: p.tok}
	case lexer.Number:
		n := Number{Lit: p.tok}
		if n.IsNat() {
			name = n
		} else {
			panic("expected nat")
		}
	default:
		panic("missing identifier or number")
	}
	p.next()
	return NamedTypeParameter{
		SingleQuote: singleQuote,
		Name:        name,
	}
}

func (p *parser) parseNamedTypeArgument() Node {
	defer p.trace("parseNamedTypeArgument")()
	singleQuote := p.tok
	p.next()
	var name Node
	switch p.tok.Type {
	case lexer.Ident:
		name = Ident{Name: p.tok}
	case lexer.Number:
		n := Number{Lit: p.tok}
		if n.IsNat() {
			name = n
		} else {
			panic("expected nat")
		}
	default:
		panic("missing identifier or number")
	}
	p.next()
	return NamedTypeArgument{
		SingleQuote: singleQuote,
		Name:        name,
	}
}

func (p *parser) parseTypeApplication(name Node) Node {
	defer p.trace("parseTypeApplication")()
	var namedType NamedType
	if name == nil {
		namedType.Name = p.parseTypeName()
	} else {
		namedType.Name = name
	}
	for p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident || p.tok.Type == lexer.Fun {
		namedType.Args = append(namedType.Args, p.parseTypeBody(false))
	}
	return namedType
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

// TODO: TypeBody should have one sum case, and if it sees a |, it should parse more cases.
func (p *parser) parseTypeBody(parseSumType bool) Node {
	defer p.trace("parseTypeBody")()
	// parse forall
	if p.tok.Type == lexer.SingleQuote {
		var forall ForallType
		typeArg := p.parseNamedTypeArgument()
		if p.tok.Type == lexer.Period {
			forall.TypeArg = typeArg
			forall.Period = p.tok
			p.next()
			forall.Type = p.parseTypeBody(parseSumType)
			return forall
		}
		return p.parseTypeApplication(typeArg)
	}
	switch p.tok.Type {
	case lexer.LeftParen:
		return p.parseTupleType()
	case lexer.Ident:
		return p.parseTypeApplication(nil)
	case lexer.Fun:
		return p.parseFunctionType()
	case lexer.Or:
		if !parseSumType {
			panic("sum types are only allowed in type declaration")
		}
		return p.parseSumType()
	default:
		panic("missing type body")
	}
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
		if p.tok.Type == lexer.LeftParen || p.tok.Type == lexer.SingleQuote || p.tok.Type == lexer.Ident || p.tok.Type == lexer.Fun {
			sumElem.Type = p.parseTypeBody(false)
		}
		sum.Elements = append(sum.Elements, sumElem)
	}
	return sum
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
		p.shouldInsertDelimAfter(true, lexer.Fun, lexer.SingleQuote)
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
				field.Type = p.parseTypeBody(false)
				if p.tok.Type == lexer.Equals {
					field.Equals = p.tok
					p.next()
					field.Default = p.parseExpr()
				}
				elem.X = field
			} else {
				elem.X = p.parseTypeBody(false)
			}
		default:
			elem.X = p.parseTypeBody(false)
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

func (p *parser) parseFunctionType() Node {
	defer p.trace("parseFunctionType")()
	var fun FunctionType
	fun.Fun = p.tok
	p.next()
	fun.Param = p.parseTypeBody(false)
	for p.tok.Type == lexer.RightArrow {
		var arrow Arrow
		arrow.Arrow = p.tok
		p.next()
		arrow.Type = p.parseTypeBody(false)
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
	fmt.Println(p.tok)
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
