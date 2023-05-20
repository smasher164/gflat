package lexer

import (
	"fmt"

	"golang.org/x/exp/slices"
)

//go:generate go run golang.org/x/tools/cmd/stringer -type=TokenType
type TokenType int

const (
	EOF TokenType = iota
	LineTerminator
	Plus
	Minus
	Times
	Divide
	// Backslash
	Remainder
	And
	Or
	Caret
	Tilde
	Backtick
	// SingleQuote
	LessThan
	GreaterThan

	Equals
	Colon
	Not
	Comma
	Period
	Semicolon
	LeftParen
	RightParen
	LeftBrace
	RightBrace
	LeftBracket
	RightBracket
	DollarSign
	QuestionMark

	LogicalAnd
	LogicalOr
	LeftShift
	RightShift
	LogicalEquals
	FatArrow
	NotEquals
	LessThanEquals
	GreaterThanEquals
	ColonEquals
	Exponentiation
	DotDot
	LeftArrow
	RightArrow

	Fun
	Type
	Trait
	Import
	Let
	Var
	If
	Else
	Case
	Match
	Package
	Iso
	Ref
	Mut
	Pub
	With
	Impl

	Ident
	Number
	TypeArg
	Whitespace
	SingleLineComment
	// MultilineComment
	StringBeg
	StringPart
	StringEnd
	String
	Illegal
)

var SingleCharTokens = map[rune]TokenType{
	'+': Plus,
	'%': Remainder,
	'^': Caret,
	// '\\': Backslash,
	'~': Tilde,
	',': Comma,
	';': Semicolon,
	'?': QuestionMark,
	'(': LeftParen,
	')': RightParen,
	// '{':  LeftBrace,
	'[': LeftBracket,
	']': RightBracket,
	'`': Backtick,
	// '\'': SingleQuote,
	'/': Divide,
	'-': Minus,
	'*': Times,
	'&': And,
	'|': Or,
	'<': LessThan,
	'>': GreaterThan,
	'=': Equals,
	'!': Not,
	':': Colon,
	'.': Period,
	'$': DollarSign,
	eof: EOF,
}

var DoubleCharTokens = map[[2]rune]TokenType{
	{'-', '>'}: RightArrow,
	{'*', '*'}: Exponentiation,
	{'&', '&'}: LogicalAnd,
	{'|', '|'}: LogicalOr,
	{'<', '='}: LessThanEquals,
	{'<', '<'}: LeftShift,
	{'<', '-'}: LeftArrow,
	{'>', '='}: GreaterThanEquals,
	{'>', '>'}: RightShift,
	{'=', '='}: LogicalEquals,
	{'=', '>'}: FatArrow,
	{'!', '='}: NotEquals,
	{':', '='}: ColonEquals,
	{'.', '.'}: DotDot,
}

var Keywords = map[string]TokenType{
	"fun":     Fun,
	"type":    Type,
	"trait":   Trait,
	"import":  Import,
	"let":     Let,
	"var":     Var,
	"if":      If,
	"else":    Else,
	"case":    Case,
	"match":   Match,
	"package": Package,
	"iso":     Iso, // TODO: are
	"ref":     Ref, // we
	"mut":     Mut, // doing
	"pub":     Pub, // these?
	"with":    With,
	"impl":    Impl,
}

type Pos struct {
	Offset int
	Line   int
	Column int
}

func (p Pos) Min(other Pos) Pos {
	if p.Column == 0 {
		return other
	}
	if other.Column == 0 {
		return p
	}
	if p.Offset < other.Offset {
		return p
	}
	return other
}

func (p Pos) Max(other Pos) Pos {
	if p.Column == 0 {
		return other
	}
	if other.Column == 0 {
		return p
	}
	if p.Offset > other.Offset {
		return p
	}
	return other
}

// func (p Pos) String() string {
// 	return fmt.Sprintf("%d:%d", p.Line, p.Column)
// }

type Span struct {
	Start Pos
	End   Pos
}

func (span Span) Add(other Span) Span {
	return Span{span.Start.Min(other.Start), span.End.Max(other.End)}
}

func (s Span) String() string {
	if s.Start == s.End {
		return fmt.Sprintf("%d:%d", s.Start.Line, s.Start.Column)
	}
	if s.Start.Line == s.End.Line {
		return fmt.Sprintf("%d:%d-%d", s.Start.Line, s.Start.Column, s.End.Column)
	}
	return fmt.Sprintf("%d:%d-%d:%d", s.Start.Line, s.Start.Column, s.End.Line, s.End.Column)
}

type Token struct {
	LeadingTrivia []Token
	Type          TokenType
	Span          Span
	Data          string
}

func (t Token) String() string {
	if t.Data == "" {
		return fmt.Sprintf("%s:%s", t.Span, t.Type)
	}
	return fmt.Sprintf("%s:%s %q", t.Span, t.Type, t.Data)
}

func (b Token) Eq(a Token) bool {
	return a.Type == b.Type && a.Data == b.Data
}

func (a Token) ExactEq(b Token) bool {
	return a.Type == b.Type && a.Span == b.Span && a.Data == b.Data && slices.EqualFunc(a.LeadingTrivia, b.LeadingTrivia, Token.ExactEq)
}

func (t Token) IsBinaryOp() bool {
	switch t.Type {
	case With, DotDot, Plus, Minus, Times, Divide, Remainder, LeftShift, RightShift, And, Or, Caret, LogicalAnd, LogicalOr, LogicalEquals, NotEquals, Equals, LessThan, LessThanEquals, GreaterThan, GreaterThanEquals, LeftArrow, Exponentiation, Colon:
		return true
	}
	return false
}

func (t Token) IsPrefixOp() bool {
	switch t.Type {
	case DotDot, Plus, Minus, Not, Caret, Tilde, LeftArrow:
		return true
	}
	return false
}

func (t Token) IsPostfixOp() bool { return t.Type == DotDot || t.Type == QuestionMark }

const MinPrec = 1

// In typechecking, handle ambiguity around certain operators.
func (t Token) Prec() int {
	// switch t.Type {
	// case Colon:
	// 	return 8
	// case Exponentiation:
	// 	return 7
	// case Times, Divide, Remainder, And, LeftShift, RightShift:
	// 	return 6
	// case Plus, Minus, Or, Caret:
	// 	return 5
	// case LogicalEquals, NotEquals, LessThan, GreaterThan, LessThanEquals, GreaterThanEquals:
	// 	return 4
	// case LogicalAnd:
	// 	return 3
	// case LogicalOr:
	// 	return 2
	// case Equals, LeftArrow:
	// 	return 1
	// }
	// return 0
	switch t.Type {
	case Colon, With:
		return 9
	case Exponentiation:
		return 8
	case Times, Divide, Remainder, And, LeftShift, RightShift:
		return 7
	case Plus, Minus, Or, Caret:
		return 6
	case LogicalEquals, NotEquals, LessThan, GreaterThan, LessThanEquals, GreaterThanEquals:
		return 5
	case LogicalAnd:
		return 4
	case LogicalOr:
		return 3
	case DotDot:
		return 2
	case Equals, LeftArrow:
		return 1
	}
	return 0
}

func (t Token) IsLeftAssoc() bool {
	switch t.Type {
	case Times, Divide, Remainder, And, LeftShift, RightShift, Plus, Minus, Or, Caret, LogicalEquals, NotEquals, LessThan, GreaterThan, LessThanEquals, GreaterThanEquals, LogicalAnd, LogicalOr, Colon:
		return true
	}
	return false
}

func (t Token) IsRightAssoc() bool {
	switch t.Type {
	case Exponentiation, LeftArrow, Equals:
		return true
	}
	return false
}

func (t Token) BeginsPrefixExpr() bool {
	if t.IsPrefixOp() {
		return true
	}
	switch t.Type {
	case Fun, LeftParen, LeftBracket, LeftBrace, Ident, Number, StringBeg, String:
		return true
	}
	return false
}

func (t Token) BeginsArgumentExpr() bool {
	return t.BeginsPrefixExpr() && !t.IsBinaryOp()
}

// func (t Token) BinaryCategory() int {
// 	switch t.Type {
// 	case Plus, Minus, Times, Divide, Remainder, Exponentiation:
// 		return 0
// 	case LogicalAnd:
// 		return 1
// 	case LogicalOr:
// 		return 2
// 	case LogicalEquals, NotEquals, LessThan, LessThanEquals, GreaterThan, GreaterThanEquals:
// 		return 3
// 	case Equals, LeftArrow, Colon:
// 		return 4
// 	case LeftShift, RightShift, And, Or, Caret:
// 		return 5
// 	}
// 	return 6
// }

func (t Token) IsArithmetic() bool {
	switch t.Type {
	case Plus, Minus, Times, Divide, Remainder, Exponentiation:
		return true
	}
	return false
}

func (t Token) IsBitwise() bool {
	switch t.Type {
	case LeftShift, RightShift, And, Or, Caret:
		return true
	}
	return false
}

func (a Token) BinaryInteroperable(b Token) bool {
	// if a.Type == NotEquals && b.Type == NotEquals {
	// 	return false
	// }
	// acat := a.BinaryCategory()
	// bcat := b.BinaryCategory()
	// if acat < 3 && bcat < 3 {
	// 	return true
	// }
	// return acat == bcat
	/*
		1. Logical AND is not interoperable with logical OR.
		2. Arithmetic operators are not interoperable with bitwise operators.
	*/
	if a.Type == LogicalAnd && b.Type == LogicalOr {
		return false
	}
	if a.Type == LogicalOr && b.Type == LogicalAnd {
		return false
	}
	if a.IsArithmetic() && b.IsBitwise() {
		return false
	}
	if a.IsBitwise() && b.IsArithmetic() {
		return false
	}
	return true
}

func (a Token) OnDifferentLines(b Token) bool {
	return a.Span.End.Line != b.Span.Start.Line
}

func (t Token) IsBeforeSemicolon(shouldInsert bool, toksToCheck []TokenType) bool {
	switch t.Type {
	case RightParen, RightBrace, RightBracket, Ident, TypeArg, Number, String, StringEnd:
		return true
	}
	// comma
	if shouldInsert && slices.Contains(toksToCheck, t.Type) {
		return true
	}
	return false
}

func (t Token) IsAfterSemicolon(shouldInsert bool, toksToCheck []TokenType) bool {
	switch t.Type {
	case LeftParen, LeftBrace, LeftBracket, Ident, TypeArg, Number, String, StringBeg, Fun: // when would Fun *not* be a prefix?
		return true
	}
	// Or, Fun, SingleQuote
	if shouldInsert && slices.Contains(toksToCheck, t.Type) {
		return true
	}
	return false
}

// func (a Token) NeedsSemicolon(t Token) bool {
// 	if a.IsBeforeSemicolon() && t.IsAfterSemicolon() {
// 		return true
// 	}
// 	if a.Type == Comma && t.Type == Or {
// 		return true
// 	}
// 	return false
// }
