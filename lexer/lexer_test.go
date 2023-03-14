package lexer_test

import (
	"fmt"
	"strings"
	"testing"
	"testing/fstest"
	"unicode/utf8"

	"github.com/kr/pretty"
	. "github.com/smasher164/gflat/lexer"
	"golang.org/x/exp/slices"
)

const debug = true

func single(trivia []Token, ttyp TokenType, pos Pos) Token {
	return Token{LeadingTrivia: trivia, Type: ttyp, Span: Span{Start: pos, End: pos}, Data: ""}
}

func singleWS(wsPos Pos, ttyp TokenType, pos Pos) Token {
	return Token{LeadingTrivia: []Token{
		{nil, Whitespace, unitSpan(wsPos), " "},
	}, Type: ttyp, Span: Span{Start: pos, End: pos}, Data: ""}
}

func double(trivia []Token, ttyp TokenType, start Pos) Token {
	return Token{LeadingTrivia: trivia, Type: ttyp, Span: Span{Start: start, End: Pos{Offset: start.Offset + 1, Line: start.Line, Column: start.Column + 1}}, Data: ""}
}

func doubleWS(wsPos Pos, ttyp TokenType, start Pos) Token {
	return Token{LeadingTrivia: []Token{
		{nil, Whitespace, unitSpan(wsPos), " "},
	}, Type: ttyp, Span: Span{Start: start, End: Pos{Offset: start.Offset + 1, Line: start.Line, Column: start.Column + 1}}, Data: ""}
}

func keyword(trivia []Token, ttyp TokenType, start, end Pos) Token {
	return Token{LeadingTrivia: trivia, Type: ttyp, Span: Span{Start: start, End: end}}
}

func keywordWS(wsPos Pos, ttyp TokenType, start, end Pos) Token {
	return Token{LeadingTrivia: []Token{
		{nil, Whitespace, unitSpan(wsPos), " "},
	}, Type: ttyp, Span: Span{Start: start, End: end}}
}

func dataTok(trivia []Token, ttyp TokenType, start Pos, data string) Token {
	dataLen := utf8.RuneCountInString(data)
	return Token{LeadingTrivia: trivia, Type: ttyp, Span: Span{Start: start, End: Pos{Offset: start.Offset + dataLen - 1, Line: start.Line, Column: start.Column + dataLen - 1}}, Data: data}
}

func dataTokWS(wsPos Pos, ttyp TokenType, start Pos, data string) Token {
	dataLen := utf8.RuneCountInString(data)
	return Token{LeadingTrivia: []Token{
		{nil, Whitespace, unitSpan(wsPos), " "},
	}, Type: ttyp, Span: Span{Start: start, End: Pos{Offset: start.Offset + dataLen - 1, Line: start.Line, Column: start.Column + dataLen - 1}}, Data: data}
}

func unitSpan(pos Pos) Span {
	return Span{Start: pos, End: pos}
}

func TestLexer(t *testing.T) {
	testfs := fstest.MapFS{}
	run := func(name, data string, expected []Token) {
		t.Run(name, func(t *testing.T) {
			testfs[name] = &fstest.MapFile{
				Data: []byte(data),
			}
			l, err := NewLexer(name, testfs)
			if err != nil {
				t.Error(err)
			}
			var got []Token
			var tok Token
			for tok = l.Next(); tok.Type != EOF; tok = l.Next() {
				got = append(got, tok)
			}
			got = append(got, tok)
			if !slices.EqualFunc(got, expected, Token.ExactEq) {
				if debug {
					fmt.Printf("got: %v\n", got)
				}
				t.Log(name)
				pretty.Ldiff(t, expected, got)
				t.Fail()
			}
		})
	}
	// if debug {
	// 	run("test", `\"a"\t""`, []Token{{Type: EOF}})
	// 	return
	// }

	run("empty.txt", "", []Token{single(nil, EOF, Pos{0, 1, 1})})

	run("singlechar.txt", "+ % ^ ~ , ; ? ( ) [ ] ` ' / - * & | < > = ! : . $", []Token{
		single(nil, Plus, Pos{0, 1, 1}),
		singleWS(Pos{1, 1, 2}, Remainder, Pos{2, 1, 3}),
		singleWS(Pos{3, 1, 4}, Caret, Pos{4, 1, 5}),
		singleWS(Pos{5, 1, 6}, Tilde, Pos{6, 1, 7}),
		singleWS(Pos{7, 1, 8}, Comma, Pos{8, 1, 9}),
		singleWS(Pos{9, 1, 10}, Semicolon, Pos{10, 1, 11}),
		singleWS(Pos{11, 1, 12}, QuestionMark, Pos{12, 1, 13}),
		singleWS(Pos{13, 1, 14}, LeftParen, Pos{14, 1, 15}),
		singleWS(Pos{15, 1, 16}, RightParen, Pos{16, 1, 17}),
		singleWS(Pos{17, 1, 18}, LeftBracket, Pos{18, 1, 19}),
		singleWS(Pos{19, 1, 20}, RightBracket, Pos{20, 1, 21}),
		singleWS(Pos{21, 1, 22}, Backtick, Pos{22, 1, 23}),
		singleWS(Pos{23, 1, 24}, SingleQuote, Pos{24, 1, 25}),
		singleWS(Pos{25, 1, 26}, Divide, Pos{26, 1, 27}),
		singleWS(Pos{27, 1, 28}, Minus, Pos{28, 1, 29}),
		singleWS(Pos{29, 1, 30}, Times, Pos{30, 1, 31}),
		singleWS(Pos{31, 1, 32}, And, Pos{32, 1, 33}),
		singleWS(Pos{33, 1, 34}, Or, Pos{34, 1, 35}),
		singleWS(Pos{35, 1, 36}, LessThan, Pos{36, 1, 37}),
		singleWS(Pos{37, 1, 38}, GreaterThan, Pos{38, 1, 39}),
		singleWS(Pos{39, 1, 40}, Equals, Pos{40, 1, 41}),
		singleWS(Pos{41, 1, 42}, Not, Pos{42, 1, 43}),
		singleWS(Pos{43, 1, 44}, Colon, Pos{44, 1, 45}),
		singleWS(Pos{45, 1, 46}, Period, Pos{46, 1, 47}),
		singleWS(Pos{47, 1, 48}, DollarSign, Pos{48, 1, 49}),
		single(nil, EOF, Pos{49, 1, 50}),
	})

	run("doublechar.txt", "-> ** && || <= << <- >= >> == != := ..", []Token{
		double(nil, RightArrow, Pos{0, 1, 1}),
		doubleWS(Pos{2, 1, 3}, Exponentiation, Pos{3, 1, 4}),
		doubleWS(Pos{5, 1, 6}, LogicalAnd, Pos{6, 1, 7}),
		doubleWS(Pos{8, 1, 9}, LogicalOr, Pos{9, 1, 10}),
		doubleWS(Pos{11, 1, 12}, LessThanEquals, Pos{12, 1, 13}),
		doubleWS(Pos{14, 1, 15}, LeftShift, Pos{15, 1, 16}),
		doubleWS(Pos{17, 1, 18}, LeftArrow, Pos{18, 1, 19}),
		doubleWS(Pos{20, 1, 21}, GreaterThanEquals, Pos{21, 1, 22}),
		doubleWS(Pos{23, 1, 24}, RightShift, Pos{24, 1, 25}),
		doubleWS(Pos{26, 1, 27}, LogicalEquals, Pos{27, 1, 28}),
		doubleWS(Pos{29, 1, 30}, NotEquals, Pos{30, 1, 31}),
		doubleWS(Pos{32, 1, 33}, ColonEquals, Pos{33, 1, 34}),
		doubleWS(Pos{35, 1, 36}, DotDot, Pos{36, 1, 37}),
		single(nil, EOF, Pos{38, 1, 39}),
	})

	run("keywords.txt", "fun type trait import let var if else case match package iso ref mut pub", []Token{
		keyword(nil, Fun, Pos{0, 1, 1}, Pos{2, 1, 3}),
		keywordWS(Pos{3, 1, 4}, Type, Pos{4, 1, 5}, Pos{7, 1, 8}),
		keywordWS(Pos{8, 1, 9}, Trait, Pos{9, 1, 10}, Pos{13, 1, 14}),
		keywordWS(Pos{14, 1, 15}, Import, Pos{15, 1, 16}, Pos{20, 1, 21}),
		keywordWS(Pos{21, 1, 22}, Let, Pos{22, 1, 23}, Pos{24, 1, 25}),
		keywordWS(Pos{25, 1, 26}, Var, Pos{26, 1, 27}, Pos{28, 1, 29}),
		keywordWS(Pos{29, 1, 30}, If, Pos{30, 1, 31}, Pos{31, 1, 32}),
		keywordWS(Pos{32, 1, 33}, Else, Pos{33, 1, 34}, Pos{36, 1, 37}),
		keywordWS(Pos{37, 1, 38}, Case, Pos{38, 1, 39}, Pos{41, 1, 42}),
		keywordWS(Pos{42, 1, 43}, Match, Pos{43, 1, 44}, Pos{47, 1, 48}),
		keywordWS(Pos{48, 1, 49}, Package, Pos{49, 1, 50}, Pos{55, 1, 56}),
		keywordWS(Pos{56, 1, 57}, Iso, Pos{57, 1, 58}, Pos{59, 1, 60}),
		keywordWS(Pos{60, 1, 61}, Ref, Pos{61, 1, 62}, Pos{63, 1, 64}),
		keywordWS(Pos{64, 1, 65}, Mut, Pos{65, 1, 66}, Pos{67, 1, 68}),
		keywordWS(Pos{68, 1, 69}, Pub, Pos{69, 1, 70}, Pos{71, 1, 72}),
		single(nil, EOF, Pos{72, 1, 73}),
	})

	run("identifiers.txt", "_ __ a_b_c a12 अखिल", []Token{
		dataTok(nil, Ident, Pos{0, 1, 1}, "_"),
		dataTokWS(Pos{1, 1, 2}, Ident, Pos{2, 1, 3}, "__"),
		dataTokWS(Pos{4, 1, 5}, Ident, Pos{5, 1, 6}, "a_b_c"),
		dataTokWS(Pos{10, 1, 11}, Ident, Pos{11, 1, 12}, "a12"),
		dataTokWS(Pos{14, 1, 15}, Ident, Pos{15, 1, 16}, "अखिल"),
		single(nil, EOF, Pos{19, 1, 20}),
	})

	run("numbers.txt", "0 1 1.2 0.3 1.2e3 1.2e+3 1.2e-3 0x1 0xFB 0x1.3p1 0b01001 0o777 0o755", []Token{
		dataTok(nil, Number, Pos{0, 1, 1}, "0"),
		dataTokWS(Pos{1, 1, 2}, Number, Pos{2, 1, 3}, "1"),
		dataTokWS(Pos{3, 1, 4}, Number, Pos{4, 1, 5}, "1.2"),
		dataTokWS(Pos{7, 1, 8}, Number, Pos{8, 1, 9}, "0.3"),
		dataTokWS(Pos{11, 1, 12}, Number, Pos{12, 1, 13}, "1.2e3"),
		dataTokWS(Pos{17, 1, 18}, Number, Pos{18, 1, 19}, "1.2e+3"),
		dataTokWS(Pos{24, 1, 25}, Number, Pos{25, 1, 26}, "1.2e-3"),
		dataTokWS(Pos{31, 1, 32}, Number, Pos{32, 1, 33}, "0x1"),
		dataTokWS(Pos{35, 1, 36}, Number, Pos{36, 1, 37}, "0xFB"),
		dataTokWS(Pos{40, 1, 41}, Number, Pos{41, 1, 42}, "0x1.3p1"),
		dataTokWS(Pos{48, 1, 49}, Number, Pos{49, 1, 50}, "0b01001"),
		dataTokWS(Pos{56, 1, 57}, Number, Pos{57, 1, 58}, "0o777"),
		dataTokWS(Pos{62, 1, 63}, Number, Pos{63, 1, 64}, "0o755"),
		single(nil, EOF, Pos{68, 1, 69}),
	})

	run("strings_simple.txt", `"hello" "a\a\b\f\n\r\t\v\\\"b" "\o255" "\xFE" "\uABCD" "\U0010FFFF"`, []Token{
		dataTok(nil, String, Pos{0, 1, 1}, `"hello"`),
		dataTokWS(Pos{7, 1, 8}, String, Pos{8, 1, 9}, `"a\a\b\f\n\r\t\v\\\"b"`),
		dataTokWS(Pos{30, 1, 31}, String, Pos{31, 1, 32}, `"\o255"`),
		dataTokWS(Pos{38, 1, 39}, String, Pos{39, 1, 40}, `"\xFE"`),
		dataTokWS(Pos{45, 1, 46}, String, Pos{46, 1, 47}, `"\uABCD"`),
		dataTokWS(Pos{54, 1, 55}, String, Pos{55, 1, 56}, `"\U0010FFFF"`),
		single(nil, EOF, Pos{67, 1, 68}),
	})

	stringCases := []string{
		`"this {isn't} interpolated"`,
		`$"this {is} interpolated"`,
		`$"nested{{}}braces"`,
		`$"multiple{1}instances{2}interpolation"`,
		`"multiline
		string"`,
		`DEL"is " inside " quotes"DEL`,
		`$\"tabs are {printed} with '\t'"`,
		`$$"double {{braces}} and {single}"`,
	}

	run("strings.txt", strings.Join(stringCases, " "), []Token{
		dataTok(nil, String, Pos{0, 1, 1}, `"this {isn't} interpolated"`),
		dataTokWS(Pos{27, 1, 28}, StringBeg, Pos{28, 1, 29}, `$"this {`),
		dataTok(nil, Ident, Pos{36, 1, 37}, "is"),
		dataTok(nil, StringEnd, Pos{38, 1, 39}, `} interpolated"`),
		dataTokWS(Pos{53, 1, 54}, StringBeg, Pos{54, 1, 55}, `$"nested{`),
		single(nil, LeftBrace, Pos{63, 1, 64}),
		single(nil, RightBrace, Pos{64, 1, 65}),
		dataTok(nil, StringEnd, Pos{65, 1, 66}, `}braces"`),
		dataTokWS(Pos{73, 1, 74}, StringBeg, Pos{74, 1, 75}, `$"multiple{`),
		dataTok(nil, Number, Pos{85, 1, 86}, "1"),
		dataTok(nil, StringPart, Pos{86, 1, 87}, `}instances{`),
		dataTok(nil, Number, Pos{97, 1, 98}, "2"),
		dataTok(nil, StringEnd, Pos{98, 1, 99}, `}interpolation"`),
		{
			LeadingTrivia: []Token{{nil, Whitespace, unitSpan(Pos{113, 1, 114}), " "}},
			Type:          String,
			Span:          Span{Pos{114, 1, 115}, Pos{133, 2, 10}},
			Data:          `"multiline` + "\n\t\t" + `string"`,
		},
		dataTokWS(Pos{134, 2, 11}, String, Pos{135, 2, 12}, `DEL"is " inside " quotes"DEL`),
		dataTokWS(Pos{163, 2, 40}, StringBeg, Pos{164, 2, 41}, `$\"tabs are {`),
		dataTok(nil, Ident, Pos{177, 2, 54}, "printed"),
		dataTok(nil, StringEnd, Pos{184, 2, 61}, `} with '\t'"`),
		dataTokWS(Pos{196, 2, 73}, StringBeg, Pos{197, 2, 74}, `$$"double {{`),
		dataTok(nil, Ident, Pos{209, 2, 86}, "braces"),
		dataTok(nil, StringEnd, Pos{215, 2, 92}, `}} and {single}"`),
		single(nil, EOF, Pos{231, 2, 108}),
	})
}
