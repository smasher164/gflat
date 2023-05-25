package lexer

import (
	"bufio"
	"fmt"
	"io"
	"io/fs"
	"path/filepath"
	"sort"
	"strconv"
	"unicode"

	"github.com/smasher164/xid"
	"golang.org/x/exp/slices"
)

type interpFrame struct {
	dollarCount      int
	nestedBraceCount int
	escaped          bool
	delim            []rune
}

type Lexer struct {
	prev               Token
	emittedSemi        bool
	shouldInsertBefore bool
	shouldInsertAfter  bool
	beforeToksToCheck  []TokenType
	afterToksToCheck   []TokenType
	ch                 rune
	pos                int
	i                  int // position in buffer
	err                error
	lexStringNext      bool
	buf                []rune
	rdr                *bufio.Reader
	stringStack        []interpFrame
	lines              []int
}

const eof = -1

func (l *Lexer) lexWS() Token {
	startPos := l.pos
	for unicode.IsSpace(l.ch) {
		l.next()
	}
	return Token{Type: Whitespace, Span: l.spanOf(startPos, l.pos-1), Data: l.bufString()}
}

func isLetter(ch rune) bool {
	// return ch == '_' || unicode.IsLetter(ch)
	return ch == '_' || xid.Start(ch)
}

func (l *Lexer) lexIdentOrKeyword(justIdent bool) Token {
	startPos := l.pos
	l.next()
	for xid.Continue(l.ch) {
		l.next()
	}
	ident := l.bufString()
	if !justIdent {
		if ttyp, ok := Keywords[ident]; ok {
			return Token{Type: ttyp, Span: l.spanOf(startPos, l.pos-1)}
		}
		if l.ch == '"' {
			return l.lexString([]rune(ident))
		}
	}
	return Token{Type: Ident, Span: l.spanOf(startPos, l.pos-1), Data: ident}
}

// nesting should be 1 by default.
// if it reaches 0, we are parsing the closing braces inside
// string interpolation.
// if it is greater than 1, we are inside a nested expression
// in string interpolation.

func isDecimal(ch rune) bool { return '0' <= ch && ch <= '9' }
func isHex(ch rune) bool {
	return '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F'
}
func isValidDigit(base int, ch rune) bool {
	switch base {
	case 2, 8, 10:
		return ch >= '0' && ch < rune('0'+base)
	default:
		return isHex(ch)
	}
}

func (l *Lexer) lexDigits(err *Token, _allowed bool, base int) (digitCount int) {
	setErr := func(pos int, msg string) {
		if err.Type != Illegal {
			*err = Token{Type: Illegal, Span: l.spanOf(pos, pos), Data: msg}
		}
	}
	for {
		if l.ch == '_' {
			if _allowed {
				_allowed = !_allowed
			} else {
				setErr(l.pos, "'_' must separate successive digits")
			}
		} else if base == 10 && l.ch == 'e' || l.ch == 'E' {
			if !_allowed {
				setErr(l.pos-1, "'_' must separate successive digits")
			}
			return digitCount
		} else if isHex(l.ch) {
			_allowed = true
			digitCount++
			if !isValidDigit(base, l.ch) {
				setErr(l.pos, fmt.Sprintf("%q is not a valid digit in base %d", l.ch, base))
			}
		} else {
			if !_allowed {
				setErr(l.pos-1, "'_' must separate successive digits")
			}
			return digitCount
		}
		l.next()
	}
}

func (l *Lexer) lexNumber() Token {
	var (
		startPos   = l.pos
		base       = 10
		isFloat    = false
		digitCount = 0
		tok        Token
	)
	setErr := func(msg string) {
		if tok.Type != Illegal {
			tok = Token{Type: Illegal, Span: l.spanOf(startPos, l.pos), Data: msg}
		}
	}
	if l.ch != '.' {
		_allowed := false
		if l.ch == '0' {
			l.next()
			_allowed = true
			switch l.ch {
			case 'x':
				l.next()
				base = 16
			case 'o':
				l.next()
				base = 8
			case 'b':
				l.next()
				base = 2
			}
		}
		digitCount += l.lexDigits(&tok, _allowed, base)
	}
	if l.ch == '.' {
		if l.peek() == '.' {
			return Token{Type: Number, Span: l.spanOf(startPos, l.pos-1), Data: l.bufString()}
		}
		isFloat = true
		switch base {
		case 2:
			setErr("binary numbers cannot have a decimal point")
		case 8:
			setErr("octal numbers cannot have a decimal point")
		}
		l.next()
		digitCount += l.lexDigits(&tok, false, base)
		if digitCount == 0 {
			setErr("no digits in number")
		}
	}
	switch l.ch {
	case 'e', 'E', 'p', 'P':
		switch {
		case (l.ch == 'e' || l.ch == 'E') && base != 10:
			setErr(fmt.Sprintf("%q exponent requires decimal mantissa", l.ch))
		case (l.ch == 'p' || l.ch == 'P') && base != 16:
			setErr(fmt.Sprintf("%q exponent requires hexadecimal mantissa", l.ch))
		}
		l.next()
		if l.ch == '+' || l.ch == '-' {
			l.next()
		}
		// consider requiring hex exponent for hex float
		if count := l.lexDigits(&tok, false, 16); count == 0 {
			setErr("no digits in exponent")
		}
	default:
		if base == 16 && isFloat {
			setErr("hexadecimal mantissa requires 'p' exponent")
		}
	}
	if tok.Type == Illegal {
		return tok
	}
	return Token{Type: Number, Span: l.spanOf(startPos, l.pos-1), Data: l.bufString()}
}

func (l *Lexer) lexLineComment() Token {
	startPos := l.pos
	l.until('\n')
	endPos := l.pos
	l.next()
	return Token{Type: SingleLineComment, Span: l.spanOf(startPos, endPos), Data: l.bufString()}
}

func (l *Lexer) lexEscape() string {
	var n int
	var base, max uint32
	switch l.ch {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '"':
		l.next()
		return ""
	case 'o':
		l.next()
		n, base, max = 3, 8, 255
	case 'x':
		l.next()
		n, base, max = 2, 16, 255
	case 'u':
		l.next()
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		l.next()
		n, base, max = 8, 16, unicode.MaxRune
	default:
		if l.ch == eof {
			return "escape sequence not terminated"
		}
		l.next()
		return "unknown escape sequence"
	}

	var x uint32
	for n > 0 {
		d, err := strconv.ParseInt(string(l.ch), int(base), 8)
		if err != nil {
			if l.ch == eof {
				return "escape sequence not terminated"
			}
			msg := fmt.Sprintf("illegal character %#U in escape sequence", l.ch)
			l.next()
			return msg
		}
		x = x*base + uint32(d)
		l.next()
		n--
	}

	if x > max || 0xD800 <= x && x < 0xE000 {
		return "escape sequence is invalid Unicode code point"
	}

	return ""
}

func (l *Lexer) lexStringPart(startPos int, fromLexString bool) Token {
	frame := l.stringStack[len(l.stringStack)-1] // we have a pointer here. make sure we don't invalidate it.
	// lex until we find a closing quote or an interpolation
	// if it's a closing quote, the token is a string, otherwise it's a string part.
	for {
		switch l.ch {
		case eof:
			return Token{Type: Illegal, Span: l.spanOf(startPos, l.pos), Data: "unterminated string"}
		case '"':
			l.next()
			if slices.Equal(l.checkN(len(frame.delim)), frame.delim) {
				l.nextN(len(frame.delim))
				l.stringStack = l.stringStack[:len(l.stringStack)-1]
				ttyp := StringEnd
				if fromLexString {
					ttyp = String
				}
				return Token{Type: ttyp, Span: l.spanOf(startPos, l.pos-1), Data: l.bufString()}
			}
		case '\\':
			l.next()
			if frame.escaped {
				begPos := l.pos
				if msg := l.lexEscape(); msg != "" {
					endPos := l.pos - 1
					return Token{Type: Illegal, Span: l.spanOf(begPos, endPos), Data: msg}
				}
			}
		case '{':
			if frame.dollarCount == 0 || containsFunc(l.checkN(frame.dollarCount), func(r rune) bool { return r != '{' }) {
				l.next()
				continue
			}
			l.nextN(frame.dollarCount)
			ttyp := StringPart
			if fromLexString {
				ttyp = StringBeg
			}
			return Token{Type: ttyp, Span: l.spanOf(startPos, l.pos-1), Data: l.bufString()}
		default:
			l.next()
		}
	}
}

func containsFunc[E any](s []E, f func(E) bool) bool {
	for _, e := range s {
		if f(e) {
			return true
		}
	}
	return false
}

func (l *Lexer) next() {
	if l.ch == eof {
		return
	}
	l.i++
	l.pos++
	// update line number and offset
	if l.i < len(l.buf) {
		l.ch = l.buf[l.i]
	} else {
		r, _, err := l.rdr.ReadRune()
		if err != nil {
			l.ch = eof
			if err != io.EOF {
				l.err = err
			}
		} else {
			l.ch = r
		}
		l.buf = append(l.buf, l.ch)
	}
	if l.ch == '\n' {
		if len(l.lines) == 0 || len(l.lines) > 0 && l.lines[len(l.lines)-1] < l.pos {
			l.lines = append(l.lines, l.pos)
		}
	}
}

func (l *Lexer) backup() {
	if l.i > 0 {
		l.i--
		l.pos--
		l.ch = l.buf[l.i]
	}
}

func (l *Lexer) peek() rune {
	l.next()
	ch := l.ch
	l.backup()
	return ch
}

func (l *Lexer) checkN(n int) []rune {
	if n == 0 {
		return nil
	}
	oldch := l.ch
	dst := make([]rune, n)
	dst[0] = l.ch
	backupCount := -1
	for i := 1; i < n; i++ {
		l.next()
		dst[i] = l.ch
		if l.ch == eof && backupCount < 0 {
			backupCount = i
		}
	}
	if backupCount < 0 {
		backupCount = n - 1
	}
	if backupCount > 0 || backupCount == 0 && oldch != eof {
		for i := 0; i < backupCount; i++ {
			l.backup()
		}
	}
	return dst
}

func (l *Lexer) nextN(n int) {
	for i := 0; i < n; i++ {
		l.next()
	}
}

func (l *Lexer) slurp(r rune) (count int) {
	for l.ch == r {
		count++
		l.next()
	}
	return count
}

func (l *Lexer) until(r rune) (dst []rune) {
	for l.ch != r && l.ch != eof {
		dst = append(dst, l.ch)
		l.next()
	}
	return dst
}

func (l *Lexer) bufString() string {
	s := string(l.buf[:l.i])
	return s
}

// func (l *Lexer) Line(tok Token) int {
// 	// convert tok.Pos() to line
// 	pos := tok.Pos()
// 	line, _ := sort.Find(len(l.lines), func(i int) int {
// 		v := l.lines[i]
// 		if pos == v {
// 			return 0
// 		}
// 		if pos < v {
// 			return -1
// 		}
// 		return 1
// 	})
// 	if line == len(l.lines) {
// 		line = len(l.lines) - 1
// 	}
// 	return line + 1
// }

func (l *Lexer) lineIndex(offset int) int {
	line, found := sort.Find(len(l.lines), func(i int) int {
		v := l.lines[i]
		if offset == v {
			return 0
		}
		if offset < v {
			return -1
		}
		return 1
	})
	if found {
		return line
	}
	return line - 1
}

func (l *Lexer) posOf(offset int) Pos {
	line := l.lineIndex(offset)
	return Pos{Offset: offset, Line: line + 1, Column: offset - l.lines[line] + 1}
}

func (l *Lexer) spanOf(off1, off2 int) Span {
	start := l.posOf(off1)
	var end Pos
	if off1 == off2 {
		end = start
	} else {
		end = l.posOf(off2)
	}
	return Span{Start: start, End: end}
}

func (l *Lexer) resetPos() {
	l.buf = l.buf[l.i:]
	l.i = 0
	l.ch = l.buf[l.i]
}

func (l *Lexer) lexString(delim []rune) Token {
	startPos := l.pos
	frame := interpFrame{escaped: true}
	var tok Token
	setErr := func(msg string) {
		if tok.Type != Illegal {
			tok = Token{Type: Illegal, Span: l.spanOf(l.pos, l.pos), Data: msg}
		}
	}
	frame.dollarCount = l.slurp('$')
	if l.ch == '\\' {
		frame.escaped = false
		l.next()
		if count := l.slurp('$'); count > 0 {
			frame.dollarCount = count
			setErr("'$' must appear before '#' on string literal")
		}
	}
	if len(delim) > 0 {
		frame.delim = delim
		startPos -= len(delim)
	} else {
		frame.delim = l.until('"')
	}
	if containsFunc(frame.delim, func(r rune) bool { return !unicode.IsLetter(r) }) {
		setErr("delimiter must be composed of letters")
	}
	if l.ch != '"' {
		setErr("'$' and '#' must only appear before quotes \"")
		return tok
	}
	l.next()
	l.stringStack = append(l.stringStack, frame)
	return l.lexStringPart(startPos, true)
}

func (l *Lexer) lexTypeArg() Token {
	startPos := l.pos
	l.next()
	var id Token = l.lexIdentOrKeyword(true)
	if id.Type == Illegal {
		return Token{Type: Illegal, Span: l.spanOf(startPos, id.Span.End.Offset), Data: id.Data}
	}
	return Token{Type: TypeArg, Span: l.spanOf(startPos, id.Span.End.Offset), Data: id.Data}
}

func (l *Lexer) NextToken() Token {
	defer l.resetPos()
	startPos := l.pos
	switch {
	case len(l.stringStack) > 0 && l.lexStringNext:
		tok := l.lexStringPart(startPos, false)
		l.lexStringNext = false
		return tok
	// case l.ch == eof:
	// 	return Token{Type: EOF, Span: l.spanOf(startPos, l.pos)}
	case unicode.IsSpace(l.ch):
		return l.lexWS()
	case isLetter(l.ch):
		return l.lexIdentOrKeyword(false)
	case isDecimal(l.ch) || l.ch == '.' && isDecimal(l.peek()):
		return l.lexNumber()
	case l.ch == '\'':
		return l.lexTypeArg()
	case l.ch == '#':
		return l.lexLineComment()
	// TODO: do we need multiline (nested) comments?
	// what about block/doc comments?
	// case l.ch == '/' && l.peek() == '#':
	// 	return l.lexMultiLineComment()
	case l.ch == '\\' || l.ch == '"' || l.ch == '$' && (l.peek() == '\\' || l.peek() == '"' || l.peek() == '$'):
		return l.lexString(nil)
	case l.ch == '{':
		if len(l.stringStack) > 0 {
			l.stringStack[len(l.stringStack)-1].nestedBraceCount++
		}
		l.next()
		return Token{Type: LeftBrace, Span: l.spanOf(startPos, startPos)}
	case l.ch == '}':
		if len(l.stringStack) > 0 {
			frame := &l.stringStack[len(l.stringStack)-1]
			if frame.nestedBraceCount != 0 {
				frame.nestedBraceCount--
				l.next()
				return Token{Type: RightBrace, Span: l.spanOf(startPos, startPos)}
			}
			dollarCount := l.slurp('}')
			if dollarCount == frame.dollarCount {
				return l.lexStringPart(startPos, false)
			}
			endpos := l.pos - 1
			l.next()
			l.lexStringNext = true
			return Token{Type: Illegal, Span: l.spanOf(startPos, endpos), Data: "unmatched '}'"}
		}
		l.next()
		return Token{Type: RightBrace, Span: l.spanOf(startPos, startPos)}
	}
	if ttyp, ok := DoubleCharTokens[[2]rune{l.ch, l.peek()}]; ok {
		l.next()
		l.next()
		return Token{Type: ttyp, Span: l.spanOf(startPos, l.pos-1)}
	}
	if ttyp, ok := SingleCharTokens[l.ch]; ok {
		l.next()
		return Token{Type: ttyp, Span: l.spanOf(startPos, startPos)}
	}
	ch := l.ch
	l.next()
	return Token{Type: Illegal, Span: l.spanOf(startPos, startPos), Data: fmt.Sprintf("unexpected character %q", ch)}
}

func (l *Lexer) Next() Token {
	if l.emittedSemi {
		l.emittedSemi = false
		return l.prev
	}
	var t Token
	var trivia []Token
	for t = l.NextToken(); t.Type == Whitespace || t.Type == SingleLineComment; t = l.NextToken() {
		trivia = append(trivia, t)
	}
	t.LeadingTrivia = trivia
	if l.prev.OnDifferentLines(t) && l.prev.IsBeforeSemicolon(l.shouldInsertBefore, l.beforeToksToCheck) && t.IsAfterSemicolon(l.shouldInsertAfter, l.afterToksToCheck) {
		l.prev = t
		l.emittedSemi = true
		return Token{Type: LineTerminator, Span: Span{Start: t.Span.Start, End: t.Span.Start}}
	}
	l.prev = t
	return t
}

func (l *Lexer) ShouldInsertBefore(status bool, toksToCheck []TokenType) {
	l.shouldInsertBefore = status
	l.beforeToksToCheck = toksToCheck
}

func (l *Lexer) ShouldInsertAfter(status bool, toksToCheck []TokenType) {
	l.shouldInsertAfter = status
	l.afterToksToCheck = toksToCheck
}

func NewLexer(fsys fs.FS, filename string) (*Lexer, error) {
	if filepath.Ext(filename) != ".gf" {
		return nil, fmt.Errorf("invalid file extension %q, expected \".gf\"", filepath.Ext(filename))
	}
	f, err := fsys.Open(filename)
	if err != nil {
		return nil, err
	}
	rdr := bufio.NewReader(f)
	l := &Lexer{
		rdr:   rdr,
		i:     -1,
		pos:   -1,
		lines: []int{0},
	}
	l.next()
	return l, nil
}
