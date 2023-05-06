package parser_test

import (
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/parser"

	"github.com/smasher164/gflat/lexer"
)

func Test(t *testing.T) {
	l, err := lexer.NewLexer("test.txt", fstest.MapFS{
		"test.txt": &fstest.MapFile{
			// Data: []byte(`;`),
			// Data: []byte(`let x = if (true) {
			// 	1
			// } else {
			// 	2
			// };`),
			// Data: []byte(`fun (a, b) : int -> {3}`),
			// Data: []byte(`
			// let eval = fun x -> if (x)
			// | (x,y): Add -> eval x + eval y,
			// | (x,y): Mul -> eval x * eval y,
			// | x:Num -> x;
			// `),
			// Data: []byte(`
			// let retF = fun (x) : fun int -> int -> x;
			// `),
			// Data: []byte(`
			// type A = (
			// 	f: io.Reader,
			// )
			// `),
			// Data: []byte(`
			// f $"ab {1} cd {2} ef"
			// `),
			// Data: []byte(`
			// if (true) {
			// 	1
			// }
			// foo();
			// `),
			// Data: []byte(`x = y = z? = 2 ** 3 ** 4`),
			// Data: []byte(`
			// let x = if (x)
			// 	| A -> {1
			// 		| 2}
			// 	| B -> 2
			// print x
			// `),
			Data: []byte(`
			type A = (
				a: Foo,
				'TypeVar,
				fun int -> int,
			)
			`),
			// Data: []byte(`
			// let x = (
			// 	a,
			// 	b,
			// )
			// `),
			// Data: []byte(`
			// let x = if(x)
			// 	| A -> 1,
			// 	| B -> 2,
			// print x
			// `),
			// Data: []byte(`
			// type A =
			// 	| A 'a
			// 	| B (x: 'a, y: 'b)
			// `),
		}})
	if err != nil {
		t.Fatal(err)
	}
	f := parser.ParseFile(l)
	parser.PrintAST(f)
}

// TODO: comma after last match arm

// commas in match arm and tuple
// so we need to weave in ShouldInsertDelim all throughout the parser
// we also need to look at where we parse commas and also accept terminators
