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
			Data: []byte(`x = y = z? = 2 ** 3 ** 4`),
		}})
	if err != nil {
		t.Fatal(err)
	}
	f := parser.ParseFile(l)
	parser.PrintAST(f)
}
