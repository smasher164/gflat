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
			// Data: []byte(`
			// type A = (
			// 	a: Foo,
			// 	'TypeVar,
			// 	fun int -> int,
			// )
			// `),
			// Data: []byte(`
			// let x = (
			// 	a,
			// 	b,
			// )
			// `),
			// Data: []byte(`
			// let x = if(x)
			// 	| A => 1,
			// 	| x : fun int -> int => 2,
			// print x
			// `),
			// Data: []byte(`
			// type A =
			// 	| A 'a
			// 	| B (x: 'a, y: 'b)
			// `),
			// Data: []byte(`
			// let f x = {
			// 	1
			// }
			// `),
			// Data: []byte(`
			// if (true)
			// {
			// 	1
			// }
			// `),
			// Data: []byte(`
			// let x = (1, 2, 3, 4)
			// print x
			// `),
			// Data: []byte(`
			// ("a" = 2)["a"]
			// `),
			// Data: []byte(`
			// import sort2 = "sort"
			// `),
			// Data: []byte(`
			// import (
			// 	"github.com/someone/math"
			// 	alias = "github.com/someone/math"
			// 	(Sqrt, Abs) = "github.com/someone/math"
			// )
			// `),
			// Data: []byte(`
			// let foo x : int -> int = x
			// `),
			// Data: []byte(`
			// package main
			// {

			// }
			// impl Eq []'a where Show 'a = (
			// 	Eq = fun (x, y) -> bool => x == y,
			// )
			// `),
			// Data: []byte(`
			// foo where 'a = 3
			// `),
			// Data: []byte(`
			// let x : []int = (1, 2, 3)
			// let y = x[0]
			// print y
			// `),
			Data: []byte(`
			type Foo 'a
			print Foo
			`),
		}})
	if err != nil {
		t.Fatal(err)
	}
	f := parser.ParseFile(l)
	parser.PrintAST(f)
}
