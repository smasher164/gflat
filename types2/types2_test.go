package types2_test

import (
	"testing"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
)

func TestInferExpression(t *testing.T) {
	fsys := fsx.TestFS([][2]string{
		{"a/test01.gf", `
		package a

		let a : int = 1 + 2 + 3
		`},
		{"a/test02.gf", `
		package a

		let b : int = { 1 }
		`},
		{"a/test03.gf", `
		package a

		let c = 1 + 2 + 3
		`},
		{"a/test04.gf", `
		package a

		let d = "hello " + "world"
		`},
		{"a/test05.gf", `
		package a

		let e = 1 < 2
		`},
		{"a/test06.gf", `
		package a

		let f = true && false
		`},
		{"a/test07.gf", `
		package a

		let g = if (true) 1 else 2
		`},
		{"a/test08.gf", `
		package a

		let h = {
			if (true) 1 else 2
		}
		`},
		{"a/test09.gf", `
		package a

		let i = if (true) { 1 } else { 2 }
		`},
		{"a/test10.gf", `
		package a

		let j = if (true) {
			1;
		}
		`},
		{"a/test11.gf", `
		package a

		type foo = int
		type bar = int

		let k : foo = 0
		let l : bar = 1
		`},
		{"a/test12.gf", `
		package a

		let m : int = 0
		let n = m
		`},
		{"a/test13.gf", `
		package a

		let o : (int, int) = (0, 0)
		`},
		{"a/test14.gf", `
		package a

		type Foo = (int, int)
		let p : Foo = (0, 0)
		`},
		{"a/test15.gf", `
		package a

		let q = (2, 4)[0]
		`},
		{"a/test16.gf", `
		package a

		let r : (a: int, b: int) = (2, 4)
		let s = r[0]
		`},
		{"a/test17.gf", `
		package a

		let t : (a: int, b: int) = (2, 4)
		let u = if (true) t else (1, 2)
		let v = if (true) (1, 2) else t
		`},
		{"a/test18.gf", `
		package a

		let w = ((1, 2), 3)
		let x = w[0][1]
		`},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", ""); err != nil {
		t.Fatal(err)
	}
	tc := types2.NewChecker(importer)
	if err := tc.ProcessBuild(); err != nil {
		t.Fatal(err)
	}
	for _, pkg := range importer.PkgCache {
		ast.PrintAST(pkg)
	}
}
