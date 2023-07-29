package codegen_test

import (
	"fmt"
	"io/fs"
	"strings"
	"testing"

	"github.com/smasher164/gflat/codegen"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
)

func Test(t *testing.T) {
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
		{"b/test01.gf", `
		package b

		let Foo = 1
		`},
		{"a/test19.gf", `
		package a

		import "b"

		let y = {
			b.Foo
		}
		`},
		{"a/test20.gf", `
		package a

		let z : (a: int, b: int) = (2, 4)
		let z2 = z.b
		`},
		{"a/test21.gf", `
		package a

		let z3 = (a = 2, b = 4)
		let z4 = z3.a
		`},
		{"b/test02.gf", `
		package b

		type A =
			| B
			| C int
			| D (a: int, b: int)

		let x = A.D (a = 2, b = 4)
		`},
		{"b/test03.gf", `
		package b

		type E =
			| F
			| G (x: int)

		let e = E.G (x = 3)
		`},
		{"b/test04.gf", `
		package b

		let q : (a: int) = 3
		`},

		// {"a/test9.gf", `
		// package a

		// let x = {
		// 	let y = 1
		// 	let z = 2
		// 	y + z
		// }
		// `},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", ""); err != nil {
		t.Fatal(err)
	}
	tc := types2.NewChecker(importer)
	if err := tc.ProcessBuild(); err != nil {
		t.Fatal(err)
	}
	c := codegen.NewCodegen(importer, tc)
	outfs := fsx.TestFS(nil)
	c.CodegenBuild(outfs)
	// sub, _ := fs.Sub(outfs, "a")
	// b, err := fs.ReadFile(sub, "test.go")
	// if err != nil {
	// 	t.Fatal(err)
	// }
	// fmt.Printf("%s\n", b)
	err := fs.WalkDir(outfs, ".", func(path string, d fs.DirEntry, err error) error {
		if d.IsDir() {
			return nil
		}
		b, err := fs.ReadFile(outfs, path)
		if err != nil {
			return err
		}
		s := fmt.Sprintf("path: %s", path)
		fmt.Println(s)
		fmt.Println(strings.Repeat("=", len(s)))
		fmt.Printf("%s\n", b)
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}
