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
		{"a/test1.gf", `
		package a

		let x : int = 1 + 2 + 3
		`},
		{"a/test2.gf", `
		package a

		let x : int = { 1 }
		`},
		{"a/test3.gf", `
		package a

		let x = 1 + 2 + 3
		`},
		{"a/test4.gf", `
		package a

		let x = "hello " + "world"
		`},
		{"a/test5.gf", `
		package a

		let x = 1 < 2
		`},
		{"a/test6.gf", `
		package a

		let x = true && false
		`},
		{"a/test7.gf", `
		package a

		let x = if (true) 1 else 2
		`},
		// {"a/test8.gf", `
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
