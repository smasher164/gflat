package codegen_test

import (
	"fmt"
	"io/fs"
	"testing"

	"github.com/smasher164/gflat/codegen"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
)

func Test(t *testing.T) {
	fsys := fsx.TestFS([][2]string{
		{"a/test.gf", `
		package a

		let x : int = 1 + 2 + 3
	`},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", "test.gf"); err != nil {
		t.Fatal(err)
	}
	tc := types2.NewChecker(importer)
	if err := tc.ProcessBuild(); err != nil {
		t.Fatal(err)
	}
	c := codegen.NewCodegen(importer, tc)
	outfs := fsx.TestFS(nil)
	c.CodegenBuild(outfs)
	sub, _ := fs.Sub(outfs, "a")
	b, err := fs.ReadFile(sub, "test.go")
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("%s\n", b)
	// fs.WalkDir(outfs, ".", func(path string, d fs.DirEntry, err error) error {
	// 	fmt.Println(path)
	// 	return nil
	// })
}
