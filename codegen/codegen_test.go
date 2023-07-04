package codegen_test

import (
	"fmt"
	"io/fs"
	"testing"

	"github.com/smasher164/gflat/codegen"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types"
)

func Test(t *testing.T) {
	tfs := fsx.TestFS([][2]string{
		{"a/a.gf", `let x = 3`},
	})
	imp := parser.NewImporter(tfs)
	if err := imp.ImportCrawl("a", "a.gf"); err != nil {
		t.Fatal(err)
	}
	r := types.NewResolver(imp)
	r.ResolveBuild()
	c := codegen.NewCodegen(imp)
	outfs := fsx.TestFS(nil)
	c.CodegenBuild(outfs)
	sub, _ := fs.Sub(outfs, "a")
	b, _ := fs.ReadFile(sub, "a.go")
	fmt.Printf("%s\n", b)
	// fs.WalkDir(outfs, ".", func(path string, d fs.DirEntry, err error) error {
	// 	fmt.Println(path)
	// 	return nil
	// })
}
