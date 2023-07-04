package types_test

import (
	"testing"

	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types"
)

func TestInferExpression(t *testing.T) {
	fsys := fsx.TestFS([][2]string{
		{"a/test.gf", `
		let s = "s"
		s+s
	`},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", "test.gf"); err != nil {
		t.Fatal(err)
	}
	r := types.NewResolver(importer)
	r.ResolveBuild()
	for _, pkg := range importer.PkgCache {
		parser.PrintAST(pkg)
	}
}

func Test(t *testing.T) {
	fsys := fsx.TestFS([][2]string{
		{"a/test.gf", `
		fun foo x => x
		`},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", "test.gf"); err != nil {
		t.Fatal(err)
	}
	r := types.NewResolver(importer)
	r.ResolveBuild()
	// if err := r.ResolveBuild(); err != nil {
	// 	t.Fatal(err)
	// }
	// for _, path := range importer.Sorted {
	// 	importer.PkgCache[path]
	// }
	// // l, err := lexer.NewLexer("test.txt")
	// f, err := parser.ParseFile(fsys, "test.gf")
	// if err != nil {
	// 	t.Fatal(err)
	// }
	// r := names.NewResolver()
	// f = r.Resolve(names.Universe, f)
	// parser.PrintAST(f)
}
