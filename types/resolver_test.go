package types_test

import (
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types"
)

func Test(t *testing.T) {
	fsys := fstest.MapFS{
		"a/test.gf": &fstest.MapFile{
			// TODO: make this an UnknownIdent
			// Data: []byte(`
			// fun foo(x: 'a) with Trait 'a => 1
			// `),
			// Data: []byte(`
			// let a = 2
			// Foo ('a = 1)
			// `),
			Data: []byte(`
			package a

			fun foo x => x
			`),
		},
	}
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", ""); err != nil {
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
