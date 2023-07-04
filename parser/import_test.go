package parser_test

import (
	"fmt"
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/parser"
)

func TestImport(t *testing.T) {
	fsys := fstest.MapFS{
		"a/a1/a.gf": &fstest.MapFile{
			Data: []byte(`
			package a1

			import "a/a3"
			import "a/a2"
			`),
		},
		"a/a2/b.gf": &fstest.MapFile{
			Data: []byte(`
			package a2

			import "a/a3"
			`),
		},
		"a/a3/c.gf": &fstest.MapFile{
			Data: []byte(`
			package a3

			fun Foo() => 1
			`),
		},
	}
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a/a1", ""); err != nil {
		t.Fatal(err)
	}
	fmt.Println(importer.PkgCache["a/a1"].ASTString(0))
}
