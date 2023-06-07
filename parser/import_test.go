package parser_test

import (
	"fmt"
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/parser"
)

func TestImport(t *testing.T) {
	fsys := fstest.MapFS{
		"a/a.gf": &fstest.MapFile{
			Data: []byte(`
			package a

			import "c"
			import "b"
			`),
		},
		"b/b.gf": &fstest.MapFile{
			Data: []byte(`
			package b

			import "c"
			`),
		},
		"c/c.gf": &fstest.MapFile{
			Data: []byte(`
			package c

			fun Foo() => 1
			`),
		},
	}
	importer := parser.NewImporter(fsys)
	fmt.Println(importer.ImportCrawl("a", ""))
}
