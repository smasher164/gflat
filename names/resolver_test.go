package names_test

import (
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/names"
	"github.com/smasher164/gflat/parser"
)

func Test(t *testing.T) {
	fsys := fstest.MapFS{
		"test.gf": &fstest.MapFile{
			// TODO: make this an UnknownIdent
			Data: []byte(`
			fun foo(x: 'a) => 1
			`),
			// Data: []byte(`
			// let a = 2
			// Foo ('a = 1)
			// `),
		},
	}
	// l, err := lexer.NewLexer("test.txt")
	f, err := parser.ParseFile(fsys, "test.gf")
	if err != nil {
		t.Fatal(err)
	}
	f = names.Resolve(f)
	parser.PrintAST(f)
}
