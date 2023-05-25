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
			Data: []byte(`
			type A = 
				| B
				| C
			if (1)
			| A.B x => x
			| A.C => 2
			`),
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
