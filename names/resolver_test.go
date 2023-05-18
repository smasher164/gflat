package names_test

import (
	"testing"
	"testing/fstest"

	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/names"
	"github.com/smasher164/gflat/parser"
)

func Test(t *testing.T) {
	l, err := lexer.NewLexer("test.txt", fstest.MapFS{
		"test.txt": &fstest.MapFile{
			Data: []byte(`
			let foo () = bar ()
			let x = (foo)
			let bar () = 1
			`),
		}})
	if err != nil {
		t.Fatal(err)
	}
	f := parser.ParseFile(l)
	f = names.Resolve(f)
	parser.PrintAST(f)
}
