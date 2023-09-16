package types3_test

import (
	"testing"

	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types3"
)

func fatal(t *testing.T) func(err error) {
	return func(err error) {
		if err != nil {
			t.Fatal(err)
		}
	}
}

func Test(t *testing.T) {
	f := fatal(t)
	fsys := fsx.TestFS([][2]string{
		{"a.gf", `
		let f x = g x
		let g x = f x
		`},
	})
	importer := parser.NewImporter(fsys)
	f(importer.ImportCrawl(".", "a.gf"))
	chk := types3.NewChecker(importer)
	f(chk.Check())
	// print env
}
