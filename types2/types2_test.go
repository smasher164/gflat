package types2_test

import (
	"testing"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
)

func TestInferExpression(t *testing.T) {
	fsys := fsx.TestFS([][2]string{
		{"a/test.gf", `
		package a

		let x = if (true) 1 else 2
	`},
	})
	importer := parser.NewImporter(fsys)
	if err := importer.ImportCrawl("a", ""); err != nil {
		t.Fatal(err)
	}
	tc := types2.NewChecker(importer)
	if err := tc.ProcessBuild(); err != nil {
		t.Fatal(err)
	}
	for _, pkg := range importer.PkgCache {
		ast.PrintAST(pkg)
	}
}
