package codegen

import (
	"fmt"
	"io/fs"
	"os"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types2"
)

// how should the output directory be mocked?
/*
package -> package
script file -> "go:build ignore" file in that same package directory
filename.gf -> filename.go (i think)
package -> package
package/subpackage -> package/subpackage
don't rely on concurrent writes right now, until the in-memory fs can be made thread-safe
*/

type Codegen struct {
	// assumes we have a resolved and typed build
	importer *parser.Importer
	checker  *types2.Checker
}

func NewCodegen(importer *parser.Importer, checker *types2.Checker) *Codegen {
	return &Codegen{
		importer: importer,
		checker:  checker,
	}
}

// TODO: key the build by hash of inputs (basic incremental compilation)
func (c *Codegen) CodegenBuild(outfs fs.FS) {
	for _, path := range c.importer.Sorted {
		path := path
		pkg := c.importer.PkgCache[path]
		pkgdir, err := fsx.Mkdir(outfs, path, 0)
		if err != nil {
			panic(err) // handle error
		}
		c.codegen(pkgdir, pkg) // there should be no errors
	}
}

func withoutExt(path string) string {
	for i := len(path) - 1; i >= 0 && !os.IsPathSeparator(path[i]); i-- {
		if path[i] == '.' {
			return path[:i]
		}
	}
	return ""
}

func (c *Codegen) codegen(outfs fs.FS, x ast.Node) {
	switch x := x.(type) {
	case *ast.Package:
		for _, file := range x.PackageFiles {
			c.codegen(outfs, file)
		}
		if x.ScriptFile != nil {
			c.codegen(outfs, x.ScriptFile)
		}
	case *ast.File:
		filename := withoutExt(x.Filename) + ".go"
		f, err := fsx.Create(outfs, filename)
		if err != nil {
			panic(err) // handle error
		}
		defer f.Close()
		if x.PackageName != nil {
			fmt.Fprintf(f, "package %s\n\n", x.PackageName.Name.Data)
		} else {
			fmt.Fprintf(f, "//go:build ignore\n\npackage main\n\n")
		}
		for imp := range x.Imports {
			fmt.Fprintf(f, "import %q\n", imp)
		}
		if x.PackageName != nil {
			c.codegenExprTopLevel(f, x.Body)
		} else {
			fmt.Fprintf(f, "\nfunc main() {\n")
			c.codegenExpr(f, x.Body)
			fmt.Fprintf(f, "}\n")
		}
	}
}

func (c *Codegen) codegenExprTopLevel(f fsx.WriteableFile, x ast.Node) {
	switch x := x.(type) {
	case *ast.Block:
		for _, elem := range x.Body {
			c.codegenExpr(f, elem)
		}
	}
}

func (c *Codegen) codegenExpr(f fsx.WriteableFile, x ast.Node) []string {
	switch x := x.(type) {
	case *ast.Block:
		tblk := c.checker.GetType(x)
		tmp := c.checker.FreshName()                       // TODO: also add to env
		fmt.Fprintf(f, "var %s %s\n", tmp.Name.Data, tblk) // TODO: print go type
		// blocks are weird, cause they can be used in expression position.
		fmt.Fprintf(f, "{\n")
		for i, elem := range x.Body {
			vars := c.codegenExpr(f, elem)
			if i == len(x.Body)-1 {
				if _, isStmt := elem.(*ast.Stmt); !isStmt {
					fmt.Fprintf(f, "%s = %s\n", tmp, vars[0])
				}
			}
		}
		fmt.Fprintf(f, "_ = %s\n", tmp)
		fmt.Fprintf(f, "}\n")
		return []string{tmp.Name.Data}
	case *ast.Stmt:
		c.codegenExpr(f, x.Stmt)
		fmt.Fprintf(f, "\n")
		return []string{"_"}
	case *ast.LetDecl:
		vars := c.codegenExpr(f, x.Rhs)
		tlet := c.checker.GetType(x.Destructure)
		switch des := x.Destructure.(type) {
		case *ast.Ident:
			fmt.Fprintf(f, "var %s %s = %s\n", des.Name.Data, tlet, vars[0])
		case *ast.TypeAnnotation:
			if varname, ok := des.Destructure.(*ast.Ident); ok {
				fmt.Fprintf(f, "var %s %s = %s\n", varname.Name.Data, tlet, vars[0])
			}
		}
		return []string{"_"}
	case *ast.BinaryExpr:
		lvars := c.codegenExpr(f, x.Left)
		rvars := c.codegenExpr(f, x.Right)
		tbin := c.checker.GetType(x)
		bvar := c.checker.FreshName()
		switch x.Op.Type {
		case lexer.Plus:
			fmt.Fprintf(f, "var %s %s = %s + %s\n", bvar.Name.Data, tbin, lvars[0], rvars[0])
		}
		return []string{bvar.Name.Data}
	case *ast.Number:
		nvar := c.checker.FreshName()
		fmt.Fprintf(f, "var %s %s = %s\n", nvar.Name.Data, c.checker.GetType(x), x.Lit.Data)
		return []string{nvar.Name.Data}
	}
	panic(fmt.Sprintf("unhandled node: %T", x))
}
