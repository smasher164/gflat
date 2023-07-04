package codegen

import (
	"fmt"
	"io/fs"
	"os"

	"github.com/smasher164/gflat/fsx"
	"github.com/smasher164/gflat/lexer"
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types"
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
}

func NewCodegen(importer *parser.Importer) *Codegen {
	return &Codegen{
		importer: importer,
	}
}

// TODO: key the build by hash of inputs (basic incremental compilation)
func (c *Codegen) CodegenBuild(outfs fs.FS) {
	for _, path := range c.importer.Sorted {
		path := path
		pkg := c.importer.PkgCache[path].(types.ResolvedPackage)
		pkgdir, err := fsx.Mkdir(outfs, path, 0)
		if err != nil {
			panic(err) // handle error
		}
		c.Codegen(pkgdir, pkg) // there should be no errors
		// one big source file for now
		// all in memory.
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

func (c *Codegen) Codegen(outfs fs.FS, n parser.Node) {
	switch n := n.(type) {
	case types.ResolvedPackage:
		pkg := n.OriginalPackage.(parser.Package)
		for _, script := range pkg.ScriptFiles {
			c.Codegen(outfs, script) // just script files for now
		}
	case parser.File:
		// check that it's a script file for now
		if n.Package.Type == lexer.Package {
			// generate a file with the same name as the script file
			// with a "go:build ignore" comment
			// and the contents of the script file
			filename := withoutExt(n.Filename) + ".go"
			f, err := fsx.Create(outfs, filename)
			if err != nil {
				panic(err) // handle error
			}
			defer f.Close()
			fmt.Fprintf(f, "//go:build ignore\n\npackage main\n\n")
			for imp := range n.Imports {
				fmt.Fprintf(f, "import %q\n", imp)
			}
			fmt.Fprintf(f, "\nfunc main() {\n")
			c.CodegenExpr(f, n.Body)
			fmt.Fprintf(f, "}\n")
		}
	}
}

func (c *Codegen) CodegenExpr(f fsx.WriteableFile, n parser.Node) {
	switch n := n.(type) {
	case parser.LetDecl:
		_ = n
	}
}
