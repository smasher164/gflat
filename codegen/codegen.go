package codegen

import (
	"io/fs"

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
		c.Codegen(pkg) // there should be no errors
		// one big source file for now
		// all in memory.
	}
}

func (c *Codegen) Codegen(outfs fs.FS, n parser.Node) {
	switch n := n.(type) {
	case types.ResolvedPackage:
		pkg := n.OriginalPackage.(parser.Package)
		for _, script := range pkg.ScriptFiles {

			c.Codegen(script) // just script files for now
		}
	case parser.File:
		// what about hierarchy? you've mapped filename to filename, but what about subpkg to subpkg?
	}
}
