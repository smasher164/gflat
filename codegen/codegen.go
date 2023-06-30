package codegen

import (
	"github.com/smasher164/gflat/parser"
	"github.com/smasher164/gflat/types"
)

type Codegen struct {
	// assumes we have a resolved and typed build
	importer *parser.Importer
	build    map[string][]byte
}

func NewCodegen(importer *parser.Importer) *Codegen {
	return &Codegen{
		importer: importer,
		build:    make(map[string][]byte),
	}
}

// TODO: key the build by hash of inputs (basic incremental compilation)
func (c *Codegen) CodegenBuild() {
	for _, path := range c.importer.Sorted {
		path := path
		pkg := c.importer.PkgCache[path].(types.ResolvedPackage)
		c.build[path] = c.CodegenPackage(pkg) // there should be no errors
		// one big source file for now
		// all in memory.
	}
}

func (c *Codegen) CodegenPackage(pkg types.ResolvedPackage) []byte {

}
