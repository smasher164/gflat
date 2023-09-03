package types3

import (
	"errors"

	"github.com/smasher164/gflat/ast"
	"github.com/smasher164/gflat/parser"
)

/*
Requirements:
1. Resolve: Node -> ()
2. It can also call Infer internally. Generate fresh type variables for things you don't know.
3. Per-package checker.
*/

func NewChecker(importer *parser.Importer) *Checker {
	return &Checker{}
}

type Checker struct {
	Importer   *parser.Importer
	PkgChecker map[string]*PackageChecker
	err        error
}

func (c *Checker) Check() error {
	for _, path := range c.Importer.Sorted {
		c.err = errors.Join(c.err, c.CheckPackage(path))
	}
	return c.err
}

type PackageChecker struct {
	pkg *ast.Package

	// access module exports and stuff
	c *Checker
}

func NewPackageChecker(pkg *ast.Package, c *Checker) *PackageChecker {
	return &PackageChecker{pkg: pkg, c: c}
}

func (pc *PackageChecker) Check() error {
	pc.pkg.
}

func (c *Checker) CheckPackage(importPath string) error {
	pc := NewPackageChecker(c.Importer.PkgCache[importPath], c)
	c.PkgChecker[importPath] = pc
	return pc.Check()
}

/*
If you repurpose Env for module exports, then how do you have a separate package checker?
So when resolving an import, you need to be able to reference the imported package.

How to add global definitions?
How to deal with package vs file scope?
*/
