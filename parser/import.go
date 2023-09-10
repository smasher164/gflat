package parser

import (
	"fmt"
	"io/fs"
	"path/filepath"

	"github.com/samber/lo"
	"github.com/smasher164/gflat/ast"
)

type Importer struct {
	root     fs.FS
	PkgCache map[string]*ast.Package
	Sorted   []string
}

func NewImporter(root fs.FS) *Importer {
	return &Importer{
		root:     root,
		PkgCache: make(map[string]*ast.Package),
	}
}

func (i *Importer) importCrawl(path, scriptFile string) (err error) {
	pkg, err := i.ImportSingle(path, scriptFile)
	if err != nil {
		return err
	}
	pkg.(*ast.Package).Imports.Each(func(path string) {
		if _, ok := i.PkgCache[path]; !ok {
			if err = i.importCrawl(path, ""); err != nil {
				return
			}
		}
	})
	if err != nil {
		return err
	}
	i.Sorted = append(i.Sorted, path)
	return nil
}

func (i *Importer) checkCycle() error {
	pos := make(map[string]int)
	for idx, path := range i.Sorted {
		pos[path] = idx
	}
	var err error
	for path, pkg := range i.PkgCache {
		pkg.Imports.Each(func(dep string) {
			if pos[path] <= pos[dep] {
				// TODO: print the full path in the cycle
				err = fmt.Errorf("import cycle detected: %s -> %s", path, dep)
				return
			}
		})
	}
	return err
}

// ImportCrawl imports a package and all its dependencies.
// It returns the root package.
// If a dependency is already imported, it will be skipped.
// If there is a dependency cycle, an error will be returned.
func (i *Importer) ImportCrawl(path, scriptFile string) error {
	if err := i.importCrawl(path, scriptFile); err != nil {
		return err
	}
	return i.checkCycle()
}

func (i *Importer) ImportSingle(path, scriptFile string) (pkg ast.Node, err error) {
	if pkg, ok := i.PkgCache[path]; ok {
		return pkg, nil
	}
	pkgfs, err := fs.Sub(i.root, path)
	if err != nil {
		return pkg, err
	}
	entries, err := fs.ReadDir(pkgfs, ".")
	if err != nil {
		return pkg, err
	}
	scriptFileFound := false
	fileCount := 0
	filenames := lo.FilterMap(entries, func(entry fs.DirEntry, i int) (string, bool) {
		name := entry.Name()
		if filepath.Ext(name) != ".gf" {
			return "", false
		}
		fileCount++
		if name == scriptFile {
			scriptFileFound = true
			return "", false
		}
		return name, true
	})
	if fileCount == 0 {
		return pkg, fmt.Errorf("no .gf files found in package %s", path)
	}
	if !scriptFileFound && scriptFile != "" {
		return pkg, fmt.Errorf("script file %s not found in package %s", scriptFile, path)
	}
	pkg, err = ParsePackage(path, pkgfs, scriptFile, filenames...)
	if err != nil {
		return pkg, err
	}
	i.PkgCache[path] = pkg.(*ast.Package)
	return pkg, nil
}
