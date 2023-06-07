package parser

import (
	"fmt"
	"io/fs"
	"path/filepath"

	"github.com/samber/lo"
)

type Importer struct {
	root     fs.FS
	pkgCache map[string]Node
	sorted   []string
}

func NewImporter(root fs.FS) *Importer {
	return &Importer{
		root:     root,
		pkgCache: make(map[string]Node),
	}
}

func (i *Importer) importCrawl(path, scriptFile string) (err error) {
	pkg, err := i.importSingle(path, scriptFile)
	if err != nil {
		return err
	}
	{
		pkg := pkg.(Package)
		for path := range pkg.Imports {
			if _, ok := i.pkgCache[path]; !ok {
				if err = i.importCrawl(path, ""); err != nil {
					return err
				}
			}
		}
	}
	i.sorted = append(i.sorted, path)
	return nil
}

func (i *Importer) ImportCrawl(path, scriptFile string) error {
	// ImportCrawl imports a package and all its dependencies.
	// It returns the root package.
	// If a dependency is already imported, it will be skipped.
	// If there is a dependency cycle, an error will be returned.
	if err := i.importCrawl(path, scriptFile); err != nil {
		return err
	}
	pos := make(map[string]int)
	for idx, path := range i.sorted {
		pos[path] = idx
	}
	for path, pkg := range i.pkgCache {
		for dep := range pkg.(Package).Imports {
			if pos[path] <= pos[dep] {
				// TODO: print the full path in the cycle
				return fmt.Errorf("import cycle detected: %s -> %s", path, dep)
			}
		}
	}
	return nil
}

func (i *Importer) importSingle(path, scriptFile string) (pkg Node, err error) {
	if pkg, ok := i.pkgCache[path]; ok {
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
	filenames := lo.FilterMap(entries, func(entry fs.DirEntry, i int) (string, bool) {
		name := entry.Name()
		if filepath.Ext(name) != ".gf" {
			return "", false
		}
		if name == scriptFile {
			scriptFileFound = true
		}
		return name, true
	})
	if !scriptFileFound && scriptFile != "" {
		return pkg, fmt.Errorf("script file %s not found in package %s", scriptFile, path)
	}
	pkg, err = ParsePackage(pkgfs, scriptFile, filenames...)
	if err != nil {
		return pkg, err
	}
	i.pkgCache[path] = pkg
	return pkg, nil
}

func (i *Importer) ImportSingle(path, scriptFile string) (pkg Node, err error) {
	return i.importSingle(path, scriptFile)
}
