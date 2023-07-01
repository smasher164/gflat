package fstest

import (
	"io"
	"io/fs"
	"reflect"
	"testing/fstest"
)

type mapFileInfo struct {
	name string
	f    *fstest.MapFile
}

type openMapFile struct {
	path string
	mapFileInfo
	offset int64
}

type mapFile struct {
	f *fstest.MapFile
	fs.File
}

func (mf mapFile) Write(p []byte) (n int, err error) {
	mf.f.Data = append(mf.f.Data, p...)
	return len(p), nil
}

type mapFS struct {
	fstest.MapFS
}

func MapFS() mapFS {
	return mapFS{fstest.MapFS{}}
}

func (mfs mapFS) Add(name, body string) mapFS {
	mfs.MapFS[name] = &fstest.MapFile{
		Data: []byte(body),
	}
	return mfs
}

func (mfs mapFS) Create(name string) (WriteableFile, error) {
	if _, ok := mfs.MapFS[name]; ok {
		return nil, &fs.PathError{Op: "create", Path: name, Err: fs.ErrExist}
	}
	f := &fstest.MapFile{}
	mfs.MapFS[name] = f
	of, err := mfs.Open(name)
	if err != nil {
		return nil, err
	}
	u := reflect.ValueOf(of).UnsafePointer()
	o := (*openMapFile)(u)
	return mapFile{o.f, of}, nil
}

type WriteableFile interface {
	fs.File
	io.Writer
}

type CreateFS interface {
	fs.FS
	Create(name string) (fs.File, error)
}

func Create(fsys fs.FS, name string) (fs.File, error) {
	if cfs, ok := fsys.(CreateFS); ok {
		return cfs.Create(name)
	}
	return nil, &fs.PathError{Op: "create", Path: name, Err: fs.ErrNotExist}
}
