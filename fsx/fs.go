package fsx

import (
	"errors"
	"io"
	"io/fs"
	"os"
	"strings"
	"time"
	_ "unsafe"

	"golang.org/x/exp/slices"
)

var _ fs.File = (*treeFS)(nil)
var _ fs.DirEntry = (*treeFS)(nil)
var _ fs.ReadDirFile = (*treeFS)(nil)
var _ fs.FS = (*treeFS)(nil)
var _ CreateFS = (*treeFS)(nil)
var _ MkdirFS = (*treeFS)(nil)
var _ fs.FS = DirFS("")
var _ CreateFS = DirFS("")
var _ MkdirFS = DirFS("")

type treeFS struct {
	entries []fs.File
	treeFile
}

func TestFS(files [][2]string) *treeFS {
	tfs := newTreeFS("", 0)
	for _, file := range files {
		cur := tfs
		path, body := file[0], file[1]
		parts := strings.Split(path, string(os.PathSeparator))
		for i, part := range parts {
			if i == len(parts)-1 {
				cur.entries = append(cur.entries, newTreeFile(part, 0, []byte(body)))
			} else {
				j := slices.IndexFunc(cur.entries, func(f fs.File) bool {
					return nameOf(f) == part
				})
				if j < 0 {
					cur.entries = append(cur.entries, newTreeFS(part, 0))
					j = len(cur.entries) - 1
				}
				cur = cur.entries[j].(*treeFS)
			}
		}
	}
	return tfs
}

// ReadDir implements fs.ReadDirFile
func (tfs *treeFS) ReadDir(count int) ([]fs.DirEntry, error) {
	n := len(tfs.entries) - tfs.offset
	if n == 0 && count > 0 {
		return nil, io.EOF
	}
	if count > 0 && n > count {
		n = count
	}
	list := make([]fs.DirEntry, n)
	for i := range list {
		list[i] = tfs.entries[tfs.offset+i].(fs.DirEntry)
	}
	tfs.offset += n
	return list, nil
}

var _ fs.FileInfo = (*treeFile)(nil)
var _ fs.File = (*treeFile)(nil)
var _ fs.DirEntry = (*treeFile)(nil)
var _ WriteableFile = (*treeFile)(nil)

type treeFile struct {
	name   string
	mode   fs.FileMode
	data   []byte
	offset int
}

// Write implements WriteableFile
func (tf *treeFile) Write(p []byte) (n int, err error) {
	tf.data = append(tf.data, p...)
	return len(p), nil
}

// Info implements fs.DirEntry
func (tf *treeFile) Info() (fs.FileInfo, error) {
	return tf, nil
}

// Type implements fs.DirEntry
func (tf *treeFile) Type() fs.FileMode {
	return tf.mode.Type()
}

// Close implements fs.File
func (tf *treeFile) Close() error {
	tf.offset = 0
	return nil
}

// Read implements fs.File
func (tf *treeFile) Read(p []byte) (int, error) {
	if tf.offset >= len(tf.data) {
		return 0, io.EOF
	}
	n := copy(p, tf.data[tf.offset:])
	tf.offset += n
	return n, nil
}

// Stat implements fs.File
func (tf *treeFile) Stat() (fs.FileInfo, error) {
	return tf, nil
}

// IsDir implements fs.FileInfo
func (tf *treeFile) IsDir() bool {
	return tf.mode.IsDir()
}

// ModTime implements fs.FileInfo
func (*treeFile) ModTime() time.Time {
	return time.Time{}
}

// Mode implements fs.FileInfo
func (tf *treeFile) Mode() fs.FileMode {
	return tf.mode
}

// Name implements fs.FileInfo
func (tf *treeFile) Name() string {
	return tf.name
}

// Size implements fs.FileInfo
func (tf *treeFile) Size() int64 {
	return int64(len(tf.data))
}

// Sys implements fs.FileInfo
func (*treeFile) Sys() any {
	return nil
}

func newTreeFile(name string, mode fs.FileMode, data []byte) *treeFile {
	return &treeFile{
		name: name,
		mode: mode,
		data: data,
	}
}

func (tfs *treeFS) Read([]byte) (int, error) { return 0, errors.New("cannot read directory") }

// Mkdir implements MkdirFS
func (tfs *treeFS) Mkdir(name string, perm fs.FileMode) (fs.FS, error) {
	i := slices.IndexFunc(tfs.entries, func(f fs.File) bool {
		return nameOf(f) == name
	})
	if i >= 0 {
		return nil, &fs.PathError{Op: "mkdir", Path: name, Err: fs.ErrExist}
	}
	nfs := newTreeFS(name, perm)
	tfs.entries = append(tfs.entries, nfs)
	return nfs, nil
}

func newTreeFS(name string, perm fs.FileMode) *treeFS {
	return &treeFS{
		entries: []fs.File{},
		treeFile: treeFile{
			name: name,
			mode: perm | fs.ModeDir,
		},
	}
}

// Create implements CreateFS
func (tfs *treeFS) Create(name string) (WriteableFile, error) {
	i := slices.IndexFunc(tfs.entries, func(f fs.File) bool {
		return nameOf(f) == name
	})
	if i >= 0 {
		// if it already exists, truncate it
		if f, ok := tfs.entries[i].(*treeFile); ok {
			f.data = nil
			return f, nil
		} else {
			return nil, &fs.PathError{Op: "create", Path: name, Err: fs.ErrExist}
		}
	}
	f := newTreeFile(name, 0, nil)
	tfs.entries = append(tfs.entries, f)
	return f, nil
}

func nameOf(f fs.File) string {
	switch f := f.(type) {
	case *treeFS:
		return f.name
	case *treeFile:
		return f.name
	}
	panic("unreachable")
}

func (tfs *treeFS) open(name string) (fs.File, error) {
	elems := strings.Split(name, string(os.PathSeparator))
	cur := tfs
	for _, elem := range elems {
		if elem == "." {
			continue
		}
		i := slices.IndexFunc(cur.entries, func(f fs.File) bool {
			return nameOf(f) == elem
		})
		if i < 0 {
			return nil, &fs.PathError{Op: "open", Path: name, Err: fs.ErrNotExist}
		}
		entry := cur.entries[i]
		stat, err := entry.Stat()
		if err != nil {
			return nil, &fs.PathError{Op: "open", Path: name, Err: err}
		}
		if stat.IsDir() {
			cur = entry.(*treeFS)
		} else {
			return entry, nil
		}
	}
	ntfs := *cur
	return &ntfs, nil
}

// Open implements fs.FS
func (tfs *treeFS) Open(name string) (fs.File, error) {
	if !fs.ValidPath(name) {
		return nil, &fs.PathError{Op: "open", Path: name, Err: fs.ErrInvalid}
	}
	return tfs.open(name)
}

type WriteableFile interface {
	fs.File
	io.Writer
}

type CreateFS interface {
	fs.FS
	Create(name string) (WriteableFile, error)
}

func Create(fsys fs.FS, name string) (WriteableFile, error) {
	if cfs, ok := fsys.(CreateFS); ok {
		return cfs.Create(name)
	}
	return nil, &fs.PathError{Op: "create", Path: name, Err: fs.ErrNotExist}
}

type MkdirFS interface {
	fs.FS
	Mkdir(name string, perm fs.FileMode) (fs.FS, error)
}

func Mkdir(fsys fs.FS, name string, perm fs.FileMode) (fs.FS, error) {
	if mfs, ok := fsys.(MkdirFS); ok {
		return mfs.Mkdir(name, perm)
	}
	return nil, &fs.PathError{Op: "mkdir", Path: name, Err: fs.ErrInvalid}
}

type DirFS string

// Mkdir implements MkdirFS
func (dir DirFS) Mkdir(name string, perm fs.FileMode) (fs.FS, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &os.PathError{Op: "mkdir", Path: name, Err: err}
	}
	if err := os.Mkdir(fullname, perm); err != nil {
		err.(*os.PathError).Path = name
		return nil, err
	}
	return DirFS(fullname), nil
}

// Create implements CreateFS
func (dir DirFS) Create(name string) (WriteableFile, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &os.PathError{Op: "create", Path: name, Err: err}
	}
	f, err := os.Create(fullname)
	if err != nil {
		err.(*os.PathError).Path = name
		return nil, err
	}
	return f, nil
}

func (dir DirFS) Open(name string) (fs.File, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &os.PathError{Op: "stat", Path: name, Err: err}
	}
	f, err := os.Open(fullname)
	if err != nil {
		err.(*os.PathError).Path = name
		return nil, err
	}
	return f, nil
}

//go:linkname safefilepath_FromFS internal/safefilepath.FromFS
func safefilepath_FromFS(path string) (string, error)

// join returns the path for name in dir.
func (dir DirFS) join(name string) (string, error) {
	if dir == "" {
		return "", errors.New("os: DirFS with empty root")
	}
	if !fs.ValidPath(name) {
		return "", os.ErrInvalid
	}
	name, err := safefilepath_FromFS(name)
	if err != nil {
		return "", os.ErrInvalid
	}
	if os.IsPathSeparator(dir[len(dir)-1]) {
		return string(dir) + name, nil
	}
	return string(dir) + string(os.PathSeparator) + name, nil
}

func (dir DirFS) Stat(name string) (fs.FileInfo, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &os.PathError{Op: "stat", Path: name, Err: err}
	}
	f, err := os.Stat(fullname)
	if err != nil {
		err.(*os.PathError).Path = name
		return nil, err
	}
	return f, nil
}
