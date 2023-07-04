package fsx

import (
	"testing"
	"testing/fstest"
)

func Test(t *testing.T) {
	tfs := TestFS([][2]string{
		{"a/b/c.txt", "hello"},
		{"a/d/e.txt", "goodbye"},
		{"f/g/h.txt", "another one"},
	})
	if err := fstest.TestFS(tfs, "a/b/c.txt", "a/d/e.txt", "f/g/h.txt"); err != nil {
		t.Fatal(err)
	}
}

func TestWrite(t *testing.T) {
	tfs := TestFS([][2]string{
		{"a/b/c.txt", "hello"},
		{"a/d/e.txt", "goodbye"},
		{"f/g/h.txt", "another one"},
	})
	dir, err := Mkdir(tfs, "i", 0)
	if err != nil {
		t.Fatal(err)
	}
	f, err := Create(dir, "j.txt")
	if err != nil {
		t.Fatal(err)
	}
	if wf, ok := f.(WriteableFile); ok {
		if _, err := wf.Write([]byte("hello world")); err != nil {
			t.Fatal(err)
		}
	}
}
