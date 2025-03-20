# Hello FS

## Dependencies

* For Debian libfuse-dev is required
* For macOS macfuse is required

## Run

```sh
$ cabal build hello-fs
$ mkdir yo
$ cabal run hello-fs yo
$ cat yo/hello
Hello Fuse!
$ umount yo
$ rm yo
```
