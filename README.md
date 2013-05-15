# Device Mapper Bindings For Racket

Basic `libdevmapper.so.1.02` FFI bindings for Racket.

If you need some functionality that is missing right now,
just ask me and we will work something out.


## Example

```racket
(require dm)

(dm-create "test"
           (dm-target 0 1024 'linear '("8:1" 0)))

(dm-rename "test" "foobar")

(dm-suspend "foobar")
(dm-reload "foobar"
           (dm-target 0 1024 'linear '("8:1" 0))
           (dm-target 1024 1024 'linear '("8:1" 2048)))

(dm-resume "foobar")
(dm-remove "foobar")
```

