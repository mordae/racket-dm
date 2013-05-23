#lang scribble/manual

@require[(for-label dm)
         (for-label racket)]

@title{Device Mapper}
@author+email["Jan Dvorak" "mordae@anilinux.org"]

Linux Device Mapper bindings using @filepath{libdevmapper.so.1.02}.

@defmodule[dm]

Linux Device Mapper works with mappings and targets.

Every mapping have a named block device in @filepath{/dev/mapper} and a
table of targets that together represent surface of the block device.
For example a mapping "vg_mordae-lv_root" may represent a logical volume
that maps 10 GiB of space in the middle of my laptop's only disk drive using
a linear mapping.


@defproc[(dm-create (name string?)
                    (#:uuid uuid (or/c #f string?) #f)
                    (target dm-target?) ...)
         void?]{
  Create new mapping from list of targets.
  You can supply an uuid or have it automatically generated.
}


@defproc[(dm-reload (name string?) (target dm-target?) ...) void?]{
  Atomically replace table of specified mapping.
}


@defproc[(dm-rename (name string?)
                    (new-name string?)
                    (#:new-uuid new-uuid (or/c #f string?) #f))
         void?]{
  Rename specified mapping, possibly with uuid (if needed).
}


@defproc[(dm-remove (name string?)) void?]{
  Destroy specified mapping.
}


@defproc[(dm-get-table (name string?)) (listof dm-target?)]{
  Query current table (that is a list of targets) of specified mapping.
}


@defstruct[dm-target
             ((start  integer?)
              (length integer?)
              (type   symbol?)
              (params (listof (or/c string? integer?))))
             #:transparent]{
  Single row of a device map table.
  For example when creating a mapping:

  @racketblock[
    (dm-create "test"
      (dm-target    0 1024 'linear "253:1" 2048)
      (dm-target 1024 1024 'linear "253:2" 2048))
  ]
}


@defproc[(dm-list) (listof string?)]{
  List names of mappings defined on this machine.
}


@defproc[(dm-get-info (name string?)) dm-info?]{
  Query information about a mapping.
}


@defstruct[dm-info
             ((exists         boolean?)
              (suspended      boolean?)
              (live-table     boolean?)
              (inactive-table boolean?)
              (open-count     integer?)
              (event-nr       integer?)
              (major          integer?)
              (minor          integer?)
              (read-only      boolean?)
              (target-count   integer?))
           #:transparent]{
  Mapping information that can be obtained with @racket[dm-get-info].
}


@defproc[(dm-suspend (name string?)) void?]{
  Suspend specified mapping, that is:
  flush pending requests and block new ones.
}


@defproc[(dm-resume (name string?)) void?]{
  Cancel suspension of specified mapping.
}

@; vim:set ft=scribble sw=2 ts=2 et:
