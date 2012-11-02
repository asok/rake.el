# rake.el #

Rake task runner for GNU Emacs.

![screenshot](https://github.com/vderyagin/rake.el/raw/master/screenshot.png)

## Installation ##

Download `rake.el` from this repository, put it on Emacs `load-path`,
put `(require 'rake)` in your initialization file.

Alternatively, If you are using [el-get](https://github.com/dimitri/el-get),
use this recipe:

``` lisp
(:name rake.el
       :type github
       :pkgname "vderyagin/rake.el")
```

## Usage ##

**M-x rake** - interactively select rake task from list and run it in `*rake*`
  buffer.

**M-x rake-goto-task-definition** - interactively select rake task from list
  and jump to it's implementation.

### System-wide rake tasks ###

Rake supports defining system-wide tasks in `~/.rake/*.rake`

To use that global set of tasks instead of local to the current buffer, use
commands with prefix argument:

**C-u M-x rake** - interactively select system-wide rake task from list and
  run it in `*rake*` buffer.

**C-u M-x rake-goto-task-definition** - interactively select system-wide rake
  task from list and jump to it's implementation.

## Issues ##

This package uses Emacs `compilation-mode`, which does not expect process to
launch asynchronous subprocesses; if it does, and they keep running after the
main process has terminated, Emacs may kill them. Be aware of that if you are
shelling out using `IO::popen`, `Kernel#spawn` or launching async subprocesses
is some other way from your rake tasks and not waiting for them to exit (using
``Kernel#` ``, `Kernel#system`, also `FileUtils#sh` and `FileUtils#ruby` added
by rake is no problem, as they work synchronously).
