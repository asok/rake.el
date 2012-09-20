Rake task runner for GNU Emacs.


![screenshot](https://github.com/vderyagin/rake.el/raw/master/screenshot.png)


## Problems ##

This package uses Emacs compilation-mode, which does not expect process to
launch asynchronous subprocesses; if it does, and they keep running after the
main process has terminated, Emacs may kill them. Be aware of that if you are
shelling out using `IO::popen`, `Kernel#spawn` or launching async subprocesses
is some other way from your rake tasks (using ``Kernel#` ``, `Kernel#system`,
also `FileUtils#sh` and `FileUtils#ruby` added by rake is no problem, as they
work synchronously).
