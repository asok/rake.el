(require 'ert)

(ert-deftest rake-extract-task-name-returns-task-name ()
  (should
   (equal
    (rake-extract-task-name "foo # comment")
    "foo")))

(ert-deftest rake-extract-task-name-works-with-namespaces ()
  (should
   (equal
    (rake-extract-task-name "foo:bar # comment")
    "foo:bar")))

(ert-deftest rake-extract-task-name-invalid-line ()
  (should
   (equal
    (rake-extract-task-name "invalid line")
    nil)))

(ert-deftest rake-get-list-of-task-lines-returns-list ()
  (should
   (equal
    (rake-get-list-of-task-lines
     "(in /some/dir)\nrake foo # comment\nrake bar:baz # comment\n")
    '("foo # comment"
      "bar:baz # comment"))))

(ert-deftest rake-get-list-of-task-lines-invalid-input ()
  (should
   (equal
    (rake-get-list-of-task-lines
     "just\nsome\nrubbish")
    nil)))
