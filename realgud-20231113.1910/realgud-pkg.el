;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "realgud" "20231113.1910"
  "A modular front-end for interacting with external debuggers."
  '((load-relative "1.3.1")
    (loc-changes   "1.2")
    (test-simple   "1.3.0")
    (emacs         "25"))
  :url "https://github.com/realgud/realgud"
  :commit "365063ea8ce8ec6a852cb388088d84147421c3c2"
  :revdesc "365063ea8ce8"
  :keywords '("debugger" "gdb" "python" "perl" "go" "bash" "zsh" "bashdb" "zshdb" "remake" "trepan" "perldb" "pdb")
  :authors '(("Rocky Bernstein" . "rocky@gnu.org"))
  :maintainers '(("Rocky Bernstein" . "rocky@gnu.org")))
