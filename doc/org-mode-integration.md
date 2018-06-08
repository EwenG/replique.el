# Org-mode integration

The following section explains how to integrate Replique with [org-mode](http://orgmode.org/manual/Evaluating-code-blocks.html)

## Emacs setup

```elisp
(add-hook 'org-mode-hook
          (lambda ()
            (require 'ob-replique))
```

Clojure code blocks

```
#+begin_src clojure
  (+ 1 2)
#+end_src

#+RESULTS:
: 3
```

Clojurescript code blocks

```
#+begin_src clojurescript
  (+ 1 2)
#+end_src

#+RESULTS:
: 3
```