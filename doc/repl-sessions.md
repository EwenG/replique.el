# REPL sessions

## REPL hostname

The REPL binds itself to localhost by default. The default host can be changed by customizing the `replique/host` variable. 

A REPL can be started using a specific host on a per command basis by using the universal prefix argument (`C-u`) when running the `replique/repl` command. 

## Using multiple REPL sessions

To start multiple REPL sessions in the same JVM process, use `M-x replique/repl` multiple times, using the same directory.

Replique will keep at most one Clojure REPL and one Clojurescript REPL active at the same time. Use `M-x replique/switch-active-repl` to change the currently active REPL.

Multiple JVM processes can be started simultaneously for different projects by using different directories when starting the REPLs. Use `replique/switch-active-process` to change the currently active process. Symbolic links can be used to start multiple JVM processes simultaneously for the same project.

Use `replique/close-process` to close all the REPL sessions associated with a JVM process at the same time.

## REPL printing

All Clojure and Clojurescript REPL output is printed in the [comint](https://www.emacswiki.org/emacs/ComintMode) buffer associated with the REPL. Data printed to the standard output of the JVM process is displayed in the emacs \*Messages* buffer. Data can be printed to the standard output of the JVM process by binding \*out* to `replique.utils/process-out`.

## Uncaught exceptions

Replique registers a default exception handler that prints all uncaught exceptions in the emacs \*Messages\* buffer. To override the default exception handler:

```clojure
(Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
       ;; Do what you want here
       )))
```

## Evaluation context

When evaluating a form from a buffer containing Clojure or Clojurescript code, Replique automatically changes the REPL namespace to the one of this buffer. The buffer namespace is determined by looking at the closest `(in-ns ...)` form above the one beeing evaluated, or, if none is found, at the top level `(ns ...)` form of the file.

A useful pattern is to use the following code snippet to evaluate some code in a different namespace than the one of the current file:

```
(comment
  (in-ns 'some-other-namespace)
  ;; this code is evaluated in the some-other-namespace namespace
)
```

## REPL parameters

Replique provides a dedicated interface to edit some of the dynamic vars that affect the REPL behavior.
These vars are \*print-length\*, \*print-level\* and \*warn-on-reflection\*. To edit one of them, use the `replique/params` interactive command.
The `replique/params` interface provides two keybindings: `+` and `-` to quickly increment/decrement numerical values or to toggle boolean values.

## Starting a Clojure REPL programmatically

Replique does not support the use of the `clojure.main/repl` because Replique relies on the customization of the REPL to keep track of the current namespace. The `replique.interactive/repl` function should be used
in place of `clojure.main/repl`. `replique.interactive/repl` supports the same options than `clojure.main/repl`.

## REPL configuration through Leiningen

Replique does not support all Leiningen configuration keys. In particular, since Replique is not based on [nrepl](https://github.com/clojure/tools.nrepl), none of the keys related to nrepl are supported.

The *:global-vars* leiningen key is supported and the following vars values are propagated to the cljs REPL: `#{#'*assert* #'*print-length* #'*print-meta* #'*print-level* #'*flush-on-newline* #'*print-readably* #'*print-dup*})`
