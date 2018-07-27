# REPL sessions

## REPL start script

By default, Replique starts REPLs without any [CLI aliases](https://clojure.org/reference/deps_and_cli#_aliases).
Using aliases is useful to start a REPL with a customized classpath or with JVM options.
Replique gives you the possibility to start a REPL with custom [options](https://clojure.org/reference/deps_and_cli#_usage) by selecting a start script.
On REPL startup, Replique prompts for a file with a .sh or a .bat extension among the files at the root of the project directory.
The selected file must contain a valid Clojure command line. The following options present in the command line are used when starting the REPL:

- -Jopt
- -Ralias...
- -Calias...
- -Oalias
- -Malias
- -Aalias
- -Sdeps DEPS
- -Scp CP
- -Srepro
- -Sforce

Other options are ignored.

For example, a REPL start script can be a file named `repl.sh` at the root of your project directory with the following content:

```
#!/bin/sh

;; Start a REPL, using the :dev alias
clojure -A:dev
```

## REPL hostname and port number

By default, Replique starts a socket REPL on localhost using a random available port number.
The hostname and port number can be changed by writing a Replique [init file](https://github.com/EwenG/replique.el/blob/master/doc/repl-sessions.md#repl-init-file). 

A REPL can be started using a specific host and port number on a per command basis by using the universal prefix argument (`C-u`) when running the `replique/repl` command.

## Using multiple REPL sessions

To start multiple REPL sessions in the same JVM process, use `M-x replique/repl` multiple times, using the same directory.

Replique will keep at most one Clojure REPL and one Clojurescript REPL active at the same time. Use `M-x replique/switch-active-repl` to change the currently active REPL.

Multiple JVM processes can be started simultaneously for different projects by using different directories when starting the REPLs. Use `replique/switch-active-process` to change the currently active process. Symbolic links can be used to start multiple JVM processes simultaneously for the same project.

Use `replique/close-process` to close all the REPL sessions associated with a JVM process.

## REPL printing

All Clojure and Clojurescript REPL output is printed in the [comint](https://www.emacswiki.org/emacs/ComintMode) buffer associated with the REPL. Data printed to the standard output of the JVM process is displayed in the emacs \*Messages* buffer. Data can be printed to the standard output of the JVM process by binding \*out\* to `replique.utils/process-out`.

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

## Starting a Clojure REPL programmatically

Replique does not support the use of the `clojure.main/repl` function because Replique relies on the customization of the REPL to keep track of the current namespace. The `replique.interactive/repl` function should be used
in place of `clojure.main/repl`. `replique.interactive/repl` supports the same options than `clojure.main/repl`.

## Using the process standard input

Each REPL is backed by a network socket. Thus typing into the REPL buffer does not send data to the standard input of the process.
The `replique/process-input` and `replique/process-input-password` interactive commands can be used to send a string to the standard input of the process.

## REPL parameters

Replique provides a dedicated interface to edit some of the dynamic vars that affect the REPL behavior.
These vars are \*print-length\*, \*print-level\* and \*warn-on-reflection\*. To edit one of them, use the `replique/params` interactive command.
The `replique/params` interface provides two keybindings: `+` and `-` to quickly increment/decrement numerical values or to toggle boolean values.

## REPL init file

On REPL startup, Replique loads the following files, if found. 

- *user-home-directory*/.replique/init.clj
- *project-directory*/.replique/init.clj

The user-level init file is executed before the project-level one.
Init files are executed before the socket REPL server has been started.
Init files can be used to customize vars on startup. Here are examples of possible customizations:

```clojure
(in-ns 'user)
;; Sets the default *print-length* to 15
(alter-var-root #'*print-length* (constantly 15))
;; Sets the default *print-length* to 10
(alter-var-root #'*print-level* (constantly 10))
;; Don't print namespaced maps. Note the conditional execution because namespaced maps were introduced in Clojure 1.9
(when (resolve '*print-namespace-maps*)
  (alter-var-root (resolve '*print-namespace-maps*) (constantly false)))
```

```clojure
(in-ns 'user)
;; Customize the socket REPL server host and port number
(alter-var-root #'replique.utils/host (constantly "0.0.0.0"))
(alter-var-root #'replique.utils/port (constantly 9000))
```

## Dynamic classpath reloading

To dynamically reload the classpath, use the `replique/classpath` interactive command.
Upon execution, the `replique/classpath` command prompts for a REPL start script.
Selecting the *\*default\** entry reloads the classpath using the deps.edn file.
Selecting a REPL start script reloads the classpath using the deps.edn file merged with the options of the command line found in the selected script.