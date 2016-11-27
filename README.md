# Replique

An emacs mode for [Replique](https://github.com/EwenG/replique).

Replique is a development environment for Clojure and Clojurescript implemented as a leiningen plugin.

## Installation

Replique is compatible with clojure 1.8.0+, clojurescript 1.8.40+ and requires emacs 25+ with clojure-mode.

### Emacs mode

Download the latest [replique release](https://github.com/EwenG/replique.el/releases). Extract the zip file in a directory of your choice and add it to your emacs load-path. For example

`(add-to-list 'load-path "~/.emacs.d/replique")`

Load replique

`(require 'replique)`

Enable Replique when editing clojure files

`(add-hook 'clojure-mode-hook 'replique/minor-mode)`

### Leiningen

Download the [leiningen script](http://leiningen.org/). Either place the lein script in emacs `exec-path` or customize the `replique/lein-script` variable, for example

`(setq replique/lein-script "~/bin/lein")`

## Optional configuration

Auto completion is supported through [company-mode](https://github.com/company-mode/company-mode).

Enable company-mode either globally or locally

`(add-hook 'after-init-hook 'global-company-mode)`

Or

```elisp
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (company-mode 1)))
```
       
```elisp
(add-hook 'replique/mode-hook
	  (lambda ()
	    (company-mode 1)))
```

Enable [css files reloading](#css-files-reloading)

`(add-hook 'css-mode-hook 'replique/minor-mode)`

Enable [javascript files reloading](#javascript-files-reloading)

`(add-hook 'js-mode-hook 'replique/minor-mode)`

Enable [code evaluation in org-mode](#using-replique-with-org-mode)

```elisp
(add-hook 'org-mode-hook
          (lambda ()
            (require 'ob-replique))
```

## Features

- Clojure and Clojurescript REPL, one or multiple sessions
- Preconfigured Clojurescript compiler for development builds
- Autocompletion
- Jump to symbol definition
- [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation
- CSS files reloading
- Javascript file reloading
- Org-mode integration

Replique tries, as much as possible, to keep features parity between Clojure and Clojurescript.

## Getting started

### Clojure REPL

`M-x replique/repl`

Replique will prompt you for a project directory and a port number. The project directory must contain a leiningen *project.clj* file. Replique will start a socket REPL using the provided port number. Use `0` to start the REPL on a random port number.

Use `C-x C-e` to evaluate a Clojure form, `C-c C-l` to load a Clojure file in the REPL and `C-c M-n` to change the REPL namespace.

Kill the buffer to close the REPL.

### Clojurescript REPL

Add Clojurescript to your *project.clj* dependencies

`[org.clojure/clojurescript VERSION_NUMBER]`

Start a Clojure REPL

`M-x replique/repl`

Start a Clojurescript REPL in your Clojure REPL

`M-x replique/cljs-repl` or, at the REPL `(replique.interactive/cljs-repl)`

Connect a browser to the cljs REPL

`M-x replique/browser`

Use `C-x C-e` to evaluate a Clojurescript form, `C-c C-l` to load a Clojurescript file in the REPL and `C-c M-n` to change the REPL namespace.

Kill the buffer to close the REPL.

### Going further

See the [default keymap](#default-keymap), the [interactive commands](#interactive-commands) and the [REPL API](#repl-api).

## Using multiple sessions

To start multiple REPL sessions in the same JVM process, use `M-x replique/repl` multiple times, using the same directory. Replique will keep at most one Clojure REPL and one Clojurescript REPL active at the same time. Use `M-x replique/switch-active-repl` to change the currently active REPL.

Multiple JVM processes can be started simultaneously for different projects by using different directories when starting the REPLs. Use `replique/switch-active-process` to change the currently active process. Symbolic links can be used to start multiple JVM processes simultaneously for the same project.

Use `replique/close-process` to close all the REPL sessions associated with a JVM process at the same time.

## Clojurescript and cljc support

Replique supports autocompletion, [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation, and jump to definition for Clojurescript with the limitation that autocompletion does not work for interop calls.

Clojurescript code can be evaluated in the browser, optionally in the context of a web application. Others javascript environments are not currently supported.

Loading a cljs file is done by compiling it to disk and then loading it in the Clojurescript environment. By default, Replique compiles Clojurescript files in the *target/cljs* folder. The compilation output folder can be customized using the *project.clj* file:

`{:replique {:cljs-compile-path "%s/cljs"}}`

Including *%s* will splice the *:target-path* into this value.

### Cljc support

Loading *.cljc* files (see [reader conditionals](http://clojure.org/guides/reader_conditionals)) requires both a Clojure and a Clojurescript REPL to be started. Replique will load *.cljc* files simultaneously in the Clojure and the Clojurescipt REPL. Autocompletion candidates (and other tooling features) for *.cljc* files are computed using the Clojure runtime, unless the cursor is in a *#?cljs* reader conditional, in which case it will be computed using the Clojurescript runtime.

### Clojurescript compiler configuration

The Clojurescript compiler is preconfigured for development builds (optimizations at :none, sourcemaps enabled ...). A subset of the compiler options and REPL options can be updated at the REPL (see [the Clojurescript wiki](https://github.com/clojure/clojurescript/wiki) for a description of the options).

- To update the *:repl-verbose* REPL options, **from a cljs REPL**: `(replique.interactive/set-cljs-repl-verbose true)`. Note that `set-cljs-repl-verbose` is a macro.
- To update one of *#{:verbose :warnings :compiler-stats :language-in :language-out :closure-warnings}* compiler options, **from a clj REPL**: `(replique.interactive/set-cljs-compiler-opt :verbose true)`

Replique only manages development builds. For production builds, I would recommend using a custom script, a lein plugin (such as [lein-cljsbuild](https://github.com/emezeske/lein-cljsbuild)) or any other existing solution.

### Clojurescript browser REPL

Replique listens on the REPL port for a connection from a javascript runtime. Replique currently only supports connections from web browsers. Call `(replique.interactive/repl-port)` to know the port the REPL is listening on. Use `M-x replique/browser` to connect a web browser to the cljs REPL, or simply browse `http://localhost:port`

### Using the REPL in a web application context

`M-x replique/output-main-js-file` can be used to emit a javascript file acting as an entry point for your application. This javascript file also contains code to connect to the cljs REPL. This allows connecting to a cljs REPL in the context of a web application. The web application need not be aware of the cljs REPL and your HTML markup need not be different between development and production builds. This is equivalent to using the [*:main* option](https://github.com/clojure/clojurescript/wiki/Compiler-Options#main) of the cljs compiler.

`M-x replique/output-main-js-file` prompts for the output path of the main js file. If a cljs REPL is started, `M-x replique/output-main-js-file` also prompts for a cljs namespace. This namespace is the the entry point of your application.

On startup, Replique will update all main js files found in the project directory with the (possibly new) REPL port number. This avoids the need to update main js files manually between REPL sessions when starting Replique with different port numbers.

Nothing prevents you from using multiple main js files. Outputting multiple main js files is particularly useful when using [Google closure modules](https://github.com/clojure/clojurescript/wiki/Compiler-Options#modules), to keep HTML markup identical between dev and production.

## Javascript files reloading

Replique support reloading javascript files from the cljs REPL

### Emacs setup

`(add-hook 'js-mode-hook 'replique/minor-mode)`

From a javascript file: `M-x replique/load-file` or `C-c C-l`

## CSS files reloading

Replique support reloading css file from the cljs REPL

### Emacs setup

`(add-hook 'css-mode-hook 'replique/minor-mode)`

From a css file: `M-x replique/load-file` or `C-c C-l`

Replique will prompt you for a css file to reload among all the css files linked in the current web page. Alternatively a css file can be embedded in the web page directly as a data uri.

## Using Replique with [org-mode](http://orgmode.org/manual/Evaluating-code-blocks.html)

### Emacs setup

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

## Leiningen configuration

Replique does not support all Leiningen configuration keys. In particular, since Replique is not based on [nrepl](https://github.com/clojure/tools.nrepl), none of the keys related to nrepl are supported.

The *:global-vars* leiningen key is supported and the following vars values are propagated to the cljs REPL: `#{#'*assert* #'*print-length* #'*print-meta* #'*print-level* #'*flush-on-newline* #'*print-readably* #'*print-dup*})`

## Remote REPL

Replique supports connecting to a remote REPL by nesting a REPL inside another. `(replique.interactive/remote-repl host port)` will start a REPL on the server at *host:port*, assuming the server is running a [socket REPL server](http://clojure.org/reference/repl_and_main#_launching_a_socket_server).

## Uncaught exceptions

Replique registers a default exception handler that prints all uncaught exceptions in the emacs \*Messages* buffer. To override the default exception handler:

```clojure
(Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
       ;; Do what you want here
       )))
```

## Process standard output

All Clojure and Clojurescript REPL output is printed in the [comint](https://www.emacswiki.org/emacs/ComintMode) buffer associated with the REPL. Data printed to the standard output of the JVM process is displayed in the emacs \*Messages* buffer. Data can be printed to the standard output of the JVM process by binding \*out* to `replique.interactive/process-out`.

## Default keymap

Keybinding           | Description
---------------------|----------------------------------
<kbd>C-c C-r</kbd>   | Evaluate region
<kbd>C-x C-e</kbd>   | Evaluate last sexp
<kbd>C-M-x</kbd>     | Evaluate top level sexp
<kbd>C-c C-l</kbd>   | Load file
<kbd>C-c M-n</kbd>   | Change namespace
<kbd>M-.</kbd>       | Jump to symbol definition
<kbd>C-x r</kbd>     | Change active REPL session

## Interactive commands

Command                          | Description
---------------------------------|----------------------------------
`replique/repl`                  | Start a REPL
`replique/cljs-repl`             | Start a cljs REPL in a clj REPL
`replique/browser`               | Open a browser tab on the REPL port
`replique/switch-active-repl`    | Change the active REPL session
`replique/switch-active-process` | Change the active JVM process
`replique/close-process`         | Close all processes associated with a JVM process
`replique/output-main-js-file`   | Write a main js file to disk

## REPL API

All REPL functions/macros are in the `replique.interactive` namespace

Function/macro                   | Description
---------------------------------|----------------------------------
                                 |

# License

Copyright 2016 Ewen Grosjean.

Distributed under the GNU General Public License, version 3.
