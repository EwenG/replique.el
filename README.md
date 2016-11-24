# Replique

An emacs mode for [Replique](https://github.com/EwenG/replique).

Replique is a development environment for Clojure and Clojurescript bundled as a leiningen plugin.

## Installation

Replique is compatible with clojure 1.8.0+, clojurescript 1.8.40+ and requires emacs 25+ with clojure-mode.

### Emacs mode

Download the latest [replique release](https://github.com/EwenG/replique.el/releases). Extract the zip file in a directory of your choice and add it to your emacs load-path. For example

`(add-to-list 'load-path "~/.emacs.d/replique")`

Load replique

`(require 'replique)`

Enable replique when editing clojure files:

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

Enable css files reloading

`(add-hook 'css-mode-hook 'replique/minor-mode)`

Enable javascript files reloading

`(add-hook 'js-mode-hook 'replique/minor-mode)`

Enable code evaluation in org-mode

```elisp
(add-hook 'org-mode-hook
          (lambda ()
            (require 'ob-replique))
```

## Features

- Clojure and Clojurescript REPL, support for mulitple simultaneous sessions
- Preconfigured Clojurescript compiler for development builds
- Autocompletion for Clojure and Clojurescript
- [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation for Clojure and Clojurescript
- CSS files reloading
- Javascript file reloading
- Org-babel integration

## Getting started

### Clojure REPL

`M-x replique/repl`

Replique will ask you for a project directory and a port number. The project directory must contain a leiningen *project.clj* file. Replique will start a socket REPL using the provided port number. Use `0` to start the REPL on a random port number.

Use `C-x C-e` to evaluate a Clojure form, `C-c C-l` to load a Clojure file in the REPL and `C-c M-n` to change the REPL namespace.

Kill the buffer to close the REPL.

### Clojurescript REPL

Add Clojurescript to your *project.clj* dependencies

`[org.clojure/clojurescript VERSION_NUMBER]`

Start a Clojure REPL

`M-x replique/repl`

Start a Clojurescript REPL in your Clojure REPL

`M-x replique/cljs-repl` or, in the REPL `(replique.interactive/cljs-repl)`

Connect a browser to the cljs REPL

`M-x replique/browser`

Use `C-x C-e` to evaluate a Clojurescript form, `C-c C-l` to load a Clojurescript file in the REPL and `C-c M-n` to change the REPL namespace.

kill the buffer to close the REPL.

### Going further

See the [default keymap](#default-keymap), the [interactive commands](#interactive-commands) and the [REPL API](#repl-api).

## Using multiple sessions

To start multiple REPL sessions in the same JVM process, use `M-x replique/repl` multiple times, using the same directory. Replique will keep at most one Clojure REPL and one Clojurescript REPL active at the same time. Use `M-x replique/switch-active-repl` to change the currently active REPL.

Multiple JVM processes can be started simultaneously. Use `replique/switch-active-process` to change the currently active process. Symbolic links can be used to start multiple JVM processes simultaneously for the same project.

Use `replique/close-process` to close all the REPL sessions associated with a JVM process at the same time.

## Clojurescript and cljc support

Replique supports autocompletion and [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation for Clojurescript with the limitation that autocompletion does not work for interop calls.

Clojurescript code can be evaluated in the browser, optionaly in the context of a web application. Others javascript environments are not currently supported.

When Clojurescript files get loaded, they are compiled to disk and then loaded in the Clojurescript environment. By default, Replique compiles Clojurescript files in the *target/cljs* folder. The compilation output folder can be customized using the *project.clj* file:

`{:replique {:cljs-compile-path "%s/cljs"}}`

Including *%s* will splice the *:target-path* into this value.

Loading *.cljc* files (see [reader conditionals](http://clojure.org/guides/reader_conditionals)) requires that both a Clojure and a Clojurescript REPL are started. Replique will load *.cljc* files simultaneously in the Clojure and the Clojurescipt REPL. Autocompletion candidates for *.cljc* files are computed using the Clojure runtime, unless the cursor is in a *#?cljs* reader conditional.

## Default keymap

Keybinding           | Description
---------------------|----------------------------------
<kbd>C-c C-r</kbd>   | Evaluate region
<kbd>C-x C-e</kbd>   | Evaluate last sexp
<kbd>C-M-x</kbd>     | Evaluate top level sexp
<kbd>C-c C-l</kbd>   | Load file
<kbd>C-c M-n</kbd>   | Change namespace
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
`replique/output-main-cljs-file` | Write a main cljs file to disk

## REPL API

All REPL functions/macros are in the `replique.interactive` namespace

Function/macro                   | Description
---------------------------------|----------------------------------
                                 |

# License

Copyright 2016 Ewen Grosjean.

Distributed under the GNU General Public License, version 3.
