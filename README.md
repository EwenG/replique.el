# Replique

An emacs mode for [Replique](https://github.com/EwenG/replique). 
Replique is a development environment for Clojure and Clojurescript.
Replique relies on the Clojure [command line tools](https://clojure.org/guides/deps_and_cli) for starting REPLs.

## Installation

Replique is compatible with clojure 1.8.0+, clojurescript 1.9.473+ and requires emacs 25+.

### Emacs mode

Add Replique to your emacs package archives

```elisp
(add-to-list 'package-archives '("replique" . "https://raw.githubusercontent.com/EwenG/replique.el/master/packages/") t)
```

Install Replique

```elisp
(unless (package-installed-p 'replique)
    (package-refresh-contents)
  (package-install 'replique))
```

Load replique

`(require 'replique)`

Enable Replique when editing clojure files

`(add-hook 'clojure-mode-hook 'replique/minor-mode)`

### Clojure command line tools

See the [Clojure getting started](https://clojure.org/guides/getting_started) for instructions on how to install the command line tools.

The Clojure command used to start REPLs is customizable: 
`(setq replique/clojure-bin "~/bin/clojure")`

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

Enable [css files reloading](https://github.com/EwenG/replique.el/blob/master/doc/web-development.md#css-files-reloading)

`(add-hook 'css-mode-hook 'replique/minor-mode)`

Enable [stylus files reloading](https://github.com/EwenG/replique.el/blob/master/doc/web-development.md#reloading-styluslesssass-files)

`(add-hook 'stylus-mode-hook 'replique/minor-mode)`

Enable [less files reloading](https://github.com/EwenG/replique.el/blob/master/doc/web-development.md#reloading-styluslesssass-files)

`(add-hook 'less-css-mode-hook 'replique/minor-mode)`

Enable [javascript files reloading](https://github.com/EwenG/replique.el/blob/master/doc/web-development.md#javascript-files-reloading)

`(add-hook 'js2-mode-hook 'replique/minor-mode)`

Enable [code evaluation in org-mode](https://github.com/EwenG/replique.el/blob/master/doc/org-mode-integration.md)

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
- Find usages
- Built-in pretty printing
- [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation
- CSS, [Stylus](http://stylus-lang.com/), [less](http://lesscss.org/), [sass](http://sass-lang.com/), javascript files live reloading
- Data visualization for mutable references
- Support for capturing and visualizing local bindings
- Org-mode integration
- Dynamic classpath reloading
- Logback configuration reloading

Replique tries, as much as possible, to keep features parity between Clojure and Clojurescript.

## Getting started

### Clojure REPL

Execute the command:

`M-x replique/repl`

At that point, Replique prompts for a project directory. Choose the root directory of your project.
Replique then prompts for a REPL start script. Choose the "\*default\*" entry to start the REPL without CLI options.
See [REPL start script](https://github.com/EwenG/replique.el/blob/master/doc/repl-sessions.md#repl-start-script) for an explanation of the use of REPL start scripts.
Once done, Replique starts a socket REPL using a random available port number.

Use `C-x C-e` to evaluate a Clojure form, `C-c C-l` to load a Clojure file in the REPL and `C-c M-n` to change the REPL namespace.

When evaluating a form from a buffer containing Clojure or Clojurescript code, Replique automatically changes the REPL namespace to the one of this buffer.

Kill the buffer to close the REPL.

### Clojurescript REPL

Add Clojurescript to your [deps.edn](https://clojure.org/guides/deps_and_cli) dependencies.

`[org.clojure/clojurescript {:mvn/version VERSION_NUMBER}]`

Start a Clojure REPL:

`M-x replique/repl`

Start a Clojurescript REPL in your Clojure REPL:

`M-x replique/cljs-repl` or, at the REPL `(replique.interactive/cljs-repl)`

Connect a browser to the cljs REPL:

`M-x replique/browser`

Use `C-x C-e` to evaluate a Clojurescript form, `C-c C-l` to load a Clojurescript file in the REPL and `C-c M-n` to change the REPL namespace.

Kill the buffer to close the REPL.

## Cheatsheets

See the [default keymap](#default-keymap), the [interactive commands](#interactive-commands) and the [REPL API](#repl-api).

### User manual

- [REPL sessions](https://github.com/EwenG/replique.el/blob/master/doc/repl-sessions.md)
- [Clojurescript support](https://github.com/EwenG/replique.el/blob/master/doc/clojurescript.md)
- [Web development](https://github.com/EwenG/replique.el/blob/master/doc/web-development.md)
- [Pretty printing](https://github.com/EwenG/replique.el/blob/master/doc/pretty-printing.md)
- [Removing vars and reloading dependencies](https://github.com/EwenG/replique.el/blob/master/doc/removing-var-and-reloading-dependencies.md)
- [Watching / visualizing mutable references](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md)
- [Finding usages](https://github.com/EwenG/replique.el/blob/master/doc/finding-usages.md)
- [Debugging](https://github.com/EwenG/replique.el/blob/master/doc/debugging.md)
- [Org-mode integration](https://github.com/EwenG/replique.el/blob/master/doc/org-mode-integration.md)
- [Logback reloading](https://github.com/EwenG/replique.el/blob/master/doc/logback-reloading.md)
- [Migrating from Leiningen]()
- [Building/packaging an application]()

### Default keymap

Keybinding              | Description
------------------------|----------------------------------
<kbd>C-x C-r</kbd>      | Evaluate region
<kbd>C-x C-e</kbd>      | Evaluate last sexp. The REPL namespace is changed to the namespace of the current buffer before the form is evaluated
<kbd>C-M-x</kbd>        | Evaluate top level sexp. If the top level form is a `(comment ...)` block, then the form evaluated is the one under the comment block
<kbd>C-c C-l</kbd>      | Load file
<kbd>C-c M-n</kbd>      | Change the REPL namespace
<kbd>M-.</kbd>          | Jump to symbol definition
<kbd>C-c C-r</kbd>      | Change active REPL session
<kbd>C-c C-c</kbd>      | Pretty print the expression at point
<kbd>C-c C-u</kbd>      | Find all the [usages](https://github.com/EwenG/replique.el/blob/master/doc/finding-usages.md) of the expression at point
<kbd>C-c C-w</kbd>      | [Watch](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md) a mutable reference or the stream of the REPL output

### Interactive commands

Command                          | Description
---------------------------------|----------------------------------
`replique/repl`                  | Start a REPL, use the universal prefix argument (`C-u`) to customize the REPL host and port number
`replique/cljs-repl`             | Turn a clj REPL into a cljs REPL
`replique/cljs-repl-nashorn`     | Turn a clj REPL into a Nashorn cljs REPL
`replique/browser`               | Open a browser tab and connects it to the Clojurescript REPL
`replique/switch-active-repl`    | Change the active REPL session
`replique/switch-active-process` | Change the active JVM process
`replique/close-process`         | Close all processes associated with a JVM process
`replique/output-main-js-file`   | Write a main js file to disk
`replique/classpath`             | Reload the classpath based on the deps.edn configuration, optionally using the optons of a REPl start script
`replique/reload-all`            | Reload the current namespace as well as all its dependencies
`replique/remove-var`            | Prompts for a var to be undefined. The var is also removed from all the mappings of all the namespaces
`replique/params`                | Set the value of \*print-length\*, \*print-level\* or \*warn-on-reflection\* for the currently active REPL
`replique/pprint`                | Pretty print the expression at point
`replique/find-usage`            | Find all the [usages](https://github.com/EwenG/replique.el/blob/master/doc/finding-usages.md) of the expression at point
`replique/watch`                 | [Watch](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md) a mutable reference or the stream of the REPL output
`replique/logback-reload`        | [Reload](https://github.com/EwenG/replique.el/blob/master/doc/logback-reloading.md) the currently loaded logback configuration 

### REPL API

All REPL functions/macros are in the `replique.interactive` namespace

Functions                        | Description
---------------------------------|----------------------------------
`repl`                           | Start a Clojure REPL. This MUST be used instead of `clojure.main/repl`
`cljs-repl`                      | Turn the REPL into a Clojurescript REPL
`cljs-repl-nashorn`              | Turn the REPL into a Clojurescript REPL running in a [Nashorn](http://www.oracle.com/technetwork/articles/java/jf14-nashorn-2126515.html) environement
`logback-reload`                 | [Reload](https://github.com/EwenG/replique.el/blob/master/doc/logback-reloading.md) the currently loaded logback configuration

Macros                           | Description
---------------------------------|----------------------------------
`load-file`                      | Loads a clj/cljs/cljc file in the REPL
`load-url`                       | Loads a clj/cljs/cljc URL in the REPL. The URL can be a file or a file in a jar 
`cljs-in-ns`                     | Change the cljs REPL namespace
`set-cljs-repl-verbose`          | Set the [verbose option](https://github.com/EwenG/replique.el/blob/master/doc/clojurescript.md#clojurescript-compiler-configuration) of the Clojurescript REPL
`set-cljs-compiler-opt`          | Customize a Clojurescript [compiler option](https://github.com/EwenG/replique.el/blob/master/doc/clojurescript.md#clojurescript-compiler-configuration). See the `compiler-opts` var for available options
`remove-var`                     | [Remove](https://github.com/EwenG/replique.el/blob/master/doc/removing-var-and-reloading-dependencies.md) a var from its namespace. Also remove all mappings to this var from any namespace
`capture-env`                    | [Capture](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md) a local environment and saves it in the atom given as first parameter
`capture-child-env`              | [Capture](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md) a local environment and appends it to its parent. Must be used in the body of a `capture-env` macro or another `capture-child-env`  macro
`with-env`                       | [Restore](https://github.com/EwenG/replique.el/blob/master/doc/watching-visualizing-mutable-references.md) the bindings of its first parameter wich must be a captured environment and then executes its body 

Vars                             | Description
---------------------------------|----------------------------------
`compiler-opts`                  | The [compiler options]((https://github.com/EwenG/replique.el/blob/master/doc/clojurescript.md#clojurescript-compiler-configuration)) that can be customized at the REPL

# License

Copyright 2016 Ewen Grosjean.

Distributed under the GNU General Public License, version 3.
