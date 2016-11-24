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
- Preconfigured Clojurescript compiler 
- Autocompletion for Clojure and Clojurescript
- [Eldoc](https://www.emacswiki.org/emacs/ElDoc) style documentation for Clojure and Clojurescript
- CSS files reloading
- Javascript file reloading
- Org-babel integration

## Getting started

### Clojure REPL

`M-x replique/repl`

Replique will ask you for a project directory and a port number. The project directory must contain a leiningen *project.clj* file. Replique will start a socket REPL using the provided port number. Use `0` to start the REPL on a random port number.

Use `C-c C-e` to evaluate a Clojure form, `C-c C-l` to load a Clojure file in the REPL and `C-c M-n` to change the REPL namespace.

### Clojurescript REPL

Add Clojurescript to your *project.clj* dependencies

`[org.clojure/clojurescript VERSION_NUMBER]`

Start a Clojure REPL

`M-x replique/repl`

Start a Clojurescript REPL in your Clojure REPL

`M-x replique/cljs-repl` or, in the REPL `(replique.interactive/cljs-repl)`

Connect a browser to the cljs REPL

`M-x replique/browser`

Use `C-c C-e` to evaluate a Clojurescript form, `C-c C-l` to load a Clojurescript file in the REPL and `C-c M-n` to change the REPL namespace.

## License

Copyright 2016 Ewen Grosjean.

Distributed under the GNU General Public License, version 3.