# Web development

## Using the Clojurescript REPL in the context of a web application

See [the Clojurescript section](Using the REPL to build a web application).

## Javascript files reloading

Replique supports reloading javascript files from the cljs REPL.

### Emacs setup

`(add-hook 'js2-mode-hook 'replique/minor-mode)`

### Usage

From a javascript file: `M-x replique/load-file` or `C-c C-l`

## CSS files reloading

Replique supports reloading css file from the cljs REPL.

### Emacs setup

`(add-hook 'css-mode-hook 'replique/minor-mode)`

### Usage

From a css file: `M-x replique/load-file` or `C-c C-l`

Replique will search in the currently loaded web page for a css file with the same name than the file being reloaded. If found, this file is refreshed. If multiple files are found, Replique will prompt you for the right one and remember your choice.

## Using a CSS preprocessor

### Reloading stylus/less/sass files

Stylus/less/sass files are organized into a files hierarchy. Files reference one another using `import` statements. Replique always recompiles the whole file hierarchy, starting at the root file. The only requirement is that the first compilation must be triggered from the file at the root of the hierarchy.

Here are the instructions to setup Replique for different CSS preprocessors.

### Stylus

Replique supports reloading stylus files from the cljs REPL. Stylus must be installed and the `stylus` executable must be in the emacs `exec-path`.

The stylus command can be customized:

`(setq replique/stylus-executable "stylus")`

The arguments used to compile stylus files can be customized with a function taking the input file and output path as parameters. The function returns the list of parameters passed to the stylus command:

`(setq replique/stylus-args-builder 'replique/stylus-args-builder-default)`

Setup emacs to recognize stylus files:

`(add-hook 'stylus-mode-hook 'replique/minor-mode)`

### Less

Replique supports reloading less files from the cljs REPL. Less must be installed and the `lessc` executable must be in the emacs `exec-path`.

The lessc command can be customized:

`(setq replique/less-executable "lessc")`

The arguments used to compile less files can be customized with a function taking the input file and output path as parameters. The function returns the list of parameters passed to the stylus command:

`(setq replique/less-args-builder 'replique/less-args-builder-default)`

Setup emacs to recognize less files:

`(add-hook 'less-css-mode-hook 'replique/minor-mode)`

### Sass/scss

Replique supports reloading sass or scss files from the cljs REPL. Sass must be installed and the `sass` or `scss` executable must be in the emacs `exec-path`.

The sass and scss commands can be customized:

`(setq replique/sass-executable "sass")`

Or

`(setq replique/scss-executable "scss")`

The arguments used to compile sass or scss files can be customized with a function taking the input file and output path as parameters. The function returns the list of parameters passed to the sass or scss command:

`(setq replique/sass-args-builder 'replique/sass-args-builder-default)`

Or

`(setq replique/scss-args-builder 'replique/scss-args-builder-default)`
