# Version 0.0.6

# Version 0.0.5

- Add support for javafx css file reloading 
- Add a cljs-repl-nashorn command to start a nashorn cljs REPL
- sass/scss commands now use UTF-8 encoding by default 
- The replique/repl commands now takes the hostname to start the REPL with as an optional argument
- When started with the universal prefix argument, the replique/repl commands now prompts for the hostname to start the REPL with 

# Version 0.0.4

# Version 0.0.3

- Enable jump-to-definition in the comint buffer of the REPL
- Avoid the split of long messages from the standard output (caused by buffering)
- Exclude the cljs-output-folder when searching for main javascript files
- implement the replique/cljs-repl-connection-script command to connect to the cljs-repl
from any environment, even non clojurescript ones

# Version 0.0.2

* Changes

- Implement stylus, less, sass files reloading
- Update the installation process to use [package.el](https://www.emacswiki.org/emacs/ELPA)

- Also see [replique](https://github.com/EwenG/replique/blob/master/CHANGES.md)