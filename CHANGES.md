See also [replique](https://github.com/EwenG/replique/blob/master/CHANGES.md)

# Version 0.0.16

- Reloading logback files now requires an explicit url
- Minor jump-to-definition and eldoc fixes
- Clj REPLs now support compiling less/scss/stylus files
- Fix a finding usage bug where some directories could be excluded from the search 

# Version 0.0.15

- Improve Clojurescript namespace/var watching
- Make comint-watch-for-password-prompt compatible with Replique
- Add the replique/process-input and replique/process-input-password commands to send a string to the standard input of the process (by opposition of the input stream of the REPL)
- Fix the replique/classpath command when new dependencies need to be downloaded
- Replique now depends on Clojure tools.deps instead of leiningen

# Version 0.0.14

- Add the replique/logback-reload command to reload logback configuration
- Compatibility with Clojurescript 1.10.312

# Version 0.0.13

- The omniscient debugger has been refactored
- Add the REPL session ID to the names of the REPL buffers
- Add a dependency on ivy-mode
- Handling of locals introduced by extend-type/extend-protocol/reify/specify/proxy
- Add the possibility to load a cljs namespace and its dependencies in the cljs environment when starting a cljs REPL 
- Add the replique/find-usage interactive command
- A .repliquedoc file can be added to directories to exclude from the Replique search based features (find-usage, main js file refreshing, ...)
- Fix autocompletion for locals introduced by deftype/defrecord and defmethod forms
- Add the replique/watch interactive command to visualize watchable referneces using a dedicated buffer
- Add the replique/params interactive command to quickly edit the most common clojure.core/cljs.core dynamic vars (*print-length*, *print-level*, *warn-on-reflection*)
- Add the replique/pprint interactive command to pretty print the expression at point

# Version 0.0.12 - Corrupted release - see 0.0.13

# Version 0.0.11 - Corrupted release - see 0.0.13

# Version 0.0.10

- Pretty printing for exceptions printed in the *Messages* buffer
- Buffers are no longer switched when changing the currently active REPL and the new active REPL is already displayed
- Buffers are no longer switched when changing the currently active process
- Fix the computation of the current namespace to take into account calls to "in-ns"
- jump-to-definition for locals
- The jump-to-definition command now prompts for the file to jump to when used with a namespace which definition is split in multiple files 
- Fix a potential error when analyzing local parameters
- Fix autocompletion for parameters of named functions 

# Version 0.0.9

- Autocompletion refactoring - fuzzy matching, autocompletion for files and javascript interop calls
- Add metadata (line, column number, file name) to forms evaluated at the REPL from a clj/cljs buffer
- Enable the replique/in-ns interactive command in REPL buffers
- Automatically change the namespace of the REPL when evaluating a form from a source code buffer 
- Do not overwrite a currently active minibuffer when printing in the echo area
- When printing REPL output in the echo area, consider both active Clojure and Clojurescript REPLs instead of the active REPL only
- Bufferize the REPL output printed in the echo area 
- Add the replique/reload-all command to reload a Clojure/Clojurescript file and all its dependencies
- Add the replique/offline-mode command to switch leiningen into offline mode (-o flag)

# Version 0.0.8

- clj/cljs/cljc files can now be loaded from files or from files in jar archives
- Remove ansi colors from messages printed in the *Messages* buffer
- replique/eval-defn now unwraps the top level (comment ...) block, if any
- Fix jump to definition when used in the REPL buffer
- REPLs not belonging to the active process can no longer be considered as active
- Replique no longer breaks when using clojure symbols that cannot be read by the elisp reader
- Fix javascript commands (using js2-mode)
- Add the replique/remove-var interactive command

# Version 0.0.7

- The omniscient debugger no longer automatically select the environment to be debugged when there is only one environment
- Add the replique/classpath command for classpath reloading

# Version 0.0.6

- Add an omniscient debugger
- Change switch-active-repl and eval-region default keymap
- Remove the possibility to eval cljc code in multiple repls simultaneously

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