# Removing vars and reloading dependencies

## Removing vars

Clojure and Clojurescript vars can be removed using the `replique/remove-var`. The `replique/remove-var` command prompts for the var to be removed amongs all the vars of the current buffer's namespace. Removing a var also removes all the mappings (ie. :require :refer) to the var from all the loaded namespaces. 

## Reloading a file and all its dependencies

The `replique/reload-all` interactive command can be used to reload a file and, recursively, all its dependencies. 
In Clojure, this is effectively equivalent to using the `:reload-all` option of `clojure.core/require`.
In Clojurescript, the `:reload-all` option of `cljs.core/require` does not work and thus Replique implements its own logic.
