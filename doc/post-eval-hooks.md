# Post eval hooks

Replique implement a post eval hook mechanism in order for developers to be able to trigger side effects on
vars/namespace changes.

Clojure vars can be watched using [watchers](https://clojuredocs.org/clojure.core/add-watch) but there is no easy way
to react to a change to any of the vars of a given namespace.

Clojurescript namespace and vars are not reified in the javascript runtime environment. Thus they cannot be watched
using watchers. Replique provides a workaround to watch vars and namespaces during development, by using the data 
available in the Clojurescript compiler environment.

Clojure watchers are triggered on every var mutation, thus one REPL evaluation can trigger a watcher multiple times.
Replique hooks are triggered at most once by form evaluation.

## Watching Clojure namespaces

Clojure namespaces can be watched by mutating the `replique.utils/clj-env-hooks` atom.
The `replique.utils/clj-env-hooks` atom contains a map which keys are namespace prefixes and values are functions
which are executed when any of the vars in one of the namespaces with the given prefix is mutated.

Here is an example of watching all the Clojure namespaces which name starts with "my.namespace.prefix":

```
(swap! utils/clj-env-hooks assoc 'my.namespace.prefix
       (fn [] (prn "namespace update trigger")))
```

The above example can be put in a
[REPL init file](https://github.com/EwenG/replique.el/blob/master/doc/repl-sessions.md#repl-init-file)
in order for it to be executed on each REPL start.

## Watching Clojurescript vars or namespaces

Clojurescript namespaces can be watched by mutating the `replique.utils/cljs-env-hooks` atom.
The `replique.utils/cljs-env-hooks` atom contains a map which keys are namespace prefixes and values are functions
which are executed when any of the vars in one of the namespaces with the given prefix is mutated.
The hook functions must take three parameters:
- The Clojurescript repl environment, which can be used to evaluate javascript code using the 
`replique.interactive/eval-js` function
- The value of the Clojurescript compiler environment before the update
- The value of the Clojurescript compiler environment after the update

Here is an example of watching all the Clojurescript namespaces which name starts with "my.namespace.prefix":

```
(swap! utils/cljs-env-hooks assoc 'my.namespace.prefix
       (fn [repl-env prev-comp-env comp-env]
         (replique.interactive/eval-js repl-env "app.view.render()")))
```

Here is an example of watching a single Clojurescript var:

```
(swap! utils/cljs-env-hooks assoc 'my.namespace.prefix
       (fn [repl-env prev-comp-env comp-env]
         (when (replique.repl-cljs/updated-var? prev-comp-env comp-env 'my.namespace.prefix/my-var)
           (replique.interactive/eval-js repl-env "app.view.render()"))))
```

The above examples can be put in a
[REPL init file](https://github.com/EwenG/replique.el/blob/master/doc/repl-sessions.md#repl-init-file)
in order for them to be executed on each REPL start.