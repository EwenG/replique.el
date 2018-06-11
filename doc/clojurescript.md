# Clojurscript support

The Clojurescript compilation pipeline involves two steps:

- Compiling Clojurescript code to javascript code and writing it to disk
- Loading the compiled javascript code in a javascript runtime environment

Replique supports the use of two javascript runtime environments: the browser and [Nashorn](http://www.oracle.com/technetwork/articles/java/jf14-nashorn-2126515.html). Others javascript environments are not currently supported.

Unlike others Clojurescript development environments, Replique does not relies on watching the filesystem to load Clojurescript code. Instead, Replique leverages the same file loading and evaluation commands than the ones used in Clojure REPLs.

All Replique features (autocompletion, jump to definition, ...) are also supported in Clojurescript REPLs

## Browser REPLs

There are two kinds of workflows when using a browser REPL.

The first one is when you want to evaluate Clojurescript code but don't want to bother with the setup of an HTTP server. In this case, the Replique embedded HTTP server can be leveraged.

The second one is when you are developing a web application hosted by an HTTP server and want to use a Clojurescript REPL as part of your development workflow.

These two use cases are covered below.

### Using the Replique embedded HTTP server

Start a Clojurescript REPL using the `M-x replique/cljs-repl` command. Then connect a browser to the Clojurescript REPL by using the `M-x replique/browser` command.
 
Under the hood, the `M-x replique/browser` command starts a browser and points it to the `http://localhost:port` URL. The port number is the same than the one returned by evaluating `(replique.interactive/repl-port)` in a Clojure REPL.

### Using the REPL to build a web application

When developing a web application, the setup is slightly different than the one described above. See [replique-pedestal](https://github.com/EwenG/replique-pedestal) for a detailed example of using replique with a pedestal web application.

The first thing you will need to do is to emit a javascript file that will be the entry point of your application. This file is the one you must include in your HTML markup. The command `M-x replique/output-main-js-file` emits such a file. This javascript file includes the code that connects to the Replique Clojurescript REPL. The web application needs not be aware of the Clojurescript REPL and your HTML markup needs not be different between development and production builds. This is equivalent to using the [`:main` option](https://clojurescript.org/reference/compiler-options#main) of the cljs compiler.

`M-x replique/output-main-js-file` prompts for the output path of the main js file. If a Clojurescript REPL is started, `M-x replique/output-main-js-file` also prompts for a cljs namespace. This namespace is the the entry point of your application.

Nothing prevents you from using multiple main js files. Outputting multiple main js files can be useful when using [Google closure modules](https://github.com/clojure/clojurescript/wiki/Compiler-Options#modules), in order to keep HTML markup identical between dev and production.

### Cljc support

When both a Clojure and Clojurescript REPL are started, Replique loads .cljc files in the currently active REPL. Use `M-x replique/switch-active-repl` (C-c C-r) to change the currently active REPL.

## Clojurescript compiler configuration

The Clojurescript compiler is preconfigured for development builds (optimizations at :none, sourcemaps enabled ...). A subset of the compiler options and REPL options can be updated at the REPL (see [the Clojurescript wiki](https://github.com/clojure/clojurescript/wiki) for a description of the options).

- To update the *:repl-verbose* REPL options, **from a cljs REPL**: `(replique.interactive/set-cljs-repl-verbose true)`. Note that `set-cljs-repl-verbose` is a macro.
- To update one of *#{:verbose :warnings :compiler-stats :language-in :language-out :closure-warnings :checked-arrays}* compiler options, **from a clj REPL**: `(replique.interactive/set-cljs-compiler-opt :verbose true)`

Replique does not yet support the :npm-deps Clojurescript compiler options. It will be supported when the npm dependencies feature of the Clojurescript compiler gets more stable.

Replique only manages development builds. For production builds, I would recommend using a custom script, a lein plugin (such as [lein-cljsbuild](https://github.com/emezeske/lein-cljsbuild)) or any other existing solution.

### Customizing the compilation folder

By default, Replique compiles Clojurescript files in the `target/cljs` folder. The compilation output folder can be customized using the `project.clj` file:

`{:replique {:cljs-compile-path "%s/cljs"}}`

Including `%s` will splice the `:target-path` into this value.

### Using external javascript libraries

Replique does not expose the Clojurescript compiler options related to using javascript libraries ([:foreign-libs](https://github.com/clojure/clojurescript/wiki/Compiler-Options#foreign-libs) and [:libs](https://github.com/clojure/clojurescript/wiki/Compiler-Options#libs)). The recommended way to deal with external libraries when using Replique is to use a `deps.clj` file, as described in the [cljsjs](https://clojurescript.org/reference/dependencies#cljsjs) section of the Clojurescript reference.

### Watching Clojurescript vars or namespaces

Clojurescript vars and namespaces are not reified in the javascript runtime environment. As a consequence, one cannot watch them using
[Clojure watchers](https://clojuredocs.org/clojure.core/add-watch).
Replique provides a workaround to watch vars and namespaces during development, by using the data available in the Clojurescript
compiler environment.
Here is an example of a Clojurescript macro that sets a watcher on the namespace where it is defined. Every time a var is
redefined in the namespace, `my-callback` is triggered.

```
(defn my-callback [repl-env compiler-env]
  (cljs.repl/-evaluate repl-env "<cljs repl>" 1 "do_something();"))

(defmacro def-with-watcher []
  (swap! cljs.env/*compiler* update :replique/ns-watches
         assoc ana/*cljs-ns* my-callback)
  nil)
```

To watch a var instead of a namespace:

```
(swap! cljs.env/*compiler* update :replique/var-watches
       assoc ana/*cljs-ns* {'my-var my-callback})
```

### Other considerations when using the browser REPL

The main javascript files emitted using the `replique/output-main-js-file` command internally uses the port number of the Replique REPL. If the port number changes across REPL session, the main javascript files must be updated accordingly. Fortunately, Replique handles this for you and updates all main javascript files found in the project directory on REPL startup.

Replique internally serves javascript files on a different domain than your web server, using cross domain requests. As a consequence, your web server must allow cross domain requests to be performed during development. In particular, the [content security policy](https://www.owasp.org/index.php/OWASP_Secure_Headers_Project#Content-Security-Policy) HTTP headers must not be set to forbid these. Of course, this does not affects production environments.
