(ns replique.interactive
  (:refer-clojure :exclude [load-file])
  (:require [replique.utils :as utils]))

(def cljs-repl (utils/dynaload 'replique.repl-cljs/cljs-repl))
(def ^:private cljs-load-file (utils/dynaload 'replique.repl-cljs/load-file))
(def ^:private cljs-in-ns (utils/dynaload 'replique.repl-cljs/in-ns))
(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-set-repl-verbose
  (utils/dynaload 'replique.repl-cljs/set-cljs-repl-verbose))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

;; At the moment, load file does not intern macros in the cljs-env, making dynamically loaded
;; macros unavailable to autocompletion/repliquedoc.
(defmacro load-file [file-path]
  (if (cljs-env? &env)
    (@cljs-load-file file-path)
    (clojure.core/load-file file-path)))

;; It seems that naming this macro "in-ns" make the cljs compiler to crash
(defmacro cljs-in-ns [ns-quote]
  (quote ~(@cljs-in-ns ns-quote)))

(defmacro set-cljs-repl-verbose [b]
  (@cljs-set-repl-verbose b)
  b)

(def compiler-opts #{:verbose :warnings :compiler-stats :language-in :language-out
                     :closure-warnings})

(defmacro set-cljs-compiler-opt [opt-key opt-val]
  {:pre [(contains? compiler-opts opt-key)]}
  (swap! @@cljs-compiler-env assoc-in [:options opt-key] opt-val)
  opt-val)

