(ns ewen.replique.interactive
  (:refer-clojure :exclude [load-file])
  (:require [ewen.replique.utils :as utils]))

(def ^:private cljs-load-file
  (utils/dynaload 'ewen.replique.server-cljs/load-file))
(def ^:private cljs-in-ns
  (utils/dynaload 'ewen.replique.server-cljs/in-ns))
(def ^:private cljs-repl-env
  (utils/dynaload 'ewen.replique.server-cljs/repl-env))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

;; t the moment, load file does not intern macros in the cljs-env, making dynamically loaded
;; macros unavailable to autocompletion/repliquedoc.
(defmacro load-file [file-path]
  (if (cljs-env? &env)
    (@cljs-load-file file-path)
    (clojure.core/load-file file-path)))

;; It seems that naming this macro "in-ns" make the cljs compiler to crash
(defmacro cljs-in-ns [ns-quote]
  (quote ~(@cljs-in-ns ns-quote)))

#_(defmacro set-cljs-verbose [b]
  (set! cljs.repl/*cljs-verbose* b)
  b)


