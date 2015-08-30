(ns ewen.replique.cljs
  (:require [cljs.env :as env]
            [cljs.repl :refer [repl-caught repl-quit-prompt repl-read
                               repl-prompt -repl-options read-source-map
                               *cljs-verbose* *repl-opts*
                               default-special-fns -setup evaluate-form
                               analyze-source err-out -tear-down]]
            [cljs.repl.server]
            [cljs.repl.browser]
            [cljs.closure :as cljsc]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.tagged-literals :as tags]
            [cljs.util :as util]
            [clojure.string :as str])
  (:import [java.io File PushbackReader FileWriter PrintWriter]))

;;Force :simple compilation optimization mode in order to compile to a
;;single file. With optimization :none, the REPL does not seem to work.
(alter-var-root
 #'cljs.repl.browser/compile-client-js
 (constantly
  (fn compile-client-js [opts]
    (let [copts {:optimizations :simple
                 :output-dir (:working-dir opts)}]
      ;; we're inside the REPL process where
      ;; cljs.env/*compiler* is already
      ;; established, need to construct a new one to avoid
      ;; mutating the one the REPL uses
      (cljsc/build
       '[(ns clojure.browser.repl.client
           (:require [goog.events :as event]
                     [clojure.browser.repl :as repl]))
         (defn start [url]
           (event/listen js/window
                         "load"
                         (fn []
                           (repl/start-evaluator url))))]
       copts (env/default-compiler-env copts))))))

;;Make read-get able to parse the query string
(alter-var-root #'cljs.repl.server/read-get
                (constantly
                 (fn read-get [line rdr]
                   (let [[_ path _] (str/split line #" ")
                         [path query-string] (str/split path #"\?")
                         headers (cljs.repl.server/parse-headers
                                  (cljs.repl.server/read-headers rdr))]
                     {:method :get
                      :path path
                      :headers headers}))))
