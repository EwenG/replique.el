(ns ewen.replique.cljs
  (:require [cljs.env :as env]
            [cljs.repl :refer [repl-caught repl-quit-prompt repl-read
                               repl-prompt -repl-options read-source-map
                               *cljs-verbose* *repl-opts*
                               default-special-fns -setup evaluate-form
                               analyze-source err-out -tear-down]]
            [cljs.repl.server]
            [cljs.repl.browser]
            [cljs.closure :as closure]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.tagged-literals :as tags]
            [cljs.util :as util]
            [clojure.string :as str]
            [clojure.data.json :as json])
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
      (closure/build
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

;;Patch cljs.closure/output-main-file in order to:
;; - Avoid the need to provide an :asset-path option. :asset-path is
;; computed from the :main namespaces. When using a node.js env,
;; :output-dir is used instead of :asset-path
;; - Allow multiple :main namespaces. This permits leaving HTML markup
;; identical between dev and production when using google closure modules.
(alter-var-root
 #'closure/output-main-file
 (constantly
  (fn output-main-file [opts]
    (let [closure-defines (json/write-str (:closure-defines opts))
          main-requires (->> (for [m (:main opts)]
                               (str "goog.require(\""
                                    (comp/munge m)
                                    "\"); "))
                             (apply str))
          output-dir-uri (-> (:output-dir opts) (File.) (.toURI))
          output-to-uri (-> (:output-to opts) (File.) (.toURI))
          main-rel-path (.relativize output-dir-uri output-to-uri)]
      (case (:target opts)
        :nodejs
        (closure/output-one-file
         opts
         (closure/add-header
          opts
          (str
           "var path = require(\"path\");\n"
           "try {\n"
           "    require(\"source-map-support\").install();\n"
           "} catch(err) {\n"
           "}\n"
           "require(path.join(path.resolve(\".\"),\"" (:output-dir opts) "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
           "require(path.join(path.resolve(\".\"),\"" (:output-dir opts) "\",\"cljs_deps.js\"));\n"
           "goog.global.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
           "goog.require(\"cljs.nodejscli\");\n"
           main-requires)))
        (closure/output-one-file
         opts
         (str
          "(function(){"
          "var mainFile = \"" main-rel-path "\";\n"
          "var mainFileParts = mainFile.split(\"/\");\n"
          "var scripts = document.getElementsByTagName(\"script\");\n"
          "var src = scripts[0].src;\n"
          "var srcParts = src.split(\"/\");\n"
          "var assetPath = srcParts.slice(3, srcParts.length-mainFileParts.length).join(\"/\");\n"
          "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
          "if(typeof goog == \"undefined\") document.write('<script src=\"' + assetPath + '/goog/base.js\"></script>');\n"
          "document.write('<script src=\"' + assetPath + '/cljs_deps.js\"></script>');\n"
          "document.write('<script>if (typeof goog != \"undefined\") { " main-requires
          " } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
          "})();")))))))
