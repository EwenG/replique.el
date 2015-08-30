(ns ewen.replique.cljs-env.browser
  (:require [clojure.browser.repl :as repl]
            [goog.cssom]
            [goog.date :as date]
            [goog.Uri :as uri]))

(defn css->uri [css]
  (cond (.-href css)
        (-> (.-href css)
            (goog.Uri.parse)
            str)
        (and (.-ownerNode css)
             (.-file (.-ownerNode css)))
        (-> (.-file (.-ownerNode css))
            (goog.Uri.parse)
            str)
        :else nil))

(defn list-css-stylesheet-paths []
  (let [css-list (->> (goog.cssom.getAllCssStyleSheets)
                      (map css->uri)
                      (remove nil?))]
    css-list))

(comment
  "/zz/zz1.css"
  "@import url ('../zz2.css')"
  (reload-css "/zz/zz1.css" "@import url ('../zz2.css')")

  (js/AnimationEvent. 2)
  )

(defn css-uri->css-stylesheets [css-uri]
  (->> (goog.cssom.getAllCssStyleSheets)
       (filter #(= css-uri (css->uri %)))))

(defn reload-css-http [css]
  (let [css-node (.-ownerNode css)
        css-rule (.-ownerRule css)]
    (cond css-node
          (let [head (.querySelector js/document "head")]
            (goog.dom.removeNode css-node)
            (goog.dom.appendChild head css-node))
          (and css-rule (.-insertRule css))
          (let [css (goog.cssom.getParentStyleSheet css-rule)]
            (goog.cssom.removeCssRule
             css (goog.cssom.getCssRuleIndexInParentStyleSheet
                  css-rule))
            (.insertRule
             css (goog.cssom.getCssTextFromCssRule css-rule) 0))
          :else nil)))

(defn reload-css-file [css css-uri css-text]
  (let [head (.querySelector js/document "head")]
    (goog.dom.removeNode (.-ownerNode css))))

(defn reload-css [css-uri css-text]
  (let [css-uri (goog.Uri.parse css-uri)
        css-list (css-uri->css-stylesheets (str css-uri))]
    (cond (= "http" (.getScheme css-uri))
          (doseq [css css-list]
            (reload-css-http css))
          (= "file" (.getScheme css-uri))
          (do (doseq [css css-list]
                (reload-css-file css css-uri css-text))
              (let [css (goog.cssom.addCssText css-text)]
                (goog.dom.setProperties css (js-obj "file" (str css-uri)))))
          :else nil)))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

;; writeScriptTag is customized in clojure.browser.repl
;; Here we override writeScriptTag to include a timestamp query string
;; parameter in order to avoid hitting the browser cache when reloading
;; cljs files.
(set! (.-writeScriptTag_ js/goog)
      (fn [src opt_sourceText]
        (if repl/load-queue
          (.push repl/load-queue (array src opt_sourceText))
          (let [timestamp (.getTime (date/DateTime.))
                src-uri (-> (uri/parse src)
                            (.setParameterValue "timestamp" timestamp))]
            (set! repl/load-queue (array))
            (js/goog.writeScriptTag__ (str src-uri) opt_sourceText)))))
