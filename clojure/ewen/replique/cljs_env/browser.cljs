(ns ewen.replique.cljs-env.browser
  (:require [clojure.browser.repl :as repl]
            [goog.cssom]))

(defn css->path [css]
  (-> (.-href css)
      (goog.Uri.parse)
      (.getPath)))

(defn list-css-stylesheet-paths []
  (->> (goog.cssom.getAllCssStyleSheets)
       (filter #(.-href %))
       (filter #(.-insertRule %))
       (map css->path)))

(comment
  "/zz/zz1.css"
  "@import url ('../zz2.css')"
  (reload-css "/zz/zz1.css" "@import url ('../zz2.css')")
  )

(defn css-path->css-stylesheet [css-path]
  (->> (goog.cssom.getAllCssStyleSheets)
       (filter #(= css-path (css->path %)))
       first))

(defn reload-css [css-path css-text]
  (let [css (css-path->css-stylesheet css-path)
        css-node (.-ownerNode css)
        css-rule (.-ownerRule css)]
    (cond css-node
          (let [head (.querySelector js/document "head")]
            (goog.dom.removeNode css-node)
            (goog.dom.appendChild head css-node))
          css-rule
          (let [css (goog.cssom.getParentStyleSheet css-rule)]
            (goog.cssom.removeCssRule
             css (goog.cssom.getCssRuleIndexInParentStyleSheet css-rule))
            (.insertRule
             css (goog.cssom.getCssTextFromCssRule css-rule) 0))
          :else nil)))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))
