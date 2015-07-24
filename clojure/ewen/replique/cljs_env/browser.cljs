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
          (let [head (.querySelector js/document "head")
                css-node (goog.dom.createDom
                          "link"
                          #js {:rel "stylesheet"
                               :type "text/css"
                               :href css-path})]
            (goog.cssom.removeCssRule
             (goog.cssom.getParentStyleSheet css-rule)
             (goog.cssom.getCssRuleIndexInParentStyleSheet css-rule))
            (goog.dom.appendChild head css-node))
          :else nil)))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))
