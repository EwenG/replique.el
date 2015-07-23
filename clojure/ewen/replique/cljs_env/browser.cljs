(ns ewen.replique.cljs-env.browser
  (:require [clojure.browser.repl :as repl]
            [goog.cssom]))

(defn css->path [css]
  (-> (.-href css)
      (goog.Uri.parse)
      (.getPath)))

(defn list-css-paths []
  (->> (.querySelectorAll js/document "link[rel='stylesheet']")
       array-seq
       (map css->path)))

#_(defn css-path->tag [css-path]
  (let [css-path (if (goog.string.startsWith css-path "/")
                   (goog.string.remove css-path "/")
                   css-path)]
    (.querySelector js/document (str "link[href$='" css-path "']"))))

(defn list-css-stylesheets []
  (map css->path (goog.cssom.getAllCssStyleSheets)))

(comment
  "/zz/zz1.css"
  "@import url (\"../zz2.css\")"

  )

(defn reload-css [css-path css-text]
  (.log js/console css-path)
  (.log js/console css-text))

#_(defn reload-css [css]
  (if-let [href (.-href css)]
    (let [idx (.indexOf href "?")
          last-reload (str "last_reload=" (.getTime (js/Date.)))]
      (cond
        (< idx 0)
        (aset css "href" (str href "?" last-reload))
        (< (.indexOf href "last_reload" idx) 0)
        (aset css "href" (str href "&" last-reload))
        :else (.replace href #"last_reload=\d+" last-reload))
      (.-href css))))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))
