(ns ewen.replique.cljs-env.browser
  (:require [cljs.reader :as reader]
            [clojure.browser.repl :as repl]
            [goog.cssom]
            [goog.date :as date]
            [goog.Uri]))


  (defn add-timestamp [uri]
    (let [timestamp (.getTime (date/DateTime.))]
      (-> (goog.Uri/parse uri)
          (.setParameterValue "timestamp" timestamp)
          str)))

(defn remove-query-string [uri]
  (-> (goog.Uri.parse uri)
      (.setQuery "")
      str))

(defn css-infos [css]
  (cond (and (.-href css)
             (= "data" (.-scheme (.-ownerNode css))))
        {:scheme "data"
         :uri  (.-href css)
         :file-path (.-filepath (.-ownerNode css))}
        (.-href css)
        {:scheme "http"
         :uri (remove-query-string (.-href css))}
        :else nil))

(defn list-css-infos []
  (let [css-list (->> (goog.cssom.getAllCssStyleSheets)
                      ;; Filter out css files imported with the
                      ;; @import directive
                      (filter #(.-ownerNode %))
                      (map css-infos)
                      (remove nil?))]
    css-list))

(defn match-css-infos? [css {:keys [scheme uri file-path]}]
  (cond (= scheme "http")
        (= uri (remove-query-string (.-href css)))
        (= scheme "data")
        (= file-path (.-filepath (.-ownerNode css)))
        :else false))

(defn css-infos->css-stylesheets [css-infos]
  (->> (goog.cssom.getAllCssStyleSheets)
       ;; Filter out css files imported with the
       ;; @import directive
       (filter #(.-ownerNode %))
       (filter #(match-css-infos? % css-infos))
       (map #(.-ownerNode %))))

(defn reload-css-http [css-node]
  (let [href (.-href css-node)
        new-href (add-timestamp href)]
    (goog.dom.setProperties css-node (js-obj "href" new-href))))

(defn reload-css-file [css-node data-uri]
  (goog.dom.setProperties css-node (js-obj "href" data-uri)))

(defn reload-css [css-infos]
  (let [{:keys [scheme uri file-path]
         :as css-infos}
        (reader/read-string css-infos)
        css-list (css-infos->css-stylesheets css-infos)]
    (cond (= "http" scheme)
          (doseq [css css-list]
            (reload-css-http css))
          (= "data" scheme)
          (if (= (count css-list) 0)
            (let [css-node (goog.dom.createDom
                            "link"
                            (js-obj "rel" "stylesheet"
                                    "type" "text/css"
                                    "href" uri))]
              (goog.dom.setProperties
               css-node (js-obj "scheme" "data"
                                "filepath" file-path))
              (goog.dom.append
               (.querySelector js/document "head")
               css-node))
            (doseq [css css-list]
              (reload-css-file css uri)
              (goog.dom.setProperties
               css (js-obj "scheme" "data"
                           "filepath" file-path))))
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
          (let [new-src (add-timestamp src)]
            (set! repl/load-queue (array))
            (js/goog.writeScriptTag__ new-src opt_sourceText)))))
