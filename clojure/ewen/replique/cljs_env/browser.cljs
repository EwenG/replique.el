(ns ewen.replique.cljs-env.browser
  (:require [clojure.browser.repl :as repl]
            [goog.cssom]
            [goog.date :as date]
            [goog.Uri]))

(defn add-timestamp [uri]
  (let [timestamp (.getTime (date/DateTime.))]
    (-> (goog.Uri/parse uri)
        (.setParameterValue "timestamp" timestamp)
        str)))

(defn css->uri [css]
  (cond (.-href css)
        (let [uri (goog.Uri.parse (.-href css))
              scheme (.getScheme uri)
              uri (.setQuery uri "")]
          (cond (= "http" scheme)
                (str uri)
                (= "data" scheme)
                (.-file (.-ownerNode css))
                :else nil))
        :else nil))

(defn list-css-stylesheet-paths []
  (let [css-list (->> (goog.cssom.getAllCssStyleSheets)
                      (filter #(not (.-ownerRule %)))
                      (map css->uri)
                      (remove nil?))]
    css-list))

(defn css-uri->css-stylesheets [css-uri]
  (->> (goog.cssom.getAllCssStyleSheets)
       (filter #(not (.-ownerRule %)))
       (filter #(= css-uri (css->uri %)))
       (map #(.-ownerNode %))
       (remove nil?)))

(defn reload-css-http [css]
  (let [href (.-href css)
        new-href (add-timestamp href)]
    (goog.dom.setProperties css (js-obj "href" new-href))))

(defn reload-css-file [css css-text]
  (let [data-url (str "data:text/css;base64," css-text)]
    (goog.dom.setProperties css (js-obj "href" data-url))))

(defn reload-css [css-uri css-text]
  (let [css-uri (goog.Uri.parse css-uri)
        css-list (css-uri->css-stylesheets (str css-uri))]
    (cond (= "http" (.getScheme css-uri))
          (doseq [css css-list]
            (reload-css-http css))
          (= "file" (.getScheme css-uri))
          (if (= (count css-list) 0)
            (let [data-url (str "data:text/css;base64," css-text)
                  css-node (goog.dom.createDom
                            "link"
                            (js-obj "rel" "stylesheet"
                                    "type" "text/css"
                                    "href" data-url))]
              (goog.dom.setProperties
               css-node (js-obj "file" (str css-uri)))
              (goog.dom.append
               (.querySelector js/document "head")
               css-node))
            (doseq [css css-list]
              (reload-css-file css css-text)
              (goog.dom.setProperties
               css (js-obj "file" (str css-uri)))))
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
