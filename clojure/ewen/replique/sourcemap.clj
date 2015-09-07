(ns ewen.replique.sourcemap
  (:import [java.util Base64]
           [java.nio.charset StandardCharsets]
           [org.json JSONObject JSONArray]))

;;/*# sourceMappingURL=data:application/json;base64,ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJlc2V0LnNjc3MiCgldLAoJInNvdXJjZXNDb250ZW50IjogW10sCgkibWFwcGluZ3MiOiAiQUFHRSxHQUFHLENBQUMsQ0FBQyxDQUFIO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWDs7QUFLSCxDQUFDLENBQUMsQ0FBQyxDQUFEO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWCIsCgkibmFtZXMiOiBbXQp9 */

(defn decodeBase64 [s-b64]
  (if (nil? s-b64)
    nil
    (-> (Base64/getDecoder)
        (.decode s-b64)
        (String.))))

(defn encodeBase64 [s]
  (if (nil? s)
    nil
    (let [b (.getBytes s StandardCharsets/UTF_8)]
      (-> (Base64/getEncoder)
          (.encode b)
          (String.)))))

(defn parse-json [s]
  (if (nil? s)
    nil
    (JSONObject. ^String s)))

(defprotocol org-json-clj
  (org-json->clj [o]))

(extend-protocol org-json-clj
  JSONObject
  (org-json->clj [o]
    (let [keys (.keySet o)
          vals (map #(org-json->clj (.get o %)) keys)]
      (zipmap (map keyword keys) vals)))
  JSONArray
  (org-json->clj [a]
    (mapv org-json->clj a))
  nil
  (org-json->clj [o] nil)
  Object
  (org-json->clj [o] o))

(def sourcemapping-regexp #"(?:\/\/[@#][ \t]+sourceMappingURL=([^\s'\"]+?)[ \t]*$)|(?:\/\*[@#][ \t]+sourceMappingURL=([^\*]+?)[ \t]*(?:\*\/)[ \t]*$)")
(def data-uri-regexp #"^data:[^;]*(?:;base64)?,(.*)$")

(def parse-data-uri
  (fnil
   (fn [s]
     (let [matcher (re-matcher data-uri-regexp s)]
       (second (re-find matcher))))
   ""))

(def parse-sourcemap
  (fnil
   (fn [s]
     (let [matcher (re-matcher sourcemapping-regexp s)]
       (-> (re-find matcher)
           (nth 2)
           parse-data-uri
           decodeBase64
           parse-json
           org-json->clj)))
   ""))

(comment

  (parse-sourcemap "a {
  color: red; }

a {
  color: blue; }

p a {
  color: blue; }

/*# sourceMappingURL=data:application/json;base64,ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJ1bm5hYmxlcy9fcmVzZXQuc2NzcyIKCV0sCgkic291cmNlc0NvbnRlbnQiOiBbCgkJIkBpbXBvcnQgJ19yZXNldCc7XG5cbmEge1xuICAgIGNvbG9yOiBibHVlO1xuICB9XG5cbnAge1xuICBhIHtcbiAgICBjb2xvcjogYmx1ZTtcbiAgfVxufSIsCgkJImEge1xuICAgIGNvbG9yOiByZWQ7XG59XG4iCgldLAoJIm1hcHBpbmdzIjogIkFDQUEsQ0FBQyxDQUFDO0VBQ0UsS0FBSyxFQUFFLEdBQUksR0FEWjs7QURFSCxDQUFDLENBQUM7RUFDRSxLQUFLLEVBQUUsSUFBSyxHQURiOztBQUtELENBQUMsQ0FBQyxDQUFDLENBQUQ7RUFDQSxLQUFLLEVBQUUsSUFBSyxHQURYIiwKCSJuYW1lcyI6IFtdCn0= */
")

  )

(comment
  (eshell-command-result
   (format "%srunnables/replique_sass \"%s\" \"%s\" \"%s\" %s"
           (replique/replique-root-dir)
           "@import '_reset';

a {
    color: blue;
  }

p {
  a {
    color: blue;
  }
}"
           (concat (replique/replique-root-dir) "test2.scss")
           (concat (replique/replique-root-dir) "test2.css")
           (concat (replique/replique-root-dir) "runnables"))))


(comment

  ;;sourceRoot: 'http://example.com/www/js/'
  (let [s (decodeBase64 "ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJIi4uLy4uLy4uL3J1bm5hYmxlcy9fcmVzZXQuc2NzcyIKCV0sCgkic291cmNlc0NvbnRlbnQiOiBbCgkJIkBpbXBvcnQgJ19yZXNldCc7XG5cbmEge1xuICAgIGNvbG9yOiBibHVlO1xuICB9XG5cbnAge1xuICBhIHtcbiAgICBjb2xvcjogYmx1ZTtcbiAgfVxufSIsCgkJImEge1xuICAgIGNvbG9yOiByZWQ7XG59XG4iCgldLAoJIm1hcHBpbmdzIjogIkFDQUEsQ0FBQyxDQUFDO0VBQ0UsS0FBSyxFQUFFLEdBQUksR0FEWjs7QURFSCxDQUFDLENBQUM7RUFDRSxLQUFLLEVBQUUsSUFBSyxHQURiOztBQUtELENBQUMsQ0FBQyxDQUFDLENBQUQ7RUFDQSxLQUFLLEVBQUUsSUFBSyxHQURYIiwKCSJuYW1lcyI6IFtdCn0=")
        json-map (org-json->clj (JSONObject. ^String s))
        sources (:sources json-map)
        sourceRoot (:sourceRoot json-map)]
    json-map
    #_sourceRoot
    #_s
    )

  )
