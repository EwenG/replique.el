(ns ewen.replique.sourcemap
  (:import [java.util Base64]
           [java.nio.charset StandardCharsets]
           [org.json JSONObject JSONArray]))

;;/*# sourceMappingURL=data:application/json;base64,ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJlc2V0LnNjc3MiCgldLAoJInNvdXJjZXNDb250ZW50IjogW10sCgkibWFwcGluZ3MiOiAiQUFHRSxHQUFHLENBQUMsQ0FBQyxDQUFIO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWDs7QUFLSCxDQUFDLENBQUMsQ0FBQyxDQUFEO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWCIsCgkibmFtZXMiOiBbXQp9 */


(defn decodeBase64 [s-b64]
  (-> (Base64/getDecoder)
      (.decode s-b64)
      (String.)))

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
  Object
  (org-json->clj [o] o))


(comment
  (let [s (decodeBase64 "ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJlc2V0LnNjc3MiCgldLAoJInNvdXJjZXNDb250ZW50IjogW10sCgkibWFwcGluZ3MiOiAiQUFHRSxHQUFHLENBQUMsQ0FBQyxDQUFIO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWDs7QUFLSCxDQUFDLENBQUMsQ0FBQyxDQUFEO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWCIsCgkibmFtZXMiOiBbXQp9")]
    (org-json->clj (JSONObject. ^String s)))

  )
