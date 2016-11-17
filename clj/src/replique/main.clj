(ns replique.main
  (:require [replique.repl :refer [start-repl-process]]))

(defn -main [opts]
  (start-repl-process (read-string opts)))

(comment
  (css
   [.class1 .class2 (class3 class4)
    {:background (:1px solid black)}
    class4 {:height :3px}]
   ['id1 .class4 {:width :1px}])
  
  )