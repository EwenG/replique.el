(ns ewen.replique.main
  (:require [ewen.replique.server :refer [start-repl-process]]
            [ewen.replique.tooling]
            [ewen.replique.replique-conf]))

(defn -main [opts]
  (start-repl-process (read-string opts)))

(comment
  (css
   [.class1 .class2 (class3 class4)
    {:background (:1px solid black)}
    class4 {:height :3px}]
   ['id1 .class4 {:width :1px}])
  
  )
