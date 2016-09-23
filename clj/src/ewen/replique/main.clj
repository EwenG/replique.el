(ns ewen.replique.main
  (:require [ewen.replique.server :refer [start-repl-process]]
            [ewen.replique.tooling]))

(defn -main [opts]
  (start-repl-process (read-string opts)))
