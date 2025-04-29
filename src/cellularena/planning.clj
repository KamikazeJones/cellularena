(ns cellularena.planning
   (:require [clojure.core.logic :as logic :refer [run* fresh membero distincto]])
  (:gen-class))
 
 (def === logic/==)

 (defn assign-tasks []
   (run* [q] 
         (fresh [task1 task2 task3]
                (=== q {:task1 task1 :task2 task2 :task3 task3})
                (membero task1 [:alice :bob :carol])
                (membero task2 [:alice :bob :carol])
                (membero task3 [:alice :bob :carol])
                (distincto [task1 task2 task3]))))
 
 (defn -main [& args]
   (println (assign-tasks)))
