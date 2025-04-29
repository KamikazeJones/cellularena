(ns a-star
  (:import [java.util PriorityQueue Comparator])
  (:require [Player :as player :refer
             [ME get-field is-protein? is-harvested-protein?]]))

(defn attacked? [game x y]
  (let [dxdy [[-1 0] [1 0] [0 -1] [0 1]]]
    (some (fn [[dx dy]]
            (let [x (+ x dx) 
                  y (+ y dy)]
              (and
               (>= x 0) (< x (game :width))
               (>= y 0) (< y (game :height))
               (let [field (get-field game x y)]
                 (and (= (field :type) :TENTACLE)
                      (= (field :owner) :ENEMY)))))) 
          dxdy)))

(defn get-neighbours-func [game]
  ;; findet die begehbaren Nachbarn eines Knotens
  (fn [node]
    (let [dxdy [[-1 0] [1 0] [0 -1] [0 1]]]
      (for [[dx dy] dxdy
            :let [x    (+ (node :x) dx)
                  y    (+ (node :y) dy)
                  field (get-field game x y)]
            :when (and
                   (>= x 0) (< x (game :width))
                   (>= y 0) (< y (game :height))
                   (or (= :EMPTY (field :type))
                       (is-protein? field))
                   (not (attacked? game x y)))]
          field))))


(defn get-cost-func [game] 
  (fn [a b]
    (if (is-protein? b)
      (if (is-harvested-protein? game b ME)
        10
        6)
      1)))

(defn reconstruct-path [came-from current]
  (loop [path [current] current current]
    (if (contains? came-from current)
      (recur (conj path (came-from current)) (came-from current))
      (reverse path))))

(defn heuristic [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))))

(defn init [start-nodes goal]
  (let [comparator (reify Comparator
                     (compare [this a b]
                       (compare (:f a) (:f b))))
        g-score (atom {})
        f-score (atom {})
        open-list (PriorityQueue. 11 comparator)]
    
       (doseq [start start-nodes]
         (swap! g-score assoc start 0)
         (swap! f-score assoc start (heuristic start goal))
         (.add open-list {:node start :f (heuristic start goal)}))
       [open-list g-score f-score]))

(defn a-star [start-nodes goal neighbours-fn cost-fn]
  (let [
        closed-set (atom #{})
        came-from (atom {}) 
        [open-list g-score f-score] (init start-nodes goal)]
    
    (loop []
      (if (.isEmpty open-list)
        nil
        (let [m (.poll open-list)
              {:keys [node]} m]
          (println "Current node:" m)
          (if (= node goal)
            (reconstruct-path @came-from node)
            (do
              (swap! closed-set conj node)
              (doseq [neighbour (neighbours-fn node)]
                (let [tentative-g-score (+ (@g-score node) (cost-fn node neighbour))]
                  (when (or (not (contains? @g-score neighbour))
                            (< tentative-g-score (@g-score neighbour)))
                    (swap! came-from assoc neighbour node)
                    (swap! g-score assoc neighbour tentative-g-score)
                    (swap! f-score assoc neighbour (+ tentative-g-score (heuristic neighbour goal)))
                    (when (not (contains? @closed-set neighbour))
                      (.add open-list {:node neighbour :f (@f-score neighbour)})))))
              (recur))))))))
