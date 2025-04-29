(ns cellularena.simulation
  (:require [Player :refer [ME ENEMY]]))

(defn swap-owner [game]
  (let [field-map (game :field)]
    (loop [field-map field-map new-field-map {}]
      (if (empty? field-map)
        (assoc game :field new-field-map)
        (let [[pos fmap] (first field-map)
              new-field (assoc fmap :owner
                               (if (= (fmap :owner) ME)
                                 ENEMY
                                 (if (= (fmap :owner) ENEMY)
                                   ME
                                   (fmap :owner))))]
          (recur (rest field-map) (assoc new-field-map pos new-field)))))))

(defn swap-resources [game]
  (let [myA (game :myA) myB (game :myB) myC (game :myC) myD (game :myD)
        oppA (game :oppA) oppB (game :oppB) oppC (game :oppC) oppD (game :oppD)]
    (assoc game :myA oppA :myB oppB :myC oppC :myD oppD
           :oppA myA :oppB myB :oppC myC :oppD myD)))

(defn swap-game [game]
  (swap-resources (swap-owner game)))

(defn clean-up-clashes [game1 game2])

(defn simulate-turn [game strategy]
  (strategy game))

(defn simulate-round [game strategy-player-1 strategy-player-2]
  (let [g1 (simulate-turn game strategy-player-1)
        g2 (simulate-turn (swap-game game) strategy-player-2)]
    (clean-up-clashes g1 g2)))