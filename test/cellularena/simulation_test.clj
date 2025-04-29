(ns cellularena.simulation-test
  (:require [clojure.test :refer [deftest is testing with-test-out]]
             [clojure.string :as str]
             [Player :refer [with-input-from-file ME ENEMY 
                             parse-turn parse-game place-element
                             show-arena]]
             [cellularena.simulation :refer :all]))

(defn setup-game []
  (let [game
        (with-input-from-file "test/resources/test-input-1.txt"
          (fn []
            (parse-turn (parse-game))))
        game (place-element game 14 2 :A)]
    game))

(deftest swap-owner-test
  (testing "tausche owner in game"
    (let [game (setup-game)
          swapped-owner (swap-owner game)]
      (with-test-out
        (println)
        (println "swap-owner-test")
        (show-arena game)
        (println "swapped-owner")
        (show-arena swapped-owner))
      (is (= (get-in game [:field [1 2] :owner]) ME))
      (is (= (get-in swapped-owner [:field [1 2] :owner]) ENEMY)))))

(deftest swap-game-test
  (testing "tausche owner in game"
    (let [game (setup-game)
          swapped-game (swap-game game)]
      (with-test-out
        (println)
        (println "swap-owner-test")
        (show-arena game)
        (println "swapped-game")
        (show-arena swapped-game))
      (is (= (get-in game [:field [1 2] :owner]) ME))
      (is (= (get-in swapped-game [:field [1 2] :owner]) ENEMY)))))

