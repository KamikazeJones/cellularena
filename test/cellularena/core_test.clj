(ns cellularena.core-test
  (:require [clojure.test :refer [deftest is testing with-test-out]]
            [clojure.test.junit :refer :all]
            [clojure.string :as str]
            [Player :refer :all]))

(deftest parse-game-test
  (testing "Parsen aus einer Input-Datei"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-game)))]
      (is (= (game :width) 18))
      (is (= (game :height) 9)))))

(deftest parse-turn-test
  (testing "Parsen aus einer Input-Datei"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (is (= (game :width) 18))
      (is (= (game :height) 9))
      (is (= (game :entityCount) 89))
      (let [field (game :field)]
        (is (= ((field [0 0]) :type) :WALL))
        (is (= (field [1 1]) nil))
        (is (= ((field [4 3]) :type) :BASIC))
        (is (= ((field [3 7]) :type) :HARVESTER))
        (is (= ((field [1 6]) :type) :ROOT))))))

(deftest get-field-test
  (testing "get-field liefert 'EMPTY' für leere Felder"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (is (= (get-field game 1 1 :type) :EMPTY))
      (is (= (get-field game 17 1 :owner) -1)))))

(deftest field-empty?-test
  (testing "field-empty? liefert true für leere Felder"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (is (field-empty? game 1 1))
      (is (not (field-empty? game 4 3))))))

(deftest get-type-list-test
  (testing "get-type-list liefert eine Liste der Typen"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (is (= (count (get-type-list game #(= (% :type) :WALL))) 65))
      (is (= (count (get-type-list game #(= (% :type) :A))) 0))
      (is (= (count (get-type-list game #(= (% :type) :ROOT))) 2))
      (is (= (count (get-type-list game #(and (= (% :type) :ROOT) (= (% :owner) 1)))) 1))
      (is (= (let [root
                   (first (get-type-list game
                                         #(and (= (% :type) :ROOT)
                                               (= (% :owner) 1))))]
               [(root :x) (root :y)]) [1 2])))))

(deftest get-organ-list-test
  (testing "get-organ-list liefert eine Liste der Organe"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (is (= (count (get-organ-list-for-owner game ME)) 12)
          "12 eigene Organe")
      (is (= (count (get-organ-list-for-owner game ENEMY)) 12)
          "12 gegnerische Organe")
      (is (not= (count (get-organ-list-for-organism game (get-field game 0 7))) 12)
          "0 7 ist eine WALL, und es gibt 65 mit der gleichen Root-Id 0")
      (is (= (count (get-organ-list-for-organism game (get-field game 1 2))) 12)
          "Es sollten 12 Organe gefunden werden für den Organismus mit Root [1 2]")
      (let [root-organ
            (first (filter #(= (% :type) :ROOT) (get-organ-list-for-owner game ME)))]
        (is (= [(root-organ :x) (root-organ :y)] [1 2]))))))

(deftest get-field-repr-test
  (let  [field {:type :ROOT :owner ME}]
    (testing "get-field-repr liefert die richtige Repräsentation"
      (is (= (get-field-repr field) "R")))))

(deftest show-arena-test
  (testing "show-arena zeigt die Arena an"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))]
      (with-test-out
        (println "")
        (println "Debug show-arena:")
        (show-arena game))
      (is (nil? (show-arena game))))))

(deftest replay-test
  (testing "spielt ein Spiel anhand einer Datei nach"
    (with-test-out
      (println "")
      (println "Replay:")
      (replay "test/resources/game-input.txt" 7))
    (is (nil? (replay "test/resources/game-input.txt")))))

(deftest contains-root-id?-test
  (testing "prüfe die root-ids in einer Liste von Root-Organen"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))
          enemy-roots (vec (get-root-cells game ENEMY))
          roots (vec (get-root-cells game ME))
          all-roots (into enemy-roots roots)
          o-info-list (vec (map #(create-organ-info %) roots))
          new-organs (vec (filter #(not (contains-root-id?
                                         o-info-list
                                         (% :organId))) all-roots))
          new-o-info-list (vec (add-elements o-info-list (map #(create-organ-info %) new-organs)))]
      (is (= (count all-roots) 2)
          "Es gibt insgesamt 2 Root-Organe")
      (is (= (count roots) 1)
          "Es gibt 1 Root-Organ für den Spieler")
      (is (= (if (contains-root-id? o-info-list 1) true false) true)
          "Root-Organ mit der Id 1 ist vorhanden")
      (debug (str "o-info-list: " o-info-list))
      (debug (str "new-organs" new-organs))
      (debug (str "new-organs ist ein vector? " (vector? new-organs)))

      (is (= (count new-organs) 1)
          "Das Enemy-Root-Organ mit der Id 2 sollte neu sein")
      (is (= (count new-o-info-list) 2)
          "Es gibt insgesamt 2 Root-Organe")
      (debug (str "new-o-info-list: " new-o-info-list)))))

(deftest has-resources?-test
  (testing "prüfe, ob ein Organismus genug Ressourcen für eine Aktion hat"
    (let [g1 {:myA 0 :myB 5 :myC 2 :myD 1}
          g2 {:myA 5 :myB 5 :myC 2 :myD 2}]
      (is (= (has-resources? g1 ME :SPORE) false)
          "Der Organismus hat nicht genug Ressourcen für 'SPORE'")
      (is (= (has-resources? g2 ME :ROOT) true)
          "Der Organismus hat genug Ressourcen für 'ROOT'")
      (is (= (has-resources? g2 ME :SPORE) true)
          "Der Organismus hat genug Ressourcen für 'SPORE'")
      (is (= (has-resources? g1 ME :BASIC) false)
          "Der Organismus hat nicht genug Ressourcen für 'BASIC'")
      (is (= (has-resources? g2 ME :BASIC) true)
          "Der Organismus hat genug Ressourcen für 'BASIC'"))))

(deftest priority-queue-test
  (testing "Priority-Queue"
    (let [q (create-priority-queue 20 distance-comparator)
          o1 {:distance 10 :value "A"}
          o2 {:distance 15 :value "B"}
          o3 {:distance 35 :value "C"}
          o4 {:distance 4  :value "D"}
          o5 {:distance 17 :value "E"}]
      (is (= (count q) 0))
      (.add q o1)
      (.add q o2)
      (.add q o3)
      (.add q o4)
      (.add q o5)
      (is (= (count q) 5))
      (is (= (.peek q) o4))
      (is (= (count q) 5))
      (is (= (.poll q) o4))
      (is (= (count q) 4)))))

;; (deftest enqueue-direction-test
;;   (testing "füge Felder in die Priority-Queue ein"
;;     (let [game
;;           (with-input-from-file "test/resources/test-input-1.txt"
;;             (fn []
;;               (parse-turn (parse-game))))
;;           q (create-priority-queue 20 distance-comparator)]
;;       (loop [y 0]
;;         (when (< y (game :height))
;;           (loop [x 0]
;;             (when (< x (game :width))
;;               (do
;;                 (enqueue q (get-field game 1 2) (get-field game x y))
;;                 (recur (inc x)))))
;;           (recur (inc y))))
;;       (is (= (count q) (* (game :width) (game :height))))
;;       (is (= (get-direction (get-field game 1 2) (get-field game 1 3)) "S"))
;;       (is (= (get-direction (get-field game 1 2) (get-field game 2 2)) "E"))

;;       (let [g2 (place-element game 1 3 :A)
;;             prot (some identity (get-neighbour-fields g2 1 2))]
;;         (with-test-out
;;           (show-arena g2)
;;           (println (str "proteins " prot))
;;           (is (= (nil? prot) false))
;;           (is (= (prot :type) :A))
;;           (is (= (get-direction (get-field g2 1 2) prot) "S")
;;               "Die Richtung zum Protein sollte S sein"))))))

(defn setup-game []
  (let [game
        (with-input-from-file "test/resources/test-input-1.txt"
          (fn []
            (parse-turn (parse-game))))
        game (place-element game 14 2 :A)]
    game))

(deftest get-neighbours-func-test
  (testing "get-neighbours-func liefert die Nachbarn eines Knotens"
    (let [game (setup-game)
          get-neighbours (get-neighbours-func game)]
      (with-test-out
        (println)
        (println "get-neighbours-func-test")
        (show-arena game))
      (is (= (count (get-neighbours (get-field game 1 2))) 2))
      (is (= (count (get-neighbours (get-field game 14 2))) 4)))))

(deftest a-star-init-test
  (testing "a-star-init"
    (let [game
          (with-input-from-file "test/resources/test-input-1.txt"
            (fn []
              (parse-turn (parse-game))))
          start-nodes [(get-field game 1 2)]
          goal (get-field game 4 1)
          [open-list g-score f-score] (init start-nodes goal)]

      (is (= (count open-list) 1))
      (is (= (count @g-score) 1))
      (is (= (count @f-score) 1))
      (is (= ((first open-list) :node) (first start-nodes))
          "Der erste Knoten in der Open-Liste sollte der Startknoten sein")
      (is (= ((first open-list) :f) 4)))))


(defn mark-path [game path]
  (loop [previous (first path)
         path (rest path)
         game game]
    (if (empty? path)
      game
      (let [node (first path)]
        (recur node (rest path)
               (assoc-in game [:field [(previous :x) (previous :y)]]
                         {:type (get-direction previous node)}))))))

(deftest a-star-test
  (testing "a-star test"
    (let [game (setup-game)]
      (with-test-out
        (println)
        (println "a-star-test")
        (show-arena game)
        (let [path (a-star [(get-field game 1 2)]
                           (get-field game 6 6)
                           (get-neighbours-func game)
                           (get-cost-func game))]
          (is (= (count path) 32)
              "Der Pfad sollte 32 Knoten lang sein")

          (is (= (first path) (get-field game 1 2)))
          (let [mark (mark-path game path)]
            (with-test-out
              (println)
              (println "a-star test: mark-path")
              (show-arena mark))
            (is (= (get-field mark 1 1 :type) "E")
                "Das Feld 2 2 sollte einen Pfad nach Osten zeigen")))))))
      
(deftest a-star-organ-test
  (testing "a-star-organ-test"
    (let [game (setup-game)
          organs (get-organ-list-for-organism game (get-field game 1 2))
          a-protein (first (get-type-list game #(= (% :type) :A)))
          right-to-protein (get-field game (inc (a-protein :x)) (a-protein :y))
          neighbours-func (get-neighbours-func game)
          cost-func       (get-cost-func game)]

      (with-test-out
        (println)
        (println "a-star-organ-test von Organismus 1 2 zu Protein A")
        (show-arena game)
        (println "Organe:" organs)
        (println "Protein A:" a-protein))

      (is (= (count organs) 12)
          "Es sollten 12 Organe gefunden werden für den Organismus mit Root [1 2]")

      (let [a-star-init (init organs right-to-protein)]
        (with-test-out
          (println)
          (println "a-star-organ-test: a-star-init")
          (println "Organe:" organs)
          (println "Right-To-Protein:" right-to-protein)
          (println "a-star-init:" a-star-init))
        
        (is (= (count (first a-star-init)) 12)
            "Die Open-Liste sollte 12 Knoten enthalten"))

      (let [path (a-star organs a-protein neighbours-func cost-func)]
        (is (= (count path) 4)
            "Der Pfad sollte 4 Knoten lang sein")
        (is (= (first path) (get-field game 11 2))
            "Der erste Knoten sollte der ganz links sein")
        (is (= (last path) a-protein)
            "Der letzte Knoten sollte a-protein sein")
        (let [mark (mark-path game path)]
          (with-test-out
            (println)
            (println "a-star-organ-test: mark-path")
            (show-arena mark) 
            (is (= (get-field mark 12 2 :type) "E")
              "Das Feld 13 2 sollte einen Pfad nach Osten zeigen")))))))


(deftest is-harvested-protein?-test
  (testing "Wird das Protein geerntet?"
    (let [g (setup-game)
          game (-> g 
                   (place-element 4 1 :A) 
                   (place-element 12 1 :C)
                   (place-organ 4 2 :HARVESTER ME "N") 
                   (place-organ 11 1 :HARVESTER ME "E"))]
      (with-test-out
        (println)
        (println "is-harvested-protein?-test")
        (show-arena game))
        (is (= (some? (is-harvested-protein? game (get-field game 4 1))) true) 
                "Das Protein A an Position 4 1 sollte geerntet werden")
        (is (= (some? (is-harvested-protein? game (get-field game 12 1))) true)
            "Das Protein C an Position 12 1 sollte geerntet werden"))))

(deftest combine-arenas-test
  (testing "Gebe mehrere Arenen aus"
    (let [arena1 ["A1" "A2" "A3"]
          arena2 ["B1" "B2" "B3"]
          arena3 ["C1" "C2" "C3"]
          arenas-lines (combine-arenas [arena1 arena2 arena3])]
      (with-test-out
        (println)
        (println "combine-arenas-test")
        (println "ich mache gleich doseq")
        (println arenas-lines)

        (doseq [line arenas-lines]
          (println line)))

      (is (= (count arenas-lines) 3))
      (is (= (count arenas-lines) 3))
      (is (= (first arenas-lines) "A1 B1 C1"))
      (is (= (second arenas-lines) "A2 B2 C2"))
      (with-test-out
        (println)
        (println "show-arenas-test")
        (show-arenas [arena1 arena2 arena3]))
      (is (nil? (show-arenas [arena1 arena2 arena3]))))))

(deftest get-harvested-proteins-test
  (testing "Welche Proteine werden geerntet?"
    (let [g (setup-game)
          game (-> g
                   (place-element 4 1 :A)
                   (place-element 12 1 :C)
                   (place-organ 4 2 :HARVESTER ME "N")
                   (place-organ 11 1 :HARVESTER ME "E")
                   (place-organ 12 2 :HARVESTER ME "S")
                   (place-organ 13 2 :HARVESTER ME "E"))
          ]
      (with-test-out
        (println)
        (println "get-harvested-proteins-test")
        (show-arena game)
        (let [proteins (get-harvested-proteins game ME)
              prot-strings (map #(str (name %) ": " (get proteins %)) [:A :B :C :D])] 
           (println (str/join ", " prot-strings))

        (is (= (proteins :A) 2)
            "Ein Protein A sollte geerntet werden")
        (is (= (proteins :C) 1)
            "Ein Protein C sollte geerntet werden"))))))
