(ns Player
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:import [java.util PriorityQueue Comparator])
  (:gen-class))

(def ENEMY 0)
(def ME 1)
(defn output [msg] (println msg) (flush))
(defn debug [msg] (binding [*out* *err*] (println msg) (flush)))

(declare get-neighbour-fields get-field is-protein? is-harvested-protein?)
(declare get-field grow-type spore wait)

; Grow and multiply your organisms to end up larger than your opponent.

; Plan für heute Abend:
; - Simulation des Spiels
;   - ME and ENEMY ziehen abhängig voneinander
;   - Dazu wird in game2 der owner in game vertauscht: 
;     Alle ME Organe werden ENEMY und umgekehrt
;   - eine Strategie muss das durch den Spielzug veränderte game
;     zurückgeben  
;   - Abgleich von game1 und game2:
;     Nach beiden Zügen werden Felder, die von beiden Spielern
;     bewachsen wurden, zu WALLs
;   
; - Herausfinden was passiert, wenn ein Spieler gleichzeitig
;   auf das gleiche Feld wächst
;
; -----------------------------------------------------------------------------
;
; Eine Sequenz ist eine Abfolgen von primitiven Aktionen.
; Eine primitive Aktion ist entweder GROW, SPORE oder WAIT.
; Ein Plan ist ein strategischer Spielzug einer Abfolge von 
; Sequenzen im Zusammenspiel mit mehreren Organismen.
;
; Eine Strategie muss den Fortschritt des Plans überwachen und ggf. auf
; Veränderungen der Bedingungen eingehen.
; Wurden zum Beispiel vom Gegner Harvester zerstört, so muss darauf eingegangen
; werden, dass nun weniger Protein vorhanden ist. Es muss dann ggf. eine andere 
; Strategie gewählt werden, um den Gegner aus dem eigenen
; Territorium zu vertreiben.
; 
; -----------------------------------------------------------------------------

(defn add-func-step [sequenz f & args]
  (conj sequenz [:FUNC f (vec args)]))

(defn add-grow-step [sequenz sx sy tx ty type dir]
  (conj sequenz [:GROW sx sy tx ty type dir]))

(defn add-spore-step [sequenz sx sy tx ty]
  (conj sequenz [:SPORE sx sy tx ty]))

(defn add-wait-step [sequenz]
  (conj sequenz [:WAIT]))

(defn execute-step [game oinfo steps]
  (let [step (first steps)
        action 
        (if (= (first step) :FUNC)
          (apply (second step) game oinfo (nth step 3))
          (first step))] 
    (cond
      (= action :GROW)
      (let [[sx sy tx ty type dir] step
            organId (get-field game sx sy :organId)]
        (if organId 
          (do (grow-type organId tx ty type dir) (rest steps))
          (do (wait) [])))

      (= action :SPORE)
      (let [[sx sy tx ty] step
            organId (get-field game sx sy :organId)]
        (if organId 
          (do (spore organId tx ty) (rest steps)) 
          (do (wait) [])))
      
      (= action :WAIT)
      (do (wait) (rest steps)))))

; (def create-wall-plan [game]
  
  
  
; -----------------------------------------------------------------------------
; Es gibt verschiedene Gewinnstrategien:
; - "Berliner Mauer" Strategie
;     a) Das größere Territorium permanent abgrenzen (über WALL oder TENTACLE)
;     b) ist das eigene, gesicherte Territorium kleiner als das des Gegners,
;        dann mache das gegnerische Territorium kleiner, indem Mauern 
;        im feindlichen Gebiet gebaut werden
;
;     c) Platziert man zwei Sporer direkt nebeneinander, so können diese
;        eine Mauer bilden. Dazu müssen beide Sporer auf das gleiche Feld
;        zielen.
;
; - "Dart Manöver"
;      Zwei Root-Elemente ins feindliche Territorium platzieren und dann
;      eine Pfeil-Formation an TENTACLE bauen, um das Gebiet zu sichern

;         <o  
;        <o 
;       <o 
;      <o  
;     <RR
;      <o
;       <o
;        <o 
;         <o 

; - "Quick Kill" Strategie
;      Alle Organismen des Gegners zerstören per Tentacle
;
; - "Invade and Destroy" Strategie
;       In das Territorium des Gegners eindringen und alle Proteine zerstören,
;       so dass dem Gegner das Wachsen unmöglich gemacht wird.
;       Ziele könnten sein:
;          * gezielt bestimmte Proteine zerstören, so dass der Gegner 
;             a) keine TENTACLE mehr bauen kann 
;             b) keine HARVESTER mehr bauen kann
;             c) keine SPORER mehr bauen kann
;

; ---------------------------------------------------------------------
; A* Algorithmus

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
  (let [closed-set (atom #{})
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
; ---------------------------------------------------------------------
; Simulation of the game

; ---------------------------------------------------------------------
(defn max-id [game]
  (let [field-map (game :field)]
    (reduce (fn [max-id field]
              (if (and (not (nil? (field :organId)))
                       (> (field :organId) max-id))
                (field :organId)
                max-id))
            0
            (vals field-map))))

(defn place-element 
  "platziere :WALL oder ein Protein (:A :B :C :D)"
  [game x y type]
  
  (assoc-in game [:field [x y]]
            {:x x
             :y y
             :type type
             :owner -1
             :organId 0
             :organDir "X"
             :organParentId 0
             :organRootId 0}))

; x y type owner organId organDir organParentId organRootId
(defn place-organ 
  "platziere ein Organ (:ROOT, :BASIC, :TENTACLE, :HARVESTER, :SPORER)"
  ([game x y type owner organId organDir organParentId rootId]
   (assoc-in game [:field [x y]]
             {:x x
              :y y
              :type type
              :owner owner
              :organId organId
              :organDir organDir
              :organParentId organParentId
              :rootId rootId}))

  ([game x y type owner organDir]
   (let [new-id (inc (max-id game))
         orth-neighbours (get-neighbour-fields game x y)
         parent (some #(when (= owner (% :owner)) %) orth-neighbours)
         organParentId (if (nil? parent) 0 (parent :organId))
         organRootId (if (nil? parent) 0 (parent :organRootId))]
     (place-organ game x y type owner new-id organDir organParentId organRootId))))

; START PARSING -------------------------------------------------------
(defn readline []
  (let [line (read-line)]
    ;(debug (str "; " line))
    (if (= line "# END #") nil line)))

(defn parse-game []
  (let [; game: dictionary with all game information
        ; width: columns in the game grid
        ; height: rows in the game grid
        game {}
        [width height] (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (readline) #" ")))
        new-game (-> game (assoc :width width)
                     (assoc :height height)
                     (assoc :field {}))]
    new-game))

(defn parse-field [g]
  (let [line (readline)]
    ; (debug (str "parse-field: " line))
    (if (nil? line) nil
        (let [entityCount (Integer/parseInt line)
              game (assoc g :entityCount entityCount)]
          (loop [i entityCount field {}]
            (if (= i 0) (assoc game :field field)
                (let [; y: grid coordinate
                      ; type: WALL, ROOT, BASIC, TENTACLE, HARVESTER, SPORER, A, B, C, D
                      ; owner: 1 if your organ, 0 if enemy organ, -1 if neither
                      ; organId: id of this entity if it's an organ, 0 otherwise
                      ; organDir: N,E,S,W or X if not an organ
                      [x y type owner organId organDir organParentId organRootId]
                      (filter #(not-empty %) (str/split (readline) #" "))
                      x (Integer/parseInt x)
                      y (Integer/parseInt y)
                      owner (Integer/parseInt owner)
                      organId (Integer/parseInt organId)
                      organParentId (Integer/parseInt organParentId)
                      organRootId (Integer/parseInt organRootId)
                      new-field (assoc field [x y] {:x x
                                                    :y y
                                                    :type (keyword type)
                                                    :owner owner
                                                    :organId organId
                                                    :organDir organDir
                                                    :organParentId organParentId
                                                    :organRootId organRootId})]

                  (recur (dec i) new-field))))))))

(defn parse-game-stats [g]
  (let [; myD: your protein stock
        [myA myB myC myD] (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (readline) #" ")))
      ; oppD: opponent's protein stock

        [oppA oppB oppC oppD] (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (readline) #" ")))
      ; your number of organisms, output an action for each one in any order
        requiredActionsCount (Integer/parseInt (readline))

        game (assoc g :myA  myA  :myB  myB  :myC  myC  :myD  myD
                    :oppA oppA :oppB oppB :oppC oppC :oppD oppD
                    :requiredActionsCount requiredActionsCount)]
    game))

(defn parse-turn [game]
  (let [g (parse-field game)]
    (if (nil? g) nil (parse-game-stats g))))

; END PARSING -------------------------------------------------------

(defn get-type-list [game type-func]
  (loop [field-list (vec (vals (game :field)))
         type-list []]
    (if (empty? field-list)
      type-list
      (let [field (first field-list)]
        (cond
          (type-func field) (recur (rest field-list) (conj type-list field))
          :else (recur (rest field-list) type-list))))))


(defn get-organ-list-for-owner
  "ermittelt alle Organe eines Spielers" 
  [game owner] 
   (get-type-list game #(= owner (% :owner))))

(defn get-organ-list-for-organism
  "ermittelt alle Organe, die zum gleichen Organismus gehören"
  [game field] 
   (let [root-id (field :organRootId)]
     (get-type-list game #(= root-id (% :organRootId)))))

(defn own? [field]
  (= 1 (field :owner)))

(defn enemy? [field]
  (= 0 (field :owner)))

(defn get-field
  ([game x y]
   (let [f ((game :field) [x y])]
     (if f f {:x x :y y :type :EMPTY :owner -1})))
  ([game x y key]
   ((get-field game x y) key)))

(defn get-root-cells [game player]
  (get-type-list game #(and (= (% :type) :ROOT)
                            (= player (% :owner)))))

(defn get-neighbour-fields 
  "Findet die Nachbar-Felder für eine gegebene Position"
  [game field-x field-y] 
  (let [dxdy [[-1 0] [1 0] [0 -1] [0 1]]]
    (for [[dx dy] dxdy
          :let [x    (+ field-x dx)
                y    (+ field-y dy)
                field (get-field game x y)]
          :when (and
                 (>= x 0) (< x (game :width))
                 (>= y 0) (< y (game :height)))]
      field)))

(defn grow-type [id x y type dir]
  (output (str "GROW" " " id " " x " " y " " (name type) " " dir)))

(defn spore [id x y]
  (output (str "SPORE" " " id " " x " " y)))

(defn wait []
  (output (str "WAIT")))

(defn min-element [vec-of-maps key]
  (reduce #(if (< (key %2) (key %1)) %2 %1) vec-of-maps))

(defn max-element [vec-of-maps key]
  (reduce #(if (> (key %2) (key %1)) %2 %1) vec-of-maps))

(defn field-empty? [game x y]
  (= nil ((game :field) [x y])))

; ----------------------------------------------------------------------------

(def middle-dot \u00B7)
(def bullet-operator \u2219)
(def interpunkt \u2E31)

(defn get-field-repr [field]
  (let [organ-name {:A "A", :B "B", :C "C", :D "D",
                    :BASIC "O", :HARVESTER "H", :SPORER "S", 
                    :TENTACLE "T", :ROOT "R", :WALL "#", :EMPTY interpunkt,
                    "E" ">", "W" "<", "N" "^", "S" "v"}]
    ; (debug (str "organ-name für " (name (field :type)) ": " (organ-name (field :type))))
    (if (own? field) 
      (organ-name (field :type))
      (str/lower-case (organ-name (field :type))))))

(defn format-protein-info [s width]
  (let [len (count s)]
    (cond
      (= len width) s
      (< len width) (str s (apply str (repeat (- width len) " ")))
      :else (subs s 0 width))))

(defn format-number [num width]
  (format (str "%" width "d") num))

(defn get-proteins-str [game]
  [
   (str "ME A:" (format-number (game :myA) 3) "|OP " (format-number (game :oppA) 3))
   (str "   B:" (format-number (game :myB) 3) "|   " (format-number (game :oppB) 3))
   (str "   C:" (format-number (game :myC) 3) "|   " (format-number (game :oppC) 3))
   (str "   D:" (format-number (game :myD) 3) "|   " (format-number (game :oppD) 3))])

(defn get-arena [game]
  (let [width (game :width)
        height (game :height)]
    (loop [y 0 arena (vec (map #(format-protein-info % width) (get-proteins-str game)))]
      (if (< y height)
        (let [row
              (loop [x 0 line ""]
                (if (< x width)
                  (let [f (get-field game x y)]
                    (recur (inc x) (str line (get-field-repr f))))
                  line))]
          (recur (inc y) (conj arena row)))
        arena))))

(defn combine-arenas [arenas] 
  (let [result 
        (loop [arenas arenas combined-arenas []]
          (if (empty? (first arenas))
            combined-arenas
            (let [first-line-arenas (map #(first %) arenas)
                  rest-arenas (map #(rest %) arenas)
                  line (str/join (interpose " " first-line-arenas))]
              (recur rest-arenas (conj combined-arenas line)))))]
    result))


(defn show-arenas [arenas & [debugging]]
  (let [arena-lines (combine-arenas arenas)
        print-fn (if debugging debug output)]
    (doseq [line arena-lines] 
      (print-fn line))))

(defn show-arena [game]
  (show-arenas [(get-arena game)]))

(defn debug-show-arena [game]
  (show-arenas [(get-arena game)] true))

(defn with-input-from-file [file-path f]
  (with-open [reader (io/reader file-path)]
    (binding [*in* reader]
      (f))))

(defn replay 
  "Liest einen Spielverlauf ein und gibt Spielfeld und Status-Informationen aus"
  ([filename n]
   (with-input-from-file filename
     (fn []
       (let [g (parse-game)]
         (loop [game (parse-turn g) collected-arenas [] k 0] 
           (let [collected-arenas 
                 (if (= (mod k n) 0) 
                   (do (show-arenas collected-arenas) (println) []) 
                   collected-arenas)
                 arena (get-arena game)
                 g2 (parse-turn g)]
             (if (nil? g2)
               (do ; restliche arenas ausgeben
                 (when (> (mod k n) 0) (show-arenas collected-arenas))
                 nil)
               (recur g2 (conj collected-arenas arena) (inc k)))))))))
  ([filename]
    (replay filename 1)))

(defn read-game-from-file []
  (with-input-from-file "test/resources/test-input-1.txt"
    (fn []
      (let [g (parse-game)
            game (parse-turn g)]
        game))))
; ----------------------------------------------------------------------------
; PriorityQueue 

(defn compare-distance [a b]
  (< (:distance a) (:distance b)))

; Erstelle einen Comparator aus der Vergleichsfunktion
(def distance-comparator (comparator compare-distance))

; Erstelle eine PriorityQueue mit dem benutzerdefinierten Comparator
(defn create-priority-queue [initial-size comparator]
  (PriorityQueue. initial-size comparator))

(defn neighbours? [a b]
  (or
   (and (= (a :x) (b :x)) (= (a :y) (inc (b :y))))
   (and (= (a :x) (b :x)) (= (a :y) (dec (b :y))))
   (and (= (a :y) (b :y)) (= (a :x) (inc (b :x))))
   (and (= (a :y) (b :y)) (= (a :x) (dec (b :x))))))

;; (defn enqueue [queue start-field field]
;;   (let [infinity 10000
;;         distance
;;         (cond
;;           (= (field :type) :EMPTY)
;;           (if (neighbours? start-field field)
;;             1
;;             infinity)

;;           (= (field :owner) -1)
;;           infinity

;;           (= (field :owner) ENEMY)
;;           infinity

;;       ; gehört das Startfeld zum gleichen Organismus wie das aktuelle Feld?
;;           (= (field :organRootId) (start-field :organRootId))
;;       ; ja, dann ist die Distanz 0
;;           0)]
;;     (.add queue {:field field :distance distance})))


(defn is-protein? [field]
  (or (= (field :type) :A)
      (= (field :type) :B)
      (= (field :type) :C)
      (= (field :type) :D)))

; ----------------------------------------------------------------------------
(defn get-expand-space 
  "ermittelt die freien Felder um einen Organismus herum mit Wurzel root"
  ([game root include-proteins] 
  (loop [organs (get-organ-list-for-organism game root) 
         free-spaces []]
    (let [organ (first organs)]
      (if (empty? organs)
        (do (debug (str "habe die folgenden freien Felder gefunden: "
                        (vec (map #(vec (take 2 %)) free-spaces)))) 
            (distinct free-spaces))

        (let [test-fields [[(inc (organ :x)) (organ :y) organ]
                           [(dec (organ :x)) (organ :y) organ]
                           [(organ :x) (inc (organ :y)) organ]
                           [(organ :x) (dec (organ :y)) organ]]]
          (recur (rest organs) 
             (concat free-spaces
                (filter #(let [ [x y] % ]
                               (and (>= x 0) (< x (game :width)) 
                                    (>= y 0) (< y (game :height)) 
                                    (or 
                                     (field-empty? game x y) 
                                     (and include-proteins 
                                          (is-protein? 
                                            (get-field game x y)))))) 
                        test-fields)))))))) 
    ([game root] 
       (get-expand-space game root false)))                
  
(def resources-map
  ; Here is a table to summarize all organ costs: 
  ; Organ      	A 	B 	C 	D
  ; BASIC 	    1 	0 	0 	0
  ; HARVESTER 	0 	0 	1 	1
  ; TENTACLE 	  0 	1 	1 	0
  ; SPORER 	    0 	1 	0 	1
  ; ROOT 	      1 	1 	1 	1
  
  {            ; A B C D
   :BASIC       [1,0,0,0]
   :HARVESTER   [0,0,1,1]
   :TENTACLE    [0,1,1,0]
   :SPORER      [0,1,0,1]
   :ROOT        [1,1,1,1]
 ; -----------------------
   :SPORE       [1,2,1,2]
   :ALOT        [7,2,3,3]})

(defn get-resources-for-action [action]
 (let [pkeys [:A :B :C :D]
       resources (resources-map action)]
   (reduce (fn [acc [pkey val]] 
             (assoc acc pkey val)) 
           {} 
           (map vector pkeys resources))))

(defn use-resources [game player action]
  (let [need-resources resources-map
        protein-names (if (= player ME) [:myA :myB :myC :myD]
                          [:oppA :oppB :oppC :oppD])]
    (loop [game game index 0]
      (if (= index 4)
        game
        (let [p-name (nth protein-names index)
              need   (nth (need-resources action) index)]
            (recur (assoc game p-name (- (game p-name) need))
                   (inc index)))))))

(defn has-resources? [game player action]
  (let [need-resources resources-map
        protein-names (if (= player ME) [:myA :myB :myC :myD]
                          [:oppA :oppB :oppC :oppD])]
    (every? true?
            (for [index (range 4)
                  :let [protein (nth protein-names index)
                        need (nth (need-resources action) index)]
                  :when (> need 0)]
              (>= (game protein) need)))))

(defn harvesting-resources? [action proteins]
  (let [need-resources resources-map
        protein-keys [:A :B :C :D]] 
    
    (every? true?
            (for [index (range 4)
                  :let [pkey (nth protein-keys index)
                        need (nth (need-resources action) index)]
                  :when (> need 0)]
              (>= (proteins pkey) need)))))


(defn get-direction [start target]
  (if (< (start :x) (target :x)) "E"
      (if (> (start :x) (target :x)) "W"
          (if (< (start :y) (target :y)) "S" "N"))))

(defn already-harvested? [game field]
  (let [orth-neighbours (get-neighbour-fields game (field :x) (field :y))]
    (some #(and (own? %) (= :HARVESTER (% :type))) orth-neighbours)))

(defn free-way? [game field dx dy distance]
  (let [target-x (+ (field :x) (* dx distance))
        target-y (+ (field :y) (* dy distance))]
    
    (if (and (>= target-x 0) (< target-x (game :width))
             (>= target-y 0) (< target-y (game :height)) 
             (field-empty? game target-x target-y))
      
      (loop [count 1 free-way true]
        (if (or (= count distance) (not free-way))
          free-way
          (let [x (+ (field :x) (* dx count))
                y (+ (field :y) (* dy count))
                f (get-field game x y)]
            (recur (inc count) (or (is-protein? f) (field-empty? game x y))))))
      false)))
      

(defn grow-random [game o-info]
  (let [root (o-info :root)
        grow-over-protein (if (o-info :grow-over-protein) true false)
        o-info (assoc o-info :grow-over-protein false)
        f (get-expand-space game root grow-over-protein)]
    (debug "in grow-random")
    (if (> (count f) 0)
      (let [n (rand-int (count f))
            field (nth f n)
            [x y o] field
            orth-neighbours (get-neighbour-fields game x y)
            prot (some identity
                       (filter
                        #(and (is-protein? %)
                              (not (already-harvested? game %)))
                        orth-neighbours))
            act-f (get-field game x y)
            no-walls-east  (if (free-way? game act-f  1  0 5) [(+ (act-f :x) 5) (act-f :y) "E"] nil)
            no-walls-west  (if (free-way? game act-f -1  0 5) [(- (act-f :x) 5) (act-f :y) "W"] nil)
            no-walls-north (if (free-way? game act-f  0 -1 5) [(act-f :x) (- (act-f :y) 5) "N"] nil)
            no-walls-south (if (free-way? game act-f  0  1 5) [(act-f :x) (+ (act-f :y) 5) "S"] nil)
            no-walls (some identity [no-walls-east no-walls-west no-walls-north no-walls-south])
            check-debug (debug (str "in grow-random, no-walls: " no-walls))]
        
        (cond
          ; habe ich genug Proteine für einen Harvester?
          (and (some? prot)
               (has-resources? game ME :HARVESTER))
          (do
            (grow-type (o :organId) x y :HARVESTER (get-direction (get-field game x y) prot))
            [(use-resources game ME :HARVESTER)
             (assoc o-info :actual [x y] :wait false)])

          (and (has-resources? game ME :SPORE) no-walls (nil? (o-info :sporer)))
          (let [[sx sy dir] no-walls]
            (debug (str "in grow-random, Sporer wird gesetzt, x:" x " y:" y " sx:" sx " sy:" sy " dir:" dir))
            (grow-type (o :organId) x y :SPORER dir)
            [(use-resources game ME :SPORER)
             (assoc o-info :actual [x y] :sporer [x y] :sporer-target [sx sy] :wait false)])

          (has-resources? game ME :ALOT)
          (do
            (grow-type (o :organId) x y :TENTACLE (get-direction (get-field game x y) (first (get-root-cells game ENEMY))))
            [(use-resources game ME :TENTACLE)
             (assoc o-info :actual [x y] :wait false)])

          (has-resources? game ME :BASIC)
          (do
            (grow-type (o :organId) x y :BASIC "E")
            [(use-resources game ME :BASIC)
             (assoc o-info :actual [x y] :wait false)])

          (has-resources? game ME :TENTACLE)
          (do
            (grow-type (o :organId) x y :TENTACLE (get-direction (get-field game x y) (first (get-root-cells game ENEMY))))
            [(use-resources game ME :TENTACLE)
             (assoc o-info :actual [x y] :wait false)])

          (has-resources? game ME :SPORER)
          (let [[sx sy dir] no-walls]
            (debug (str "in grow-random, Sporer wird gesetzt, x:" x " y:" y " sx:" sx " sy:" sy " dir:" dir))
            (grow-type (o :organId) x y :SPORER dir)
            [(use-resources game ME :SPORER)
             (assoc o-info :actual [x y] :sporer nil :sporer-target nil :wait false)])

          (has-resources? game ME :HARVESTER)
          (do
            (grow-type (o :organId) x y :HARVESTER (get-direction (get-field game x y) prot))
            [(use-resources game ME :HARVESTER)
             (assoc o-info :actual [x y] :wait false)])

          :else
          (do (wait)
              [game o-info])))
      (do
        (debug "keinen freien Platz zum wachsen gefunden!")
        (wait) 
        [game (assoc o-info :wait true)]))))

(defn grow-most-right [game o-info type dir]
  (let [root (o-info :root)
        f    (get-expand-space game root)]
    (debug "in grow-most-right")
    (if (> (count f) 0)
      (let [field (last (vec (sort-by first f)))
            [x y o] field]
        (grow-type (o :organId) x y type dir)
        (assoc o-info :actual [x y]))
      (do
        (debug "keinen freien Platz zum wachsen gefunden!")
        o-info))))

; (defn grow-right [game x y]
;  (debug "in grow-right")
;  (if (field-empty? game (inc x) y)
;    (do (grow-type ((get-field game x y) :organId) (inc x) y "BASIC" "E")
;        (debug (str "in grow-right, bin rechts gewachsen, x:" (inc x) " y:" y))
;        [(inc x) y])
;    nil))

(defn is-in [elem lst]
  (some #(= elem %) lst))

(defn shoot-sporer [game o-info]
  (debug "in shoot-sporer")
  (let [[x y] (o-info :sporer-target)
        [spx spy] (o-info :sporer)
        sporer (get-field game spx spy)
        sporer-id (sporer :organId)]
    (if sporer-id
      (do
        (spore (sporer :organId) x y)
        [(use-resources game ME :ROOT)
         (assoc o-info :sporer nil :sporer-target nil)])
      (do
        (debug "kein Sporer gefunden!")
        (wait)
        [game (assoc o-info :sporer nil :sporer-target nil)]))))

(defn random-strategy [game o-info]
  (let [[act-x act-y]  (o-info :actual)]
    (debug (str "in random-strategy x:" act-x " y:" act-y))
    (if (and (has-resources? game ME :ROOT) (o-info :sporer))
      (shoot-sporer game o-info)
      (grow-random game o-info))))

(defn is-harvested-protein?
  "Wird das Protein geerntet?  (von owner (default: ME))"
  ([game protein owner]
   (let [orth-neighbours (get-neighbour-fields game (protein :x) (protein :y))]
      ; gibt es einen Harvester, der neben dem Protein platziert ist
      ; und zum Protein ausgerichtet ist?
     (some #(when (and (= (% :type) :HARVESTER)
                       (= (get-direction % protein) (% :organDir))
                       (= (% :owner) owner)) %)
           orth-neighbours)))

  ([game protein]
   (is-harvested-protein? game protein ME)))

(defn get-harvested-proteins
  "Finde alle geernteten Proteine und zähle sie"
  ([game owner]
   (let [proteins (get-type-list game #(is-protein? %))
         harvested-proteins
         (filter some?
                 (map #(when (is-harvested-protein? game % owner) (% :type)) proteins))]
    ; reduce function(acc elem) start seq
     (println "harvested-proteins: " (str harvested-proteins))
     (reduce (fn [m p-type] (assoc m p-type (+ (get m p-type) 1)))
             {:A 0 :B 0 :C 0 :D 0} harvested-proteins)))
  ([game]
   (get-harvested-proteins game ME)))

;; (defn spawn-strategy [game o-info]
;;   (let [[act-x act-y]  (o-info :actual)]
;;     (debug (str "in spawn-stategy x:" act-x " y:" act-y))
;;     (if (and (has-resources? game ME :ROOT) (o-info :sporer))
;;       (shoot-sporer game o-info)
;;       (let [harvested-proteins (get-harvested-proteins game ME)]
;;         (if (every? #(>= % 1) (vals harvested-proteins))
;;           (grow-random game o-info)
;;           ; [start-nodes goal neighbours-fn cost-fn]
;;           ; kann ich harvester spawnen?
;;           (let [proteins-for-harvester (resources-map :HARVESTER)]
            
;;           (if (not (harvesting-resources? :HARVESTER harvested-proteins))
;;             ; Achtung: ich kann nicht beliebig Harvester spawnen
;;             ; Ich benötige C- und D-Proteine 
;;             (let [proteins (get-type-list game #(is-protein? %))
;;                   ; finde die nächten C- und D-Proteine
;;                   c-proteins (filter #(= (:type %) :C) proteins)
;;                   d-proteins (filter #(= (:type %) :D) proteins)
                  
                  
;;                   ; [start-nodes goal neighbours-fn cost-fn]
;;                   path (a-star
;;                         (get-organ-list-for-organism)
;;                         next-protein-field
;;                         (get-neighbour-func game)
;;                         (get-cost-func game))]
                   
;;             )))))

(defn add-elements [v elements]
  (let [vv (if (vector? v) v (vec v))]
    (if (seq? elements)
      (into vv elements)    ; Füge alle Elemente hinzu, wenn `elements` ein Vektor ist
      (conj vv elements)))) ; Füge ein einzelnes Element hinzu, wenn `elements` kein Vektor ist

(defn update-nth [lst n new-val-func]
  (let [vec-list (vec lst)] ; Konvertiere die Liste in einen Vektor
    (assoc vec-list n (new-val-func (nth vec-list n))))) ; Ändere das `n`-te Element

(defn iterate-actions [game o-info-list] 
  (loop [action-count 0 game game new-info-list []] 
     (if (= action-count (game :requiredActionsCount))
       (if (every? #(true? (% :wait)) new-info-list)
         (let [index (rand-int (count new-info-list))]
           (update-nth new-info-list index #(assoc % :grow-over-protein true))) 
         new-info-list)
       (do
         (debug (str "action-count:" action-count " requiredActionsCount:" (game :requiredActionsCount)))
         (let [[game new-info] (random-strategy game
                                          (nth o-info-list action-count))]
           (recur (inc action-count)
                  game
                  (add-elements new-info-list new-info)))))))

(defn contains-root-id? [v root-id]
  (some #(= ((% :root) :organId) root-id) v))

(defn create-organ-info [root]
  (hash-map :root root 
            :sporer nil :sporer-target nil 
            :actual [(root :x) (root :y)]))

(defn update-organ-info-list [my-root-cells o-info-list] 
  (vec (for [cell my-root-cells 
             :let [root-id (cell :organId) 
                   old-info (some #(when (= root-id (get-in % [:root :organId])) %) o-info-list)]] 
         (if old-info 
           old-info 
           (create-organ-info cell)))))

(defn -main [& args]
  (debug "Start")
  (let [g (parse-game)
        game (parse-turn g)]
        
    (loop [count 0 game game o-info-list []] 
      (debug-show-arena game)
      (let [my-root-cells (vec (get-root-cells game ME)) 
            new-o-info-list (update-organ-info-list my-root-cells o-info-list)

            debug-root-organs (do
                                (debug (str "turn: " count))
                                (debug (str "die gefundenen Root-Organe: " new-o-info-list)))
            info-list (iterate-actions game new-o-info-list)] 
        (debug (str count ": " info-list))
        (recur (inc count) (parse-turn g) info-list)))))

; ------------------------------------------------------------


