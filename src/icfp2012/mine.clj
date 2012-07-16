(ns icfp2012.mine
  (require [clojure.java.io :as io]
           [icfp2012.index :as index]
           [icfp2012.trampoline :as tramps]
           [icfp2012.water :as water])
  (import [java.io StringReader]))

(set! *warn-on-reflection* true)

(defn print-return
  ([o]
     (println o)
     o)
  ([msg o]
     (println msg o)
     o))

(defn- compare-coords [c1 c2]
  (let [cmp-y (compare (c1 0) (c2 0))]
    (if (= cmp-y 0)
      (compare (c1 1) (c2 1))
      cmp-y)))

(defprotocol AMine
  (move-left [mine])
  (move-right [mine])
  (move-up [mine])
  (move-down [mine])
  (wait-turn [mine])
  (abort [mine])
  (shave [mine])
  (location [mine obj])
  (locations [mine obj])
  (done? [mine])
  (inside? [mine x y])
  (score [mine])
  (state [mine])
  (dimensions [mine])
  (object-at [mine x y])
  (->String [mine]))

(declare ->Mine)
(declare move-robot)
(declare map-update)
(declare rep->actual)
(declare set-chars)

(defrecord Mine [grid state score extant-lambdas dead-lambdas indices water-sim tramps beard-rate beard-remaining razors]
  AMine
  (move-left [mine]
    (map-update (move-robot mine -1 0)))
  (move-right [mine]
    (map-update (move-robot mine 1 0)))
  (move-up [mine]
    (map-update (move-robot mine 0 1)))
  (move-down [mine]
    (map-update (move-robot mine 0 -1)))
  (wait-turn [mine]
    (map-update mine))
  (abort [mine]
    (assoc mine
      :state :abort
      :score (+ (:score mine) (* 25 dead-lambdas))))
  (shave [mine]
    (map-update
     (if (<= razors 0)
       mine
       (let [[x y] (location mine \R)
             shave (fn [[new-grid new-indices] cx cy]
                     (if (= \W (object-at mine cx cy))
                       [(set-chars new-grid cx cy \space)
                        (index/remove-from new-indices \W [cx cy])]
                       [new-grid new-indices]))
             [new-grid new-indices] (-> [grid indices]
                                        (shave (dec x) (dec y))
                                        (shave x (dec y))
                                        (shave (inc x) (dec y))
                                        (shave (dec x) y)
                                        (shave (inc x) y)
                                        (shave (dec x) (inc y))
                                        (shave x (inc y))
                                        (shave (inc x) (inc y)))]
         (assoc mine
           :grid new-grid
           :indices new-indices)))))
  (location [mine obj]
    (case obj
      (\R \L \O) (index/value-for indices obj)))
  (locations [mine obj]
    (case obj
      (\* \\ \A \1 \W) (seq (index/set-for indices obj))))
  (done? [mine]
    (not= :running state))
  (inside? [mine x y]
    (and (>= x 1) (>= y 1) (<= x (count (nth grid 0))) (<= y (count grid))))
  (state [mine]
    state)
  (score [mine]
    score)
  (dimensions [mine]
    [(count (nth grid 0)) (count grid)])
  (object-at [mine x y]
    (if (inside? mine x y)
      (let [[ax ay] (rep->actual grid x y)]
        (-> grid
            (nth ay)
            (nth ax)))
      \#))
  (->String [mine]
    (let [rows (map #(apply str %) grid)
          water-level (get-in mine [:water-sim :water-level])
          above-water (take (- (count grid) water-level) rows)
          under-water (drop (- (count grid) water-level) rows)]
      (str
       (char 27) "[0m"
       (apply str (interpose "\n" above-water))
       (if (> (count under-water) 0)
         (str
          (char 27) "\n[44m"
          (apply str (interpose "\n" under-water))
          (char 27) "[0m")
         "")))))

(defn- rep->actual [grid rx ry]
  [(dec rx) (- (count grid) ry)])

(defn- set-chars [grid & coords-chars]
  (loop [grid grid
         [x y ch & rest-coords-chars] coords-chars]
    (let [[ax ay] (rep->actual grid x y)
          updated (assoc grid ay (assoc (nth grid ay) ax ch))]
      (if rest-coords-chars
        (recur updated rest-coords-chars)
        updated))))

(defn- move-robot [mine dirx diry]
  (if (done? mine)
    mine
    (let [new-keys
          (let [{:keys [grid state score extant-lambdas dead-lambdas indices tramps razors]} mine
                [robot-x robot-y] (location mine \R)
                new-robot-x (+ dirx robot-x)
                new-robot-y (+ diry robot-y)]
            (when (not (inside? mine new-robot-x new-robot-y))
              {})
            (let [ch (object-at mine new-robot-x new-robot-y)]
              (case ch
                (\! \space \.) {:grid
                                (set-chars grid     robot-x     robot-y \space
                                           new-robot-x new-robot-y \R),
                                :indices
                                (-> indices
                                    (index/remove-from \R [robot-x robot-y])
                                    (index/add-to \R [new-robot-x new-robot-y]))
                                :razors (if (= \! ch) (inc razors) razors)}
                
                \\ {:grid (set-chars grid     robot-x     robot-y \space
                                     new-robot-x new-robot-y \R),
                    :score (+ score 25)
                    :extant-lambdas (dec extant-lambdas)
                    :dead-lambdas (inc dead-lambdas)
                    :indices (-> indices
                                 (index/remove-from \\ [new-robot-x new-robot-y])
                                 (index/remove-from \R [robot-x robot-y])
                                 (index/add-to \R [new-robot-x new-robot-y]))}
                
                \O {:grid (set-chars grid     robot-x     robot-y \space
                                     new-robot-x new-robot-y \R)
                    :state :winning
                    :score (dec (+ (* 50 dead-lambdas) score))
                    :indices (-> indices
                                 (index/remove-from \R [robot-x robot-y])
                                 (index/add-to \R [new-robot-x new-robot-y]))}
                
                (\* \@) (if (not= 0 dirx)
                          (let [new-rock-x (+ robot-x dirx dirx)]
                            (if (not= \space (object-at mine new-rock-x robot-y))
                              {}
                              {:grid (set-chars grid     robot-x     robot-y \space
                                                     new-robot-x new-robot-y \R
                                                      new-rock-x     robot-y ch)
                               :indices (-> indices
                                            (index/remove-from \* [new-robot-x new-robot-y])
                                            (index/add-to \* [ new-rock-x     robot-y])
                                            (index/remove-from \R [robot-x robot-y])
                                            (index/add-to \R [new-robot-x new-robot-y]))}))
                          {})

                (\A \B \C \D \E \F \G \H \I) (let [destination (tramps/destination tramps (str ch))
                                                   all-sources (tramps/sources tramps destination)
                                                   dest-location (first (filter #(= destination (% 2))
                                                                                (locations mine \1)))
                                                   source-locations (filter #(all-sources (% 2))
                                                                            (locations mine \A))]
                                               {:grid (apply set-chars grid
                                                             robot-x robot-y \space
                                                             (dest-location 0) (dest-location 1) \R
                                                             (flatten (map (fn [[x y c]] [x y \space])
                                                                           source-locations)))
                                                :indices (-> indices
                                                             (index/remove-from \R [robot-x robot-y])
                                                             (index/add-to \R [(dest-location 0) (dest-location 1)])
                                                             (index/remove-from \1 dest-location)
                                                             (index/remove-all \A source-locations))
                                                :tramps (tramps/activate tramps ch)})
                
                {})))]
      (into mine new-keys))))

(defn- map-update [mine]
  (if (done? mine)
    mine
    (let [{:keys [grid state score extant-lambdas dead-lambdas indices water-sim beard-rate beard-remaining]} mine
          rocks-moved (loop [[rock & rest-rocks :as all-rocks] (locations mine \*)
                             [beard & rest-beards :as all-beards] (if (zero? beard-remaining) (locations mine \W) [])
                             [new-grid new-state new-indices] [grid state indices]]
                        (if (and (nil? rock) (nil? beard))
                          (let [new-water-sim (water/simulate water-sim (second (location mine \R)))
                                [new-grid new-indices] (if (> extant-lambdas 0)
                                                         [new-grid new-indices]
                                                         (if-let [closed-lift (location mine \L)]
                                                           [(set-chars new-grid (closed-lift 0) (closed-lift 1) \O)
                                                            (-> new-indices
                                                                (index/remove-from \L closed-lift)
                                                                (index/add-to \O closed-lift))]
                                                           [new-grid new-indices]))]
                            (assoc mine
                              :grid new-grid
                              :state (if (not (water/operating? new-water-sim)) :losing new-state)
                              :score (dec score)
                              :indices new-indices
                              :water-sim new-water-sim
                              :beard-remaining (mod (dec beard-remaining) beard-rate)))
                          (if (or (nil? beard)
                                  (and (not (nil? rock)) (< (compare-coords rock beard) 0)))
                            (recur rest-rocks
                                   all-beards
                                   (let [[x y] rock
                                         rock-type (object-at mine x y)]
                                     (cond
                                      ;; Rocks fall straight down
                                      (= \space (object-at mine x (dec y)))
                                      (let [horock-break (and (= \@ rock-type)
                                                              (not= \space (object-at mine x (- y 2))))]
                                        [(set-chars new-grid x      y  \space
                                                    x (dec y) (if horock-break \\ rock-type))
                                         (if (= \R (object-at mine x (- y 2)))
                                           :losing
                                           new-state)
                                         (let [rock-moved
                                               (-> new-indices
                                                   (index/remove-from \* [x y])
                                                   (index/add-to \* [x (dec y)]))]
                                           (if horock-break
                                             (-> rock-moved
                                                 (index/remove-from \* [x (dec y)])
                                                 (index/add-to \\ [x (dec y)]))
                                             rock-moved))])

                                      ;; Rocks on top of rocks fall to the right
                                      (and (#{\* \@} (object-at mine x (dec y)))
                                           (= \space (object-at mine (inc x) y))
                                           (= \space (object-at mine (inc x) (dec y))))
                                      (let [horock-break (and (= \@ rock-type)
                                                              (not= \space (object-at mine (inc x) (- y 2))))]
                                        [(set-chars new-grid      x       y  \space
                                                    (inc x) (dec y) (if horock-break \\ rock-type))
                                         (if (= \R (object-at mine (inc x) (- y 2)))
                                           :losing
                                           new-state)
                                         (let [rock-moved
                                               (-> new-indices
                                                   (index/remove-from \* [x y])
                                                   (index/add-to \* [(inc x) (dec y)]))]
                                           (if horock-break
                                             (-> rock-moved
                                                 (index/remove-from \* [x (dec y)])
                                                 (index/add-to \\ [(inc x) (dec y)]))
                                             rock-moved))])

                                      ;; Rocks on top of rocks fall to the left if right blocked
                                      (and (#{\* \@} (object-at mine x (dec y)))
                                           (or (not= \space (object-at mine (inc x) y))
                                               (not= \space (object-at mine (inc x) (dec y))))
                                           (= \space (object-at mine (dec x) y))
                                           (= \space (object-at mine (dec x) (dec y))))
                                      (let [horock-break (and (= \@ rock-type)
                                                              (not= \space (object-at mine (dec x) (- y 2))))]
                                        [(set-chars new-grid      x       y  \space
                                                    (dec x) (dec y) (if horock-break \\ rock-type))
                                         (if (= \R (object-at mine (dec x) (- y 2)))
                                           :losing
                                           new-state)
                                         (let [rock-moved
                                               (-> new-indices
                                                   (index/remove-from \* [x y])
                                                   (index/add-to \* [(dec x) (dec y)]))]
                                           (if horock-break
                                             (-> rock-moved
                                                 (index/remove-from \* [x (dec y)])
                                                 (index/add-to \\ [(dec x) (dec y)]))
                                             rock-moved))])

                                      ;; Rocks on top of lambdas fall to the right
                                      (and (= \\ (object-at mine x (dec y)))
                                           (= \space (object-at mine (inc x) y))
                                           (= \space (object-at mine (inc x) (dec y))))
                                      (let [horock-break (and (= \@ rock-type)
                                                              (not= \space (object-at mine (inc x) (- y 2))))]
                                        [(set-chars new-grid      x       y  \space
                                                    (inc x) (dec y) (if horock-break \\ rock-type))
                                         (if (= \R (object-at mine (inc x) (- y 2)))
                                           :losing
                                           new-state)
                                         (let [rock-moved
                                               (-> new-indices
                                                   (index/remove-from \* [x y])
                                                   (index/add-to \* [(inc x) (dec y)]))]
                                           (if horock-break
                                             (-> rock-moved
                                                 (index/remove-from \* [x (dec y)])
                                                 (index/add-to \\ [(inc x) (dec y)]))
                                             rock-moved))]) 

                                      :else
                                      [new-grid new-state new-indices])))
                            (recur all-rocks
                                   rest-beards
                                   (let [[x y] beard
                                         grow-beard (fn [[new-grid new-indices] cx cy]
                                                      (if (= \space (object-at mine cx cy))
                                                        [(set-chars new-grid cx cy \W)
                                                         (index/add-to new-indices \W [cx cy])]
                                                        [new-grid new-indices]))
                                         [new-grid new-indices]
                                         (-> [new-grid new-indices]
                                             (grow-beard (dec x) (dec y))
                                             (grow-beard x (dec y))
                                             (grow-beard (inc x) (dec y))
                                             (grow-beard (dec x) y)
                                             (grow-beard (inc x) y)
                                             (grow-beard (dec x) (inc y))
                                             (grow-beard x (inc y))
                                             (grow-beard (inc x) (inc y)))]
                                     [new-grid new-state new-indices])))))]
      rocks-moved)))

(defn possible-moves [mine]
  [\L \R \U \D \W])

(defn execute-move [mine ^Character ch]
  (case ch
    \L (move-left mine)
    \R (move-right mine)
    \U (move-up mine)
    \D (move-down mine)
    \W (wait-turn mine)
    \A (abort mine)
    \S (shave mine)))

(defn next-mines [mine]
  (if (done? mine)
    []
    (concat
     (let [original-robot (location mine \R)] 
       (filter #(not= original-robot (location (second %) \R))
               (map #(vector % (execute-move mine %)) [\L \R \U \D])))
     (let [waiting (wait-turn mine)]
       (when (not= (:grid mine) (:grid waiting))
         [[\W waiting]]))
     (if (and (> (count (locations mine \W)) 0)
              (> (:razors mine) 0))
       (let [[rx ry] (location mine \R)]
         (when (conj #{}
              (object-at mine (dec rx) (dec ry))
              (object-at mine rx (dec ry))
              (object-at mine (inc rx) (dec ry))
              (object-at mine (dec rx) ry)
              (object-at mine (inc rx) ry)
              (object-at mine (dec rx) (inc ry))
              (object-at mine rx (inc ry))
              (object-at mine (inc rx) (inc ry)) \W)
           [[\S (shave mine)]])))
     [[\A (abort mine)]])))

(defn execute-moves
  ([mine s]
     (execute-moves mine s (fn [mine])))
  ([mine s cb]
     (cb mine)
     (loop [i 0
            curr-mine mine]
       (if (or (>= i (count s))
               (>= i (apply * (dimensions mine)))
               (done? curr-mine))
         curr-mine
         (let [new-mine (execute-move curr-mine (get s i))]
           (cb new-mine)
           (recur (inc i) new-mine))))))

(defn mine-from-reader [^java.io.BufferedReader r]
  (let [[grid max-length lambdas indices]
        (loop [grid []
               row []
               max-length 0
               lambdas 0
               indices (-> (index/create-index-group)
                           (index/create-index \R compare-coords)
                           (index/create-index \L compare-coords)
                           (index/create-index \* compare-coords)
                           (index/create-index \\ compare-coords)
                           (index/create-index \A compare-coords)
                           (index/create-index \1 compare-coords)
                           (index/create-index \W compare-coords))]
          (let [next (.read r)]
            (if (= -1 next)
              (if (> (count row) 0)
                [(conj grid row) (max max-length (count row)) lambdas indices] 
                [grid max-length lambdas indices])
              (let [ch (char next)]
                (case ch
                  \newline (if (or (> (count row) 0) (= (count grid) 0))
                             (recur (conj grid row) [] (max max-length (count row)) lambdas indices)
                             [grid max-length lambdas indices])
                  
                  (\# \. \space \!) (recur grid (conj row ch) max-length lambdas indices)

                  (\R \L) (recur grid
                                 (conj row ch)
                                 max-length
                                 lambdas
                                 (index/add-to indices ch [(inc (count row)) (count grid)]))

                  (\* \@) (recur grid
                                 (conj row ch)
                                 max-length
                                 (if (= ch \@) (inc lambdas) lambdas)
                                 (index/add-to indices \* [(inc (count row)) (count grid)]))
                  
                  \\ (recur grid
                            (conj row ch)
                            max-length
                            (inc lambdas)
                            (index/add-to indices \\ [(inc (count row)) (count grid)]))

                  \W (recur grid
                            (conj row ch)
                            max-length
                            lambdas
                            (index/add-to indices \W [(inc (count row)) (count grid)]))

                  (\A \B \C \D \E \F \G \H \I) (recur grid
                                                      (conj row ch)
                                                      max-length
                                                      lambdas
                                                      (index/add-to indices \A [(inc (count row)) (count grid) (str ch)]))

                  (\1 \2 \3 \4 \5 \6 \7 \8 \9) (recur grid
                                                      (conj row ch)
                                                      max-length
                                                      lambdas
                                                      (index/add-to indices \1 [(inc (count row)) (count grid) (str ch)])))))))

        {:keys [water flooding waterproof trampolines growth razors]}
        (loop [whole-map {:water 0
                          :flooding 0
                          :waterproof 10
                          :trampolines {}
                          :growth 25
                          :razors 0}]
          (let [^String line (.readLine r)]
            (if (nil? line)
              whole-map
              (let [[^String key ^String value] (.split line "\\s+" 2)]
                (case key
                  "Water" (recur (assoc whole-map :water (Integer/parseInt value)))
                  "Flooding" (recur (assoc whole-map :flooding (Integer/parseInt value)))
                  "Waterproof" (recur (assoc whole-map :waterproof (Integer/parseInt value)))
                  "Growth" (recur (assoc whole-map :growth (Integer/parseInt value)))
                  "Razors" (recur (assoc whole-map :razors (Integer/parseInt value)))
                  "Trampoline" (let [[source dest] (.split value "\\s+targets\\s+")]
                                 (recur (update-in whole-map [:trampolines] #(assoc % source dest)))))))))]
    
    (->Mine (vec (map #(let [deficit (- max-length (count %))]
                        (if (> deficit 0)
                          (apply conj % (repeat deficit \space))
                          %)) grid))
           :running
           0
           lambdas
           0
           (loop [[key & rest-keys] (index/index-keys indices)
                  updated-indices indices]
             (if (not key)
               updated-indices
               (let [values (index/set-for updated-indices key)
                     updated-values (map (fn [c] (update-in c [1] #(- (count grid) %))) values)]
                 (recur rest-keys (index/add-all (index/clear updated-indices key) key updated-values)))))
           (water/->WaterSim water flooding waterproof 0 0)
           (tramps/create-trampoline-system trampolines)
           growth
           (dec growth)
           razors)))

(defn mine-from-thing [t]
  (mine-from-reader (io/reader t)))

(defn mine-from-string [s]
  (mine-from-reader (io/reader (StringReader. s))))
