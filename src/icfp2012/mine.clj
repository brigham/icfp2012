(ns icfp2012.mine
  (require [clojure.java.io :as io]
           [icfp2012.index :as index]
           [icfp2012.water :as water])
  (import [java.io StringReader]))

(defprotocol AMine
  (move-left [mine])
  (move-right [mine])
  (move-up [mine])
  (move-down [mine])
  (wait-turn [mine])
  (abort [mine])
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
(declare set-char)

(defrecord Mine [grid state score extant-lambdas dead-lambdas indices water-sim]
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
    (->Mine (:grid mine)
            :abort
            (+ (:score mine) (* 25 (:dead-lambdas mine)))
            (:extant-lambdas mine)
            (:dead-lambdas mine)
            indices
            water-sim))
  (location [mine obj]
    (case obj
      (\R \L \O) (index/value-for indices obj)))
  (locations [mine obj]
    (case obj
      (\* \\) (index/set-for indices obj)))
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
    (let [[ax ay] (rep->actual grid x y)]
      (-> grid
          (nth ay)
          (nth ax))))
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
    (let [[grid state score extant-lambdas dead-lambdas indices]
          (let [{:keys [grid state score extant-lambdas dead-lambdas indices]} mine
                [robot-x robot-y] (location mine \R)
                new-robot-x (+ dirx robot-x)
                new-robot-y (+ diry robot-y)]
            (when (not (inside? mine new-robot-x new-robot-y))
              [grid state score extant-lambdas dead-lambdas indices])
            (case (object-at mine new-robot-x new-robot-y)
              (\space \.) [(set-chars grid     robot-x     robot-y \space
                                           new-robot-x new-robot-y \R)
                           state
                           score
                           extant-lambdas
                           dead-lambdas
                           (-> indices
                               (index/remove-from \R [robot-x robot-y])
                               (index/add-to \R [new-robot-x new-robot-y]))]
              
              \\ [(set-chars grid     robot-x     robot-y \space
                                  new-robot-x new-robot-y \R)
                  state
                  (+ score 25)
                  (dec extant-lambdas)
                  (inc dead-lambdas)
                  (-> indices
                      (index/remove-from \\ [new-robot-x new-robot-y])
                      (index/remove-from \R [robot-x robot-y])
                      (index/add-to \R [new-robot-x new-robot-y]))]
              
              \O [(set-chars grid     robot-x     robot-y \space
                                  new-robot-x new-robot-y \R)
                  :winning
                  (dec (+ (* 50 dead-lambdas) score))
                  extant-lambdas
                  dead-lambdas
                  (-> indices
                      (index/remove-from \R [robot-x robot-y])
                      (index/add-to \R [new-robot-x new-robot-y]))]
              
              \* (if (not= 0 dirx)
                   (let [new-rock-x (+ robot-x dirx dirx)]
                     (if (not= \space (object-at mine new-rock-x robot-y))
                       [grid state score extant-lambdas dead-lambdas indices]
                       [(set-chars grid     robot-x     robot-y \space
                                        new-robot-x new-robot-y \R
                                         new-rock-x    robot-y \*)
                        state score extant-lambdas dead-lambdas
                        (-> indices
                            (index/remove-from \* [new-robot-x new-robot-y])
                            (index/add-to \* [ new-rock-x     robot-y])
                            (index/remove-from \R [robot-x robot-y])
                            (index/add-to \R [new-robot-x new-robot-y]))]))
                   [grid state score extant-lambdas dead-lambdas indices])
              
              [grid state score extant-lambdas dead-lambdas indices]))]
      (->Mine grid state score extant-lambdas dead-lambdas indices (:water-sim mine)))))

(defn- map-update [mine]
  (if (done? mine)
    mine
    (let [{:keys [grid state score extant-lambdas dead-lambdas indices water-sim]} mine]
      (loop [x 1
             y 1
             [new-grid new-state] [grid state]]
        (if (not (inside? mine x y))
          (if (> y (count grid))
            (let [new-water-sim (water/simulate water-sim (second (location mine \R)))]
              (->Mine new-grid
                      (if (not (water/operating? new-water-sim)) :losing new-state)
                      (dec score)
                      extant-lambdas
                      dead-lambdas
                      indices
                      new-water-sim))
            (recur 1 (inc y) [new-grid new-state]))
          (recur
           (inc x) y
           (let [ch (object-at mine x y)]
             (case ch
               \* (cond
                   (= \space (object-at mine x (dec y)))
                   [(set-chars new-grid x      y  \space
                                        x (dec y) \*)
                    (if (and (inside? mine x (- y 2))
                             (= \R (object-at mine x (- y 2))))
                      :losing
                      new-state)]

                   (and (= \* (object-at mine x (dec y)))
                        (= \space (object-at mine (inc x) y))
                        (= \space (object-at mine (inc x) (dec y))))
                   [(set-chars new-grid      x       y  \space
                                        (inc x) (dec y) \*)
                    (if (and (inside? mine (inc x) (- y 2))
                             (= \R (object-at mine (inc x) (- y 2))))
                      :losing
                      new-state)]

                   (and (= \* (object-at mine x (dec y)))
                        (or (not= \space (object-at mine (inc x) y))
                            (not= \space (object-at mine (inc x) (dec y))))
                        (= \space (object-at mine (dec x) y))
                        (= \space (object-at mine (dec x) (dec y))))
                   [(set-chars new-grid      x       y  \space
                                        (dec x) (dec y) \*)
                    (if (and (inside? mine (dec x) (- y 2))
                             (= \R (object-at mine (dec x) (- y 2))))
                      :losing
                      new-state)]

                   (and (= \\ (object-at mine x (dec y)))
                        (= \space (object-at mine (inc x) y))
                        (= \space (object-at mine (inc x) (dec y))))
                   [(set-chars new-grid      x       y  \space
                                        (inc x) (dec y) \*)
                    (if (and (inside? mine (inc x) (- y 2))
                             (= \R (object-at mine (inc x) (- y 2))))
                      :losing
                      new-state)] 

                   :else
                   [new-grid new-state])
               
               \L (if (= 0 extant-lambdas)
                    [(set-chars new-grid x y \O) new-state]
                    [new-grid new-state])

               [new-grid new-state]))))))))

(defn possible-moves [mine]
  [\L \R \U \D \W])

(defn execute-move [mine ch]
  (let [ch (if (char? ch) ch (.charAt ch 0))]
   (case ch
     \L (move-left mine)
     \R (move-right mine)
     \U (move-up mine)
     \D (move-down mine)
     \W (wait-turn mine)
     \A (abort mine))))

(defn next-mines [mine]
  (if (done? mine)
    []
    (concat
     (let [original-robot (location mine \R)] ;; XXX: gross, should be on protocol
       (filter #(not= original-robot (location (second %) \R))
               (map #(vector % (execute-move mine %)) [\L \R \U \D])))
     (let [waiting (wait-turn mine)]
       (if (not= (:grid mine) (:grid waiting))
         [[\W waiting]]
         [])))))

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

(defn- compare-coords [c1 c2]
  (let [cmp-y (compare (c1 0) (c2 0))]
    (if (= cmp-y 0)
      (compare (c1 1) (c2 1))
      cmp-y)))

(defn mine-from-reader [r]
  (let [[grid max-length lambdas indices]
        (loop [grid []
               row []
               max-length 0
               lambdas 0
               indices (-> (index/create-index-group)
                           (index/create-index \R compare-coords)
                           (index/create-index \L compare-coords)
                           (index/create-index \* compare-coords)
                           (index/create-index \\ compare-coords))]
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
                  
                  (\# \. \space) (recur grid (conj row ch) max-length lambdas indices)

                  (\R \L) (recur grid
                                 (conj row ch)
                                 max-length
                                 lambdas
                                 (index/add-to indices ch [(inc (count row)) (count grid)]))

                  \* (recur grid
                            (conj row ch)
                            max-length
                            lambdas
                            (index/add-to indices ch [(inc (count row)) (count grid)]))
                  
                  \\ (recur grid
                            (conj row ch)
                            max-length
                            (inc lambdas)
                            (index/add-to indices ch [(inc (count row)) (count grid)])))))))

        [water flooding waterproof]
        (loop [water 0
               flooding 0
               waterproof 10]
          (let [line (.readLine r)]
            (if (nil? line)
              [water flooding waterproof]
              (let [[key value] (.split line "\\s+")]
                (case key
                  "Water" (recur (Integer/parseInt value) flooding waterproof)
                  "Flooding" (recur water (Integer/parseInt value) waterproof)
                  "Waterproof" (recur water flooding (Integer/parseInt value)))))))]
    
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
           (water/->WaterSim water flooding waterproof 0 0))))

(defn mine-from-thing [t]
  (mine-from-reader (io/reader t)))

(defn mine-from-string [s]
  (mine-from-reader (io/reader (StringReader. s))))
