(ns icfp2012.mine
  (require [clojure.java.io :as io])
  (import [java.io StringReader]))

(defprotocol AMine
  (move-left [mine])
  (move-right [mine])
  (move-up [mine])
  (move-down [mine])
  (wait-turn [mine])
  (abort [mine])
  (done? [mine])
  (inside? [mine x y])
  (score [mine])
  (dimensions [mine])
  (object-at [mine x y])
  (->String [mine]))

(declare find-char)
(declare move-robot)
(declare rep->actual)
(declare set-char)

(defrecord Mine [grid state score extant-lambdas dead-lambdas]
  AMine
  (move-left [mine]
    (move-robot mine -1 0))
  (move-right [mine]
    (move-robot mine 1 0))
  (move-up [mine]
    (move-robot mine 0 1))
  (move-down [mine]
    (move-robot mine 0 -1))
  (wait-turn [mine]
    mine)
  (abort [mine]
    mine)
  (done? [mine]
    (not= :running state))
  (inside? [mine x y]
    (and (>= x 1) (>= y 1) (<= x (count (nth grid 0))) (<= y (count grid))))
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
    (apply str (interpose "\n" (map #(apply str %) grid)))))

(defn- rep->actual [grid rx ry]
  [(dec rx) (- (count grid) ry)])

(defn- set-chars [grid & coords-chars]
  (loop [grid grid
         [x y ch & rest-coords-chars] coords-chars]
    (let [updated (assoc grid y (assoc (nth grid y) x ch))]
      (if rest-coords-chars
        (recur updated rest-coords-chars)
        updated))))

(defn- find-char [grid ch]
  (loop [[row & rest-rows] grid
         row-num (count grid)]
    (if row
      (let [index (.indexOf row ch)]
        (if (not= -1 index)
          [(inc index) row-num]
          (recur rest-rows (dec row-num))))
      nil)))

(defn- move-robot [mine dirx diry]
  (let [[grid state score extant-lambdas dead-lambdas]
        (let [{:keys [grid state score extant-lambdas dead-lambdas]} mine
              [robot-x robot-y] (find-char grid \R)
              new-robot-x (+ dirx robot-x)
              new-robot-y (+ diry robot-y)
              [actual-x actual-y] (rep->actual grid robot-x robot-y)
              [new-actual-x new-actual-y] (rep->actual grid new-robot-x new-robot-y)]
          (when (not (inside? mine new-robot-x new-robot-y))
            [grid state score extant-lambdas dead-lambdas])
          (case (object-at mine new-robot-x new-robot-y)
            (\space \.) [(set-chars grid     actual-x     actual-y \space
                                         new-actual-x new-actual-y \R)
                         state
                         score
                         extant-lambdas
                         dead-lambdas]
            \\ [(set-chars grid     actual-x     actual-y \space
                                new-actual-x new-actual-y \R)
                state
                (+ score 25)
                (dec extant-lambdas)
                (inc dead-lambdas)] ;; check for last lambda
            \O [(set-chars grid     actual-x     actual-y \space
                                new-actual-x new-actual-y \R)
                :winning
                (+ (* 50 dead-lambdas) score)
                extant-lambdas
                dead-lambdas]
            \* (if (not= 0 dirx)
                 (let [new-rock-x (+ robot-x dirx dirx)]
                   (if (not= \space (object-at mine new-rock-x robot-y))
                     [grid state score extant-lambdas dead-lambdas]
                     [(set-chars grid     actual-x     actual-y \space
                                      new-actual-x new-actual-y \R
                                        new-rock-x     actual-y \*)
                      state score extant-lambdas dead-lambdas]))
                 [grid state score extant-lambdas dead-lambdas])
            [grid state score extant-lambdas dead-lambdas]))]
    (->Mine grid state (dec score) extant-lambdas dead-lambdas)))

(defn execute-move [mine ch]
  (let [ch (if (char? ch) ch (.charAt ch 0))]
   (case ch
     \L (move-left mine)
     \R (move-right mine)
     \U (move-up mine)
     \D (move-down mine)
     \W (wait-turn mine)
     \A (abort mine))))

(defn mine-from-reader [r]
  (let [[grid max-length lambdas]
        (loop [grid []
               row []
               max-length 0
               lambdas 0]
          (let [next (.read r)]
            (if (= -1 next)
              (if (> (count row) 0)
                [(conj grid row) (max max-length (count row))] 
                [grid max-length])
              (let [ch (char next)]
                (case ch
                  \newline (recur (conj grid row) [] (max max-length (count row)) lambdas)
                  (\R \# \* \L \O \. \space) (recur grid (conj row ch) max-length lambdas)
                  \\ (recur grid (conj row ch) max-length (inc lambdas)))))))]
    (->Mine (vec (map #(let [deficit (- max-length (count %))]
                        (if (> deficit 0)
                          (apply conj % (repeat deficit \space))
                          %)) grid))
           :running
           0
           lambdas
           0)))

(defn mine-from-thing [t]
  (mine-from-reader (io/reader t)))

(defn mine-from-string [s]
  (mine-from-reader (StringReader. s)))

