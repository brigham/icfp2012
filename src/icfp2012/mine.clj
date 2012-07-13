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
(declare map-update)
(declare rep->actual)
(declare set-char)

(defrecord Mine [grid state score extant-lambdas dead-lambdas]
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
    (let [[ax ay] (rep->actual grid x y)
          updated (assoc grid ay (assoc (nth grid ay) ax ch))]
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
              new-robot-y (+ diry robot-y)]
          (when (not (inside? mine new-robot-x new-robot-y))
            [grid state score extant-lambdas dead-lambdas])
          (case (object-at mine new-robot-x new-robot-y)
            (\space \.) [(set-chars grid     robot-x     robot-y \space
                                         new-robot-x new-robot-y \R)
                         state
                         score
                         extant-lambdas
                         dead-lambdas]
            \\ [(set-chars grid     robot-x     robot-y \space
                                new-robot-x new-robot-y \R)
                state
                (+ score 25)
                (dec extant-lambdas)
                (inc dead-lambdas)]
            \O [(set-chars grid     robot-x     robot-y \space
                                new-robot-x new-robot-y \R)
                :winning
                (+ (* 50 dead-lambdas) score)
                extant-lambdas
                dead-lambdas]
            \* (if (not= 0 dirx)
                 (let [new-rock-x (+ robot-x dirx dirx)]
                   (if (not= \space (object-at mine new-rock-x robot-y))
                     [grid state score extant-lambdas dead-lambdas]
                     [(set-chars grid     robot-x     robot-y \space
                                      new-robot-x new-robot-y \R
                                       new-rock-x    robot-y \*)
                      state score extant-lambdas dead-lambdas]))
                 [grid state score extant-lambdas dead-lambdas])
            [grid state score extant-lambdas dead-lambdas]))]
    (->Mine grid state (dec score) extant-lambdas dead-lambdas)))

(defn- map-update [mine]
  (let [{:keys [grid state score extant-lambdas dead-lambdas]} mine]
    (loop [x 1
           y 1
           [new-grid new-state] [grid state]]
      (if (not (inside? mine x y))
        (if (> y (count grid))
          (->Mine new-grid new-state score extant-lambdas dead-lambdas)
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

             [new-grid new-state])))))))

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
                [(conj grid row) (max max-length (count row)) lambdas] 
                [grid max-length lambdas])
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

