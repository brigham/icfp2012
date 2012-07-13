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
  (score [mine])
  (dimensions [mine])
  (object-at [mine x y])
  (->String [mine]))

(declare find-char)

(defrecord Mine [grid state score extant-lambdas dead-lambdas]
  AMine
  (move-left [mine]
    (let [[grid state score extant-lambdas dead-lambdas]
          (let [[rx ry] (find-char grid \R)
                ax (dec rx)
                ay (- (count grid) ry)]
            (when (= rx 1)
              [grid state score extant-lambdas dead-lambdas])
            (case (object-at mine (dec rx) ry)
              (\space \.) [(assoc grid ay (assoc (nth grid ay) ax \space (dec ax) \R))
                           state
                           score
                           extant-lambdas
                           dead-lambdas]
              \\ [(assoc grid ay (assoc (nth grid ay) ax \space (dec ax) \R))
                  state
                  (+ score 25)
                  (dec extant-lambdas)
                  (inc dead-lambdas)] ;; check for last lambda
              \O [(assoc grid ay (assoc (nth grid ay) ax \space (dec ax) \R))
                  :winning
                  (+ (* 50 dead-lambdas) score)
                  extant-lambdas
                  dead-lambdas]
              \* (let [new-rock-x (- rx 2)]
                   (if (not= \space (object-at mine new-rock-x ry))
                     [grid state score extant-lambdas dead-lambdas]
                     [(assoc grid ay (assoc (nth grid ay)
                                       ax \space
                                       (dec ax) \R
                                       (dec new-rock-x) \*))
                      state score extant-lambdas dead-lambdas]))
              [grid state score extant-lambdas dead-lambdas]))]
      (->Mine grid state (dec score) extant-lambdas dead-lambdas)))
  (move-right [mine]
    mine)
  (move-up [mine]
    mine)
  (move-down [mine]
    mine)
  (wait-turn [mine]
    mine)
  (abort [mine]
    mine)
  (done? [mine]
    (not= :running state))
  (score [mine]
    score)
  (dimensions [mine]
    [(count (nth grid 0)) (count grid)])
  (object-at [mine x y]
    (-> grid
        (nth (- (count grid) y))
        (nth (dec x))))
  (->String [mine]
    (apply str (interpose "\n" (map #(apply str %) grid)))))

(defn- find-char [grid ch]
  (loop [[row & rest-rows] grid
         row-num (count grid)]
    (if row
      (let [index (.indexOf row ch)]
        (if (not= -1 index)
          [(inc index) row-num]
          (recur rest-rows (dec row-num))))
      nil)))

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

