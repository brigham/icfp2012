(ns icfp2012.solver
  (require [icfp2012.mine :as mine]
           [hanoi.a-star :as a*]))

(defn- compare-mines [a b]
  (compare (mine/score a) (mine/score b)))

(defn- possible-mines [mine]
  (map (partial mine/execute-move mine) (mine/possible-moves mine)))

(defn- possible-mines-for-a* [mine]  (map (fn [move] (let [next-mine (mine/execute-move mine move)]
                    {:cost 0; (- (mine/score mine) (mine/score next-mine))
                     :state next-mine
                     :move move}))
       (mine/possible-moves mine)))

(defn- mine-heuristic [mine]
  0)

(defn- winning-mine? [mine]
  (= :winning (mine/state mine)))

(defn search-a* [mine]
  (a*/a* mine
         compare-mines
         possible-mines
         mine-heuristic
         winning-mine?
         500))

(defn search
  ([mine]
     (search mine -1))
  ([mine limit]
     (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [mine []])
            [best-mine best-moves :as best] [mine []]
            remaining limit]
       (if (or (empty? queue) (= 0 remaining))
         best
         (let [[test-mine moves :as element] (peek queue)]
           (if (= :winning (mine/state test-mine))
             element
             (recur (let [next-moves (map (fn [[move new-mine]]
                                            [new-mine (conj moves move)]) (mine/next-mines test-mine))]
                      (if (seq next-moves)
                        (apply conj (pop queue) next-moves)
                        (pop queue)))
                    (if (> (mine/score test-mine) (mine/score best-mine)) element best)
                    (dec remaining))))))))
