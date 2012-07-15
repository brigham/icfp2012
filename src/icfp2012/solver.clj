(ns icfp2012.solver
  (require [icfp2012.mine :as mine]
           [hanoi.a-star :as a*]))

(defn- compare-mines [a b]
  (compare (get-in a [:water-sim :steps]) (get-in b [:water-sim :steps])))

(defn- possible-mines [mine]
  (map (partial mine/execute-move mine) (mine/possible-moves mine)))

(defn- possible-mines-for-a* [mine]
  (map (fn [[move next-mine]] {:cost 1
                               :state next-mine
                               :move move})
       (mine/next-mines mine)))

(defn- city-dist [a b]
  (+ (Math/abs (- (a 0) (b 0)))
     (Math/abs (- (a 0) (b 0)))))

(defn- mine-heuristic [mine]
  (let [lambdas (mine/locations mine \\)
        r (mine/location mine \R)]
    (cond (= 0 (count lambdas))
          (city-dist r (mine/location mine \O))

          (= 1 (count lambdas))
          (let [f (first lambdas)]
            (+ (city-dist r f)
               (city-dist f (mine/location mine \L))))

          :else
          (let [f (first lambdas)
                l (last lambdas)]
            (+ (city-dist r f)
               (city-dist f l)
               (city-dist l (mine/location mine \L)))))))

(defn- winning-mine? [mine]
  (= :winning (mine/state mine)))

(defn search-a*
  ([mine]
     (search-a* mine 500))
  ([mine limit]
     (a*/a* mine
            compare-mines
            possible-mines-for-a*
            mine-heuristic
            winning-mine?
            limit)))

(defn search
  ([mine]
     (search mine -1))
  ([mine limit]
     (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [mine []])
            [best-mine best-moves :as best] [mine []]
            remaining limit
            max-moves 0]
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
                    (dec remaining)
                    (let [turns (get-in test-mine [:water-sim :steps])]
                      (if (> turns max-moves)
                        (do
                          (println turns "(" remaining ")")
                          turns)
                        max-moves)))))))))
