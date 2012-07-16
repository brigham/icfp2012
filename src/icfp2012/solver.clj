(ns icfp2012.solver
  (require [icfp2012.mine :as mine]
           [hanoi.a-star :as a*]))

(defn print-return
  ([o]
     (println o)
     o)
  ([msg o]
     (println msg o)
     o))

(defn mine-penalty [mine]
  (case (mine/state mine)
    :losing
    (* 50 (+ (:extant-lambdas mine)
             (:dead-lambdas mine)))

    :abort
    (* 25 (+ (:extant-lambdas mine)
             (:dead-lambdas mine)))
                                       
    0))

(defn- possible-mines [mine]
  (mine/next-mines mine))

(defn possible-mines-for-a* [mine]
  (map (fn [[move next-mine]] {:cost (+ 1 (mine-penalty next-mine))
                               :state next-mine
                               :move move})
       (mine/next-mines mine)))

(defn- city-dist [a b]
  (+ (Math/abs (- (a 0) (b 0)))
     (Math/abs (- (a 1) (b 1)))))

(defn tramp-distance [mine r x y def]
  (let [dists (map (fn [sym]
                     (let [tr (mine/trampoline-location mine sym)
                           ta (mine/target-location mine (mine/trampoline-target mine sym))]
                       (+ (city-dist r tr)
                          (city-dist [x y] ta)))) (mine/trampolines mine))]
    (apply min def dists)))

(defn goto-heuristic [x y]
  (fn [mine]
    (let [r (mine/location mine \R)]
      (tramp-distance mine r x y (city-dist r [x y])))))

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

(defn goal-fn [x y]
  (fn [mine]
    (let [r (mine/location mine \R)]
      (= [x y] r))))

(defn search-a*
  ([mine]
     (search-a* mine 500))
  ([mine limit]
     (a*/a* mine
            possible-mines-for-a*
            mine-heuristic
            winning-mine?
            limit)))

(defn search-to
  ([mine x y] (search-to mine x y 100000))
  ([mine x y limit]
     (a*/a* mine
            possible-mines-for-a*
            (goto-heuristic x y)
            (goal-fn x y)
            limit)))

(defn actual-dist [mine dx dy]
  (:cost (search-to mine dx dy)))

(defn solve
  ([mine]
     (solve mine 2000))
  ([mine limit]
     (let [lambdas (mine/locations mine \\)
           robot-dists (into {} (map (fn [[lx ly :as key]] [key (actual-dist mine lx ly)]) lambdas))
           [lift-x lift-y] (mine/location mine \L)
           robot-at-lift (mine/place-bot mine lift-x lift-y)
           lift-dists (into {} (map (fn [[lx ly :as key]] [key (actual-dist robot-at-lift lx ly)]) lambdas))
           [robot-closest lift-closest] (loop [robot-closest []
                                               lift-closest []
                                               [lambda & rest-lambdas] lambdas]
                                          (if (not lambda)
                                            [robot-closest lift-closest]
                                            (if (> (robot-dists lambda) (lift-dists lambda))
                                              (recur
                                               robot-closest
                                               (conj lift-closest lambda)
                                               rest-lambdas)
                                              (recur
                                               (conj robot-closest lambda)
                                               lift-closest
                                               rest-lambdas))))
           robot-closest (sort-by #(robot-dists %) robot-closest)
           lift-closest (sort-by #(lift-dists %) #(compare %2 %1) lift-closest)]
       (loop [[[lx ly] & rest-closest] (concat robot-closest lift-closest [[lift-x lift-y]])
              curr-mine mine
              moves []]
         (if (nil? lx)
           (apply str moves)
           (let [result (search-to curr-mine lx ly)]
             (recur rest-closest
                    (:state result)
                    (apply conj moves (reverse (filter (complement nil?) (map :move (a*/solution-seq result))))))))))))

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
