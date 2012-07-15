(ns hanoi.a-star
  (require [hanoi.queue :as q]))

(defn a* [initial-state comp-fn next-states-fn heuristic-fn goal?-fn max-iterations]
  (letfn [(rec [states seen remaining-iterations]
;            (prn remaining-iterations)
            (let [next (assoc (q/next states) :iter (+ 1 (- max-iterations remaining-iterations)))]
               (if (or (goal?-fn (:state next)) (= 0 remaining-iterations))
                 states
                 (recur
                  (apply
                   q/enqueue
                   (q/dequeue states)
                   (map (fn [state]
                          (into state {:cost (+ (:cost state) (:cost next))
                                       :previous next}))
                        (filter #(not (seen (:state %)))
                                (next-states-fn (:state next)))))
                  (conj seen (next :state))
                  (- remaining-iterations 1)))))]
    (rec (q/enqueue (q/make-queue (fn [a b] (if (and (not (nil? a))
                                                     (not (nil? b)))
                                              (let [fa (+ (heuristic-fn (:state a)) (:cost a))
                                                    fb (+ (heuristic-fn (:state b)) (:cost b))] 
                                                (if (not= fa fb)
                                                  (< fa fb)
                                                  (let [post-cost (comp-fn (:state a) (:state b))]
                                                    (if (not= 0 post-cost)
                                                      (< post-cost 0)
                                                      (recur (:previous a) (:previous b))))))
                                              (and (nil? a) (not (nil? b))))))
                    {:state initial-state :cost 0 :previous nil :iter 0})
         #{}
         max-iterations)))

(defn solution-seq [solution]
  (if (nil? solution)
    nil
    (cons solution (lazy-seq (solution-seq (solution :previous))))))

(defn- build-nodes [root subpaths]
  {:value root
   :children (if (empty? subpaths)
               nil
               (map (fn [[root subpaths]]
                      (build-nodes root (filter (complement empty?) (map rest subpaths))))
                    (group-by first subpaths)))})

(defn build-tree [paths]
  (first (:children (build-nodes nil (map reverse paths)))))

(defn dump-tree [tree]
  (letfn [(rec [tree depth]
            (when (not (nil? tree))
              (println (apply str (repeat depth "  ")) (:value tree))
              (doseq [child (:children tree)]
                (rec child (+ 1 depth)))))]
    (rec tree 0)))
