(ns hanoi.a-star
  (require [hanoi.queue :as q]
           [icfp2012.mine :as m]))

(declare solution-seq)

(defn a* [initial-state next-states-fn heuristic-fn goal?-fn max-iterations]
  (letfn [(rec [states seen remaining-iterations]
            (let [next (assoc (q/peek states) :iter (+ 1 (- max-iterations remaining-iterations)))]
              (if (or (zero? (q/size states))
                      (nil? (:state next))
                      (goal?-fn (:state next))
                      (zero? remaining-iterations))
                (do
                  (when (zero? (q/size states))
                    (println "Ran out of queue items at" remaining-iterations))
                  (when (zero? remaining-iterations)
                    (println "Ran out of time in A*"))
                  (when (nil? (:state next))
                    (println "Got null state: " next))
                  next)
                (recur
                 (apply q/enqueue
                  (q/dequeue states)
                  (map (fn [state]
                         (let [new-cost (+ (:cost state) (:cost next))]
                           [(into state {:cost new-cost
                                         :previous next}) (+ new-cost (heuristic-fn (:state state)))]))
                       (filter #(not (seen (:state %)))
                               (next-states-fn (:state next)))))
                 (conj seen (next :state))
                 (- remaining-iterations 1)))))]
    (rec (q/enqueue (q/make-queue)
                    [{:state initial-state :cost 0 :previous nil :iter 0} (heuristic-fn initial-state)])
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
