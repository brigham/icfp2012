(ns icfp2012.core
  (require [hanoi.a-star :as a*]
           [hanoi.queue :as q]
           [icfp2012.mine :as mine]
           [icfp2012.solver :as solver]
           [icfp2012.water :as water]))

(set! *warn-on-reflection* true)

(defn- run-solution [mine moves]
  (println "Water Level:" (get-in mine [:water-sim :water-level])
           "Flood Rate:" (get-in mine [:water-sim :flood-rate])
           "Waterproof Life: " (get-in mine [:water-sim :water-life]))
  (print (str (char 27) "[?25l"))
  (let [end-mine
        (mine/execute-moves mine moves
                            (fn [mine]
                              (println "Turn:" (get-in mine [:water-sim :steps])
                                       "Next Rise:" (water/steps-til-rise (:water-sim mine))
                                       "Air:" (water/steps-til-lose (:water-sim mine))
                                       "Lambdas:" (:extant-lambdas mine)
                                       (str (char 27) "[K"))
                              (println (mine/->String mine))
                              (print (str (char 27) "[A"))
                              (dotimes [y (second (mine/dimensions mine))]
                                (print (str (char 27) "[A")))
                              (flush)
                              (Thread/sleep 300)))]
    (dotimes [y (inc (second (mine/dimensions mine)))]
      (println))
    (print (str (char 27) "[?25h"))
    (println "State:" (mine/state end-mine) "Score:" (mine/score end-mine))))

(defn- dump-solution [solution]
  (doseq [state (map :state solution)]
    (println (mine/->String state))
    (dotimes [y (second (mine/dimensions state))]
      (print (str (char 27) "[A")))
    (flush)
    (Thread/sleep 60))
  (dotimes [y (second (mine/dimensions (:state (first solution))))]
    (println))
  (println (map :iter solution))
  (println (map :cost solution))
  (println (apply str (reverse (map :move solution)))))

(defn- solve-mine-with-a* [mine]
  (loop [the-search (time (solver/search-a* mine 5000))]
    (let [solution (a*/solution-seq the-search)]
      (when (not (nil? solution))
        (dump-solution solution)
        ;(recur (q/dequeue the-search))
        ))))

(defn- goto-with-a* [mine x y]
  (let [the-search (time (solver/search-to mine x y 200000))]
    (let [solution (a*/solution-seq the-search)]
      (when (not (nil? solution))
        (dump-solution solution)))))

(defn- solve-mine [mine]
  (let [[solution moves] (time (solver/search mine 800000))]
    (println "Turn:" (get-in solution [:water-sim :steps])
             "Next Rise:" (water/steps-til-rise (:water-sim solution))
             "Air:" (water/steps-til-lose (:water-sim solution)))
    (println (mine/->String solution))
    (println "State:" (mine/state solution) "Score:" (mine/score solution))
    (println "Moves:" (apply str moves))))

(defn -main [& args]
  (let [mine (mine/mine-from-thing (if (empty? args) System/in (first args)))]
    (case (count args)
      (0 1) (solver/solve mine)
      2 (run-solution mine (second args))
      3 (goto-with-a* mine (Integer/parseInt (nth args 1)) (Integer/parseInt (nth args 2))))))
