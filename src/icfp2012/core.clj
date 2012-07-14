(ns icfp2012.core
  (require [icfp2012.mine :as mine]
           [icfp2012.water :as water]))

(defn -main [& args]
  (let [mine (mine/mine-from-thing (first args))]
    (println "Water Level:" (get-in mine [:water-sim :water-level])
             "Flood Rate:" (get-in mine [:water-sim :flood-rate])
             "Waterproof Life: " (get-in mine [:water-sim :water-life]))
    (print (str (char 27) "[?25l"))
    (let [end-mine
          (mine/execute-moves mine (second args)
                              (fn [mine]
                                (println "Turn:" (get-in mine [:water-sim :steps])
                                         "Next Rise:" (water/steps-til-rise (:water-sim mine))
                                         "Air:" (water/steps-til-lose (:water-sim mine)))
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
  nil)
