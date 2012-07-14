(ns icfp2012.core
  (require [icfp2012.mine :as mine]))

(defn -main [& args]
  (let [mine (mine/mine-from-thing (first args))]
    (print (str (char 27) "[?25l" (char 27) "[44m"))
    (let [end-mine
          (mine/execute-moves mine (second args)
                              (fn [mine]
                                (println (mine/->String mine))
                                (dotimes [y (second (mine/dimensions mine))]
                                  (print (str (char 27) "[A")))
                                (flush)
                                (Thread/sleep 300)))]
      (dotimes [y (second (mine/dimensions mine))]
        (println))
      (print (str (char 27) "[?25h" (char 27) "[0m"))
      (println "State:" (mine/state end-mine) "Score:" (mine/score end-mine))))
  nil)
