(ns icfp2012.test.water
  (:use [icfp2012.water])
  (:use [clojure.test]))

(deftest test-water-rising
  (let [sim (->WaterSim 2 5 100 0 0)
        end (-> sim
                (simulate 100)
                (simulate 100)
                (simulate 100)
                (simulate 100)
                (simulate 100)

                (simulate 100)
                (simulate 100))]
    (is (= 3 (water-level end)))
    (is (= 3 (steps-til-rise end)))))
