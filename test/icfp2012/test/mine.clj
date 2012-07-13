(ns icfp2012.test.mine
  (:use [icfp2012.mine])
  (:use [clojure.test]))

(deftest test-move-left
  (let [mine (mine-from-thing "data/contest3.map")
        left (move-left mine)]
    (is \space (object-at left 4 8))
    (is \R (object-at left 3 8)))
  (let [mine (mine-from-thing "data/contest1.map")
        moved (move-left mine)]
    (is \space (object-at moved 5 5))
    (is \R (object-at moved 4 5))
    (is \* (object-at moved 3 5)))
)

(deftest test-move-right
  (let [mine (mine-from-thing "data/contest3.map")
        moved (move-right mine)]
    (is \space (object-at moved 4 8))
    (is \R (object-at moved 5 8)))
  (let [mine (mine-from-string "#####
#R* #
#####")
        moved (move-right mine)]
    (is \space (object-at moved 2 2))
    (is \R (object-at moved 3 2))
    (is \* (object-at moved 4 2))))

(deftest test-move-down
  (let [mine (mine-from-thing "data/contest1.map")
        moved (move-down mine)]
    (is \space (object-at moved 5 5))
    (is \R (object-at moved 5 4))))

(deftest test-move-up
  (let [mine (mine-from-thing "data/contest2.map")
        moved (move-up mine)]
    (is \space (object-at moved 2 2))
    (is \R (object-at moved 2 3))))
