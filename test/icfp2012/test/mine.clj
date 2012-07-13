(ns icfp2012.test.mine
  (:use [icfp2012.mine])
  (:use [clojure.test]))

(deftest test-move-left
  (let [mine (mine-from-thing "data/contest3.map")
        left (move-left mine)]
    (is (= \space (object-at left 4 8)))
    (is (= \R (object-at left 3 8))))
  (let [mine (mine-from-thing "data/contest1.map")
        moved (move-left mine)]
    (is (= \space (object-at moved 5 5)))
    (is (= \R (object-at moved 4 5)))
    (is (= \* (object-at moved 3 4)))))

(deftest test-move-right
  (let [mine (mine-from-thing "data/contest3.map")
        moved (move-right mine)]
    (is (= \space (object-at moved 4 8)))
    (is (= \R (object-at moved 5 8))))
  (let [mine (mine-from-string "#####
#R* #
#####")
        moved (move-right mine)]
    (is (= \space (object-at moved 2 2)))
    (is (= \R (object-at moved 3 2)))
    (is (= \* (object-at moved 4 2)))))

(deftest test-move-down
  (let [mine (mine-from-thing "data/contest1.map")
        moved (move-down mine)]
    (is (= \space (object-at moved 5 5)))
    (is (= \R (object-at moved 5 4)))))

(deftest test-move-up
  (let [mine (mine-from-thing "data/contest2.map")
        moved (move-up mine)]
    (is (= \space (object-at moved 2 2)))
    (is (= \R (object-at moved 2 3)))))

(deftest update-replaces-closed-list
  (let [mine (mine-from-string "###
#R#
#L#")
        moved (wait-turn mine)]
    (is (= 0 (:extant-lambdas moved)))
    (is (= \O (object-at moved 2 1)))))

(deftest test-abort
  (let [mine (mine-from-thing "data/contest1.map")
        move-1 (move-left mine)
        move-2 (move-down move-1)
        move-3 (abort move-2)]
    (is (done? move-3))
    (is (= :abort (state move-3)))
    (is (= 47 (score move-3)))))

(deftest test-win
  (let [mine (mine-from-thing "data/contest1.map")
        end (-> mine
                move-left
                move-down
                move-right
                move-down
                move-down
                move-left
                move-right
                move-up
                move-left
                move-left
                move-left
                move-down
                move-left)]
    (is (done? end))
    (is (= :winning (state end)))
    (is (= 212 (score end)))))

(deftest test-invalid
  (let [mine (mine-from-string "###\n#R#\n###\n")
        moved (move-left mine)]
    (is (= \R (object-at moved 2 2)))
    (is (= -1 (score moved)))))

(deftest rocks-falling
  (let [mine (mine-from-string "
######################################
#R                                   #
#    *    *    *#   *    *    *    *##
#         *    *    *#   \\    \\#   \\ #
#                                    #
######################################")
        fall (wait-turn mine)]
    
    (is (= \* (object-at mine 6 4)))
    (is (= \* (object-at mine 11 4)))
    (is (= \* (object-at mine 16 4)))
    (is (= \* (object-at mine 21 4)))
    (is (= \* (object-at mine 26 4)))
    (is (= \* (object-at mine 31 4)))
    (is (= \* (object-at mine 36 4)))

    (is (= \space (object-at fall 6 4)))
    (is (= \space (object-at fall 11 4)))
    (is (= \space (object-at fall 16 4)))
    (is (= \space (object-at fall 21 4)))
    (is (= \space (object-at fall 26 4)))
    (is (= \* (object-at fall 31 4)))
    (is (= \* (object-at fall 36 4)))

    (is (= \* (object-at fall 6 3)))
    (is (= \* (object-at fall 12 3)))
    (is (= \* (object-at fall 15 3)))
    (is (= \* (object-at fall 20 3)))
    (is (= \* (object-at fall 27 3)))))

(deftest rock-crash
  (let [mine (mine-from-string "
#* *#
#* *#
#####")
        fall (wait-turn mine)]
    (is (= \space (object-at fall 2 3)))
    (is (= \space (object-at fall 3 3)))
    (is (= \space (object-at fall 4 3)))
    (is (= \* (object-at fall 2 2)))
    (is (= \* (object-at fall 3 2)))
    (is (= \* (object-at fall 4 2)))))

(deftest test-lose
  (let [mine (mine-from-string "
#*#
# #
# #
# #
#R#
###")
        end (-> mine
                wait-turn
                wait-turn
                wait-turn)]
    (is (done? end))
    (is (= :losing (state end)))
    (is (= -3 (score end)))))
