(ns icfp2012.test.index
  (use [icfp2012.index]
       [clojure.test]))

(deftest add-items
  (let [indices (create-index-group)
        with-items (-> indices
                       (create-index \R compare)
                       (create-index \L compare)
                       (create-index \O compare)
                       (create-index \* compare)
                       (add-all \* [[6 7] [2 3] [1 9] [1 1]])
                       (add-to \R [3 4])
                       (add-to \L [2 1])
                       (remove-from \L [2 1])
                       (remove-all \* '([2 3] [1 1]))
                       (add-to \O [2 1]))]
    (is (= [3 4] (value-for with-items \R)))
    (is (= [2 1] (value-for with-items \O)))
    (is (nil? (value-for with-items \L)))
    (is (= [[1 9] [6 7]] (vec (set-for with-items \*))))))
