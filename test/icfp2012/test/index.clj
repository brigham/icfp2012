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
                       (add-to \* [6 7])
                       (add-to \* [2 3])
                       (add-to \* [1 9])
                       (add-to \* [1 1])
                       (add-to \R [3 4])
                       (add-to \L [2 1])
                       (remove-from \L [2 1])
                       (add-to \O [2 1]))]
    (is (= [3 4] (value-for with-items \R)))
    (is (= [2 1] (value-for with-items \O)))
    (is (nil? (value-for with-items \L)))
    (is (= [[1 1] [1 9] [2 3] [6 7]] (vec (set-for with-items \*))))))
