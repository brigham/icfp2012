(ns icfp2012.test.trampoline
  (use [icfp2012.trampoline]
       [clojure.test]))

(deftest test-trampolines
  (let [system (create-trampoline-system {"A" "1",
                                          "B" "2",
                                          "C" "3",
                                          "D" "2",
                                          "E" "1",
                                          "F" "2"})
        act-b (activate system "B")
        act-d (activate system "D")
        act-c (activate system "C")]
    (is (= "1" (destination system "A")))
    (is (= "2" (destination system "B")))
    (is (= "3" (destination system "C")))
    (is (= "2" (destination system "D")))
    (is (= "1" (destination system "E")))
    (is (= "2" (destination system "F")))
    (is (= #{"A" "E"} (sources system "1")))
    (is (= #{"B" "D" "F"} (sources system "2")))
    (is (= #{"C"} (sources system "3")))
    (is (nil? (destination act-b "B")))
    (is (nil? (destination act-b "D")))
    (is (nil? (destination act-b "F")))   
    (is (nil? (destination act-d "B")))
    (is (nil? (destination act-d "D")))
    (is (nil? (destination act-d "F")))
    (is (nil? (destination act-c "C")))
    (is (nil? (sources act-b "2")))
    (is (nil? (sources act-d "2")))
    (is (nil? (sources act-c "3")))
    (is (= #{"A" "C" "E"} (set (sources act-b))))))
