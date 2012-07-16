(ns hanoi.queue
  (require [clojure.data.priority-map :as pmap]))

(defn make-queue []
  (pmap/priority-map))

(defn enqueue [queue & elements]
  (if (empty? elements)
    queue
    (into queue elements)))

(defn peek [queue]
  (first (clojure.core/peek queue)))

(defn size [queue]
  (count queue))

(defn dequeue [queue]
  (pop queue))
