(ns hanoi.queue)

(defn make-queue [comp-fn]
  {:comp-fn comp-fn :queue (sorted-set-by comp-fn)})

(defn enqueue [queue & elements]
  (if (empty? elements)
    queue
    (assoc queue :queue (apply conj (queue :queue) elements))))

(defn next [queue]
  (first (queue :queue)))

(defn size [queue]
  (count (queue :queue)))

(defn dequeue [queue]
  (assoc queue :queue (disj (queue :queue) (next queue))))
