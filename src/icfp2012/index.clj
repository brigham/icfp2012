(ns icfp2012.index)

(defprotocol AnIndexGroup
  (index-keys [group])
  (create-index [group key comp-fn])
  (set-for [group key])
  (value-for [group key])
  (clear [group key])
  (remove-from [group key value])
  (add-to [group key value])
  (add-all [group key values]))

(declare ->IndexGroup)

(defrecord IndexGroup [storage]
  AnIndexGroup
  (index-keys [group]
    (keys storage))
  (create-index [group key comp-fn]
    (->IndexGroup (assoc storage key (sorted-set-by comp-fn))))
  (clear [group key]
    (->IndexGroup (update-in storage [key] empty)))
  (remove-from [group key value]
    (->IndexGroup (update-in storage [key] #(disj % value))))
  (add-to [group key value]
    (->IndexGroup (update-in storage [key] #(conj % value))))
  (add-all [group key values]
    (if (> (count values) 0)
      (->IndexGroup (update-in storage [key] #(apply conj % values)))
      group))
  (set-for [group key]
    (get storage key))
  (value-for [group key]
    (first (get storage key))))

(defn create-index-group []
  (->IndexGroup {}))
