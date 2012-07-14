(ns icfp2012.index)

(defprotocol AnIndexGroup
  (create-index [group key comp-fn])
  (set-for [group key])
  (value-for [group key])
  (remove-value-from [group key value])
  (add-value-to [group key value]))

(defrecord IndexGroup [storage]
  AnIndexGroup)

(defn create-index-group []
  (->IndexGroup {}))
