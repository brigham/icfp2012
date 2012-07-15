(ns icfp2012.trampoline)

(defprotocol ATrampolineSystem
  (activate [ts letter])
  (destination [ts letter])
  (sources [ts number]))

(declare ->TrampolineSystem)

(defrecord TrampolineSystem [source->dest dest->source]
  ATrampolineSystem
  (destination [ts letter]
    (source->dest letter))
  (sources [ts number]
    (dest->source number))
  (activate [ts letter]
    (let [dest (destination ts letter)
          all-src (sources ts dest)]
      (->TrampolineSystem (apply dissoc source->dest all-src)
                          (dissoc dest->source dest)))))

(defn- map-invert-all [m]
  (apply merge-with into
         (for [[ok ov] m]
           {ov #{ok}})))

(defn create-trampoline-system [source->dest]
  (->TrampolineSystem source->dest (map-invert-all source->dest)))
