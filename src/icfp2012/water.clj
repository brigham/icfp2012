(ns icfp2012.water)

(defprotocol AWaterSim
  (water-level [sim])
  (simulate [sim y])
  (operating? [sim])
  (wet? [sim])
  (steps-til-rise [sim])
  (steps-til-lose [sim]))

(declare ->WaterSim)

(defrecord WaterSim [water-level flood-rate water-life steps wet-steps]
  AWaterSim
  (water-level [sim] water-level)
  (simulate [sim y]
    (let [steps (inc steps)
          water-level (if (and (> flood-rate 0) (= 0 (mod steps flood-rate))) (inc water-level) water-level)
          wet-steps (if (<= y water-level) (inc wet-steps) 0)]
      (->WaterSim water-level flood-rate water-life steps wet-steps)))
  (operating? [sim]
    (<= wet-steps water-life))
  (wet? [sim]
    (> wet-steps 0))
  (steps-til-rise [sim]
    (if (> flood-rate 0)
      (- flood-rate (mod steps flood-rate))
      Integer/MAX_VALUE))
  (steps-til-lose [sim]
    (inc (- water-life wet-steps))))
