(ns aho-corasick.core)

(def start-state 0)
(def fail-state? nil?)

(defn- gq
  "graph query function used during goto graph construction"
  [g state c]
  (get-in g [:states state c]))

(defn- gex
  "graph path extending function used during goto graph construction"
  [g state c state']
  (cond-> (assoc-in g [:states state c] state')
    (> state' (:max-state g)) (assoc :max-state state')))

(defn- search-keyword [g state xs]
  (if (empty? xs)
    [state ()]
    (let [state' (gq g state (first xs))]
      (if (fail-state? state')
        [state xs]
        (recur g state' (rest xs))))))

(defn enter-keyword [g kw]
  (let [[state kw'] (search-keyword g start-state kw)
        new-states (iterate inc (inc (:max-state g)))]
    (reduce
     (fn [curr-g [s ns k]]
       (gex curr-g s k ns))
     g (->> kw'
            (interleave (cons state new-states) new-states)
            (partition 3 3)
            (take (count kw'))))))

(defn goto-graph [keyword-set]
  (let [init-graph {:max-state start-state :states {}}]
    (reduce enter-keyword init-graph keyword-set)))

(defn g-query [g state c]
  (let [r (gq g state c)]
    (if (and (= state start-state) (fail-state? r))
      0
      r)))
