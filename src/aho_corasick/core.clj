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

(defn- gout
  "associate the given keyword with the given state"
  [g state kw]
  (update-in g [:output state] (fnil conj #{}) kw))

(defn- search-keyword [g state xs]
  (if (empty? xs)
    [state ()]
    (let [state' (gq g state (first xs))]
      (if (fail-state? state')
        [state xs]
        (recur g state' (rest xs))))))

(defn enter-keyword [g kw]
  (let [[s kw'] (search-keyword g start-state kw)]
    (as-> (reduce
           (fn [[g s] [s' k]]
             [(gex g s k s') s'])
           [g s]
           (->> kw'
                (interleave (rest (iterate inc (:max-state g))))
                (partition 2 2)
                (take (count kw'))))
        [result-graph result-state]
      (gout result-graph result-state kw))))

(defn goto-graph [keyword-set]
  (let [init-graph {:max-state start-state :states {}}]
    (reduce enter-keyword init-graph keyword-set)))

(defn g-query [g state c]
  (let [r (gq g state c)]
    (if (and (= state start-state) (fail-state? r))
      0
      r)))

(defn output [g state]
  (get-in g [:output state]))
