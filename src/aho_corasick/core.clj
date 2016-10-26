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
      start-state
      r)))

(defn output [g state]
  (get-in g [:output state]))

(defn f [g state]
  (get-in g [:failure state]))

(defn- failure-transitions [g]
  (let [gtab (get g :states)
        d1ss (vals (gtab start-state))
        ftab (reduce #(assoc %1 %2 0) {} d1ss)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY d1ss)
           ftab ftab]
      (if (empty? queue)
        (assoc g :failure ftab)
        (let [r (peek queue)
              queue' (pop queue)
              xs (keys (gtab r))

              [q ft]
              (loop [xs (keys (gtab r))
                     q queue'
                     ft ftab]
                (if (empty? xs)
                  [q ft]
                  (let [x (first xs)
                        s (g-query g r x)]
                    (recur (rest xs)
                           (conj q s)
                           (assoc ft s
                                  (->> r
                                       (iterate ft)
                                       rest
                                       (map #(g-query g % x))
                                       (drop-while fail-state?)
                                       first))))))]
          (recur q ft))))))

(= (failure-transitions (goto-graph ["he" "she" "his" "hers"]))
   {:max-state 9,
    :states
    {0 {\h 1, \s 3},
     1 {\e 2, \i 6},
     3 {\h 4},
     4 {\e 5},
     6 {\s 7},
     2 {\r 8},
     8 {\s 9}},
    :output {2 #{"he"}, 5 #{"she"}, 7 #{"his"}, 9 #{"hers"}},
    :failure {1 0, 3 0, 2 0, 6 0, 4 1, 8 0, 7 3, 5 2, 9 3}})
