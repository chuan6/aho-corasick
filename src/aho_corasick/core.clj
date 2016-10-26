(ns aho-corasick.core)

(def start-state 0)

(def fail-state? nil?)

(defn- gq
  "graph query function used during goto graph construction"
  [gtab state c]
  (get-in gtab [state c]))

(defn- gex
  "graph path extending function used during goto graph construction"
  [g state c state']
  (cond-> (assoc-in g [:states state c] state')
    (> state' (:max-state g)) (assoc :max-state state')))

(defn- gout
  "associate the given keyword with the given state"
  [otab state kw]
  (update otab state (fnil conj #{}) kw))

(defn- search-keyword [gtab state xs]
  (if (empty? xs)
    [state ()]
    (let [state' (gq gtab state (first xs))]
      (if (fail-state? state')
        [state xs]
        (recur gtab state' (rest xs))))))

(defn enter-keyword [{gtab :states otab :output :as g} kw]
  (let [[s kw']
        (search-keyword gtab start-state kw)

        [result-graph result-state]
        (reduce
         (fn [[g s] [s' k]]
           [(gex g s k s') s'])
         [g s]
         (->> kw'
              (interleave (rest (iterate inc (:max-state g))))
              (partition 2 2)
              (take (count kw'))))]
    (update result-graph :output gout result-state kw)))

(defn goto-graph [keyword-set]
  (let [init-graph {:max-state start-state :states {}}]
    (reduce enter-keyword init-graph keyword-set)))

(defn g-query [gtab state c]
  (let [r (gq gtab state c)]
    (if (and (= state start-state) (fail-state? r))
      start-state
      r)))

(defn output [g state]
  (get-in g [:output state]))

(defn f [g state]
  (get-in g [:failure state]))

(defn- depth-1-states [gtab]
  (vals (gtab start-state)))

(defn- find-fail-state [gtab ftab r a]
  (->> (rest (iterate ftab r))
       (map #(g-query gtab % a))
       (drop-while fail-state?)
       first))

(defn- init-ftab [d1ss]
  (reduce #(assoc %1 %2 0) {} d1ss))

(defn- add-output-to [s]
  (fn [otab kw]
    (gout otab s kw)))

(defn- failure-transitions [g]
  (let [gtab (get g :states)
        d1ss (depth-1-states gtab)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY d1ss)
           ftab  (init-ftab d1ss)
           otab  (get g :output)]
      (if (empty? queue)
        (assoc g :failure ftab :output otab)
        (let [r (peek queue)
              xs (keys (gtab r))

              [queue' ftab' otab']
              (reduce
               (fn [[q ft ot] x]
                 (let [s (g-query gtab r x)
                       fts (find-fail-state gtab ft r x)
                       add-output (add-output-to s)]
                   [(conj q s)
                    (assoc ft s fts)
                    (reduce add-output ot (get ot fts))]))
               [(pop queue) ftab otab] xs)]
          (recur queue' ftab' otab'))))))

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
    :output {2 #{"he"}, 5 #{"she" "he"}, 7 #{"his"}, 9 #{"hers"}},
    :failure {1 0, 3 0, 2 0, 6 0, 4 1, 8 0, 7 3, 5 2, 9 3}})
