(ns aho-corasick.state)

(def start 0)

(def fail? nil?)

(defn- q
  "graph query function used during goto graph construction"
  [gt state c]
  (get-in gt [state c]))

(defn search-keyword [gt state xs]
  (if (empty? xs)
    [state ()]
    (let [state' (q gt state (first xs))]
      (if (fail? state')
        [state xs]
        (recur gt state' (rest xs))))))

(defn goto [gt state c]
  (let [r (q gt state c)]
    (if (and (= state start) (fail? r))
      start r)))

(defn depth-1s [gt] (vals (gt start)))
