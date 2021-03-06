(ns aho-corasick.core
  (:require [aho-corasick.state :as s]))

(defn- gex
  "graph path extending function used during goto graph construction"
  [g state c state']
  (cond-> (assoc-in g [:states state c] state')
    (> state' (:max-state g)) (assoc :max-state state')))

(defn- gout
  "associate the given keyword with the given state"
  [otab state kw]
  (update otab state (fnil conj #{}) kw))

(defn- enter-keyword [{gtab :states otab :output :as g} kw]
  (let [[s kw'] (s/search-keyword gtab s/start kw)

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

(defn- goto-graph [keyword-set]
  (let [init-graph {:max-state s/start :states {}}]
    (reduce enter-keyword init-graph keyword-set)))

(defn- find-fail-state [gtab ftab r a]
  (->> (rest (iterate ftab r))
       (map #(s/goto gtab % a))
       (drop-while s/fail?)
       first))

(defn- init-ftab [d1ss]
  (reduce #(assoc %1 %2 0) {} d1ss))

(defn- add-output-to [s]
  (fn [otab kw]
    (gout otab s kw)))

(defn- failure-transitions [{gtab :states otab :output :as g}]
  (let [d1ss      (s/depth-1s gtab)
        symbols   (comp keys gtab)
        trans     (partial s/goto gtab)
        fallback  (partial find-fail-state gtab)
        expend-at (fn [r [q f o] a]
                    (let [s      (trans r a)
                          fs     (fallback f r a)
                          add-to (add-output-to s)]
                      [(conj q s)
                       (assoc f s fs)
                       (reduce add-to o (get o fs))]))]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY d1ss)
           ftab  (init-ftab d1ss)
           otab  otab]
      (if (empty? queue)
        (assoc g :failure ftab :output otab)
        (let [r (peek queue)
              [queue'
               ftab'
               otab'] (reduce (partial expend-at r)
                              [(pop queue) ftab otab] (symbols r))]
          (recur queue' ftab' otab'))))))

(defn construct [kws]
  (assert (= (count kws) (count (set kws))))
  (-> kws
      goto-graph          ; Algorithm 2.
      failure-transitions ; Algorithm 3.
      ))

(defn matching [{gtab :states otab :output ftab :failure :as g} s]
  "Algorithm 1."
  (let [sv       (vec s)
        n        (count sv)
        goto     (partial s/goto gtab)
        fallback (partial find-fail-state gtab ftab)
        entry    (fn [i xs] {:position i :outputs xs})]
    (loop [i      0
           state  s/start
           matches []]
      (if (= i n)
        matches
        (let [a      (nth sv i)
              sa     (goto state a)
              sa'    (if (not (s/fail? sa)) sa
                         (fallback state a))
              founds (otab sa')]
          (recur (inc i)
                 sa'
                 (cond-> matches
                   (seq founds) (conj (entry i founds)))))))))

(let [g (construct ["he" "she" "his" "hers"])]
  (and (= g
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
       (= (matching g "he loves her, but she doesn't think that he is hers")
          [{:position 1  :outputs #{"he"}}
           {:position 10 :outputs #{"he"}}
           {:position 20 :outputs #{"she" "he"}}
           {:position 42 :outputs #{"he"}}
           {:position 48 :outputs #{"he"}}
           {:position 50 :outputs #{"hers"}}])))
