(ns countdown.core-4
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.tools.trace :as trace]
            [medley.core :as medley]
            [clojure.walk :as walk])
  (:gen-class))

;; lein v4 676  500 100 5 3 2 1
;; lein v4 930 50 75 10 5 1 7

(defn pprint [s]
  (let [reps {* "*" + "+" - "-" / "/"}
        t    (clojure.walk/prewalk-replace reps s)
        f    (fn [m]
               (cond (and (map? m)
                          (:leaf m)) (:v m)
                     (and (map? m)
                          (:op m))   (str "(" (:left m) " " (:op m) " " (:right m) ")")
                     :else           m))]
    (clojure.walk/postwalk f t)))

(defn score-for [target value]
  (when value (abs (- target value))))

(defn remove-first* [coll value]
  (let [[before after] (split-with #(not= % value) coll)]
    (concat before (rest after))))

(def remove-first (memoize remove-first*))

(defn remove-all* [a b]
  (reduce remove-first a b))

(def remove-all (memoize remove-all*))

(defn make-initial-vals
  [target nums n]
  {:post [(:expr %)]}
  {:v           n
   :expr        (str n)
   :leaf        true
   :nums-avail  []
   :score       (score-for target n)})

(defn state-score
  [s]
  {:post [(:expr %)]}
  (let [best (first (sort-by :score (:nums-avail s)))]
    (assoc s
           :v (:v best)
           :expr (:expr best)
           :score (:score best))))

(defn starting-state [target initial-values]
  (state-score {:nums-avail initial-values}))

(defn next-states [target current-state]
  (let [r
        (filter some?
                (when (not (empty? (:nums-avail current-state)))
                  (for [op    [+ - * /]
                        [a b] (combo/combinations (:nums-avail current-state) 2)
                        [l r] [[a b] [b a]]]
                    (when (and
                           (not (and (= op /) (zero? (:v r))))                     ;; no divide by 0
                           (not (and (#{+ *} op) (= l b) (= r a)))                 ;; no need for swapped with + or *
                           (not (and (= op /) (not (zero? (mod (:v l) (:v r))))))) ;; stick to ℤ
                      (let [v       (op (:v l) (:v r))
                            new-num {:v     v
                                     :expr  (str "(" (:expr l) (pprint op) (:expr r) ")")
                                     :op    op
                                     :score (score-for target v)}]
                        (state-score {:nums-avail (conj (remove-all (:nums-avail current-state) [l r]) new-num)}))))))]
    #_(println "next-states" target "from" current-state "=>"  r)
    r))

#_(defn unique-by
  [f s]
  (map first (vals (group-by f s))))

(defn best-of [a b]
  {:pre {(or (nil? a) (number? a)) (or (nil? b) (number? b))}}
  (cond (nil? a)                  b
        (nil? b)                  a
        (< (:score b) (:score a)) b
        :else                     a))

(def profile (atom 0))

(defn solve
  ([target nums]

   ;(println "TRACE solve" (pprint [target nums energy]))
   (let [grouping-fn    (juxt :v :nums-avail)
         has-2-nums?    #(< 1(count (:nums-avail %)))
         unseen-fn      (fn [found-values new-vals]
                          (remove #(get-in found-values (grouping-fn %)) new-vals))
         initial-values (map (partial make-initial-vals target nums) nums)
         start          (starting-state target initial-values)]
     (loop [found-values   {} ;; {[:nums-avail :v] value}
            states-waiting [start]
            best-value     nil]

       (swap! profile inc)
       (when (zero? (mod @profile 1000))
         (println "found" (count found-values) "waiting" (count states-waiting)
                                        ;"best score" (:score best-value) ":" (:expr best-value) "=" (:v best-value)
                  ))
       (cond (or
              (zero? (or (:score best-value) 1))
              (empty? states-waiting))
             best-value
             :else (let [c                (first states-waiting)
                         candidate-states (sort-by :score (unseen-fn found-values (next-states target c)))]
                     (recur
                      (merge found-values (group-by grouping-fn candidate-states))
                      (concat (rest states-waiting) (filter has-2-nums? candidate-states))
                      (best-of best-value (first candidate-states)))
                     ))))))

(defn -main
  [target & nums]
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (let [r (solve target nums)]
      (when (== 0 (:score r))
        (println "FOUND!"))
      (println "BEST" (:expr r) "=" (:v r), "score=" (:score r))
      (println "num calls" @profile)))
  (shutdown-agents))
