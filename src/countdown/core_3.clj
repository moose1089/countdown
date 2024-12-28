(ns countdown.core-3
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.tools.trace :as trace]
            [medley.core :as medley]
            [clojure.walk :as walk])
  (:gen-class))

;; lein v3 676  500 100 5 3 2 1
;; lein v3 930 50 75 10 5 1 7

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

(defn make-initial-vals [target nums n]
  {:v           n
   :leaf        true
   :nums-used   [n]
   :nums-unused (remove-first* nums n)
   :score       (score-for target n)})

(defn s-contains*
  "Seq a contains b"
  [a b]
  ;; if not then b has a item not listed in a
  (empty? (remove-all b a)))

(def s-contains (memoize s-contains*))

(defn combine-values [target a b]
  (let [r
        (filter some?
                (when (and (:nums-unused a) (:nums-unused b))
                  (for [op    [+ - * /]
                        [l r] [[a b] [b a]]]
                    (when (and
                           (s-contains (:nums-unused a) (:nums-used b))            ;; they fit together
                           (not (and (= op /) (zero? (:v r))))                     ;; no divide by 0
                           (not (and (#{+ *} op) (= l b) (= r a)))                 ;; No need for swapped with + or *
                           (not (and (= op /) (not (zero? (mod (:v l) (:v r))))))) ;; stick to ℤ
                     (let [v (op (:v l) (:v r))]
                       {:v           v
                        :op          op
                        :left        l
                        :right       r
                        :nums-used   (concat (:nums-used l) (:nums-used r))
                        :nums-unused (remove-all (:nums-unused l) (:nums-used r))
                        :score       (score-for target v)})))))]
;;;    (println "combine-values" target "from" a "," b "=>" (pprint r))
    r))

#_(defn unique-by
  [f s]
  (map first (vals (group-by f s))))

(def MAX_E 200)
(def ENERGY_EXPANSION_FACTOR 2)
(def SECONDARY_BRANCH_FACTOR 0.2)

(defn best-of [a b]
  {:pre {(or (nil? a) (number? a)) (or (nil? b) (number? b))}}
  (cond (nil? a)                  b
        (nil? b)                  a
        (< (:score b) (:score a)) b
        :else                     a))

(defn sum-squares [s]
  (reduce + (map * s s)))

(defn value-position
  "Lower is better"
  [nums]
  (let [sn (sort-by :score nums)]
    (cond (== 0 (:score (first sn))) 0
          :else (/
                 (* 10 (:score (first sn)))
                 (sum-squares (map :score nums))
                 #_(max 1 (count (filter #(<= % 5) (map :v nums))))
                 ))))

(def profile (atom 0))

(defn solve
  ;; return best found
  ([target nums]
   (let [initial-values (map (partial make-initial-vals target nums) nums)]
     (loop [e          1
            best-value nil]
       (println "E=" e " Best=" (pprint best-value) "=" (:v best-value), "score=" (:score best-value))
       (if (or (== 0 (or (:score best-value) 1)) (< MAX_E e))
         best-value
         (recur (* ENERGY_EXPANSION_FACTOR e)
                (best-of best-value (solve target initial-values e)))))))
  ([target nums energy]
   (swap! profile inc)
   ;(println "TRACE solve" (pprint [target nums energy]))
   (if (or (< (count nums) 2) (neg? energy))
     (first (sort-by :score nums))
     (let [combinations    (combo/combinations nums 2)
           candidate-steps (sort-by (comp :value)
                                    (for [c         combinations
                                          new-value (apply combine-values target c)]
                                      (let [new-nums (conj (remove-all nums c) new-value)
                                            value    (value-position new-nums)]
                                       {:value value
                                        :nums  new-nums}
                                       )))
           r               (first (sort-by :score (map (fn [[i c]]
                                                         (let [e (- energy (* i SECONDARY_BRANCH_FACTOR) 1)]
                                                           (solve target (:nums c) e)))
                                         (map-indexed vector candidate-steps))))]
       r
       ))))


(trace/trace-vars #_s-contains #_combine-values
                  #_value-position
                  #_remove-first*
                  )

(defn -main
  [target & nums]
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (let [r (solve target nums)]
      (when (== 0 (:score r))
        (println "FOUND!"))
      (println "BEST" (pprint r) "=" (:v r), "score=" (:score r))
      (println "num calls" @profile)))
  (shutdown-agents))
