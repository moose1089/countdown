(ns countdown.core-2
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.tools.trace :as trace]
            [medley.core :as medley]
            [clojure.walk :as walk])
  (:gen-class))

;; lein run 930 50 75 10 5 1 7
;; takes about 7s

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
                    (do
  ;                    (println "YYY " (:v l) op (:v r))
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
                           :score       (score-for target v)}))))))]
 ;   (println "combine-values" target "from" a "," b "=>"r)
    r))

;;(trace/trace-vars #_s-contains #_combine-values remove-first*)

(defn solve
  [target nums]
  (let [grouping-fn (juxt :v :nums-used)]
    (loop
        [found-values   {} ;; [:v, nums-used] : [values]
         values-waiting (map (partial make-initial-vals target nums) nums)
         best-score     nil
         best-value     nil]
;      (println "found dist" (medley/map-vals count found-values))
;      (println "found all " found-values)
      (cond
        (or
         (zero? (or best-score 1))
         (empty? values-waiting))
        {:best-score best-score
         :best-value best-value
         :v          (:v best-value)}
        :else
        (let [next-v                 (first values-waiting)
              next-v-unused          (:nums-unused next-v)
              matching-keys          (filter #(s-contains (:nums-unused next-v) (second %)) (keys found-values))
              matching-found         (map #(first (get found-values %)) matching-keys) ;; TODO add first
              additional-values      (mapcat #(combine-values target next-v %) matching-found)
              new-vals               (doall (distinct additional-values))
              dead-end-values        (filter #(= [] (:nums-unused %)) new-vals)
              fruitful-values        (filter #(not= [] (:nums-unused %)) new-vals)
              values-to-add-to-found (conj dead-end-values next-v)
              best-candidate         (first (sort-by :score values-to-add-to-found))]
          (println
           "found " (reduce + (map count (vals found-values)))
 ;          "matching found (matching)" matching-found
           "waiting" (count values-waiting) #_(frequencies (map :nums-unused values-waiting))
;           "dist new vals" (frequencies (map :nums-unused new-vals))
           "new vals" (count new-vals)
 ;          "fruitful "(frequencies (map :nums-unused fruitful-values))
           )
          (recur (merge-with conj
                             found-values
                             (group-by grouping-fn values-to-add-to-found))
                 (concat (rest values-waiting) fruitful-values)
                 (if (nil? best-score) (:score best-candidate) (min (:score best-candidate) best-score))
                 (if (or (nil? best-value)
                         (< (:score best-candidate) best-score))
                   (do
                     (println "new best" (pprint best-candidate) "=" (:v best-candidate))
                     best-candidate)
                   best-value)))))))

;; Value =
;; {:v value
;; :nums-used [..]
;; :nums-unused []
;;  :operation +,-,*,/
;;  :leaf boolean
;;  :left x :right x
;;  :score
;; }

(defn -main
  [target & nums]
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (let [r (solve target nums)]
      (when (= 0 (:best-score r))
        (println "FOUND!"))
      (println "BEST" (pprint r))))
  (shutdown-agents))
