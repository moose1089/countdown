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
  (when (and (:nums-unused a) (:nums-unused b))
    (for [op [+ - * /]]
      (when (and
             (s-contains (:nums-unused a) (:nums-used b))
             (not (and (= op /) (zero? (:v b))))
             (not (and (= op /) (not (zero? (mod (:v a) (:v b)))))))
        (let [v (op (:v a) (:v b))]
          {:v           v
           :op          op
           :left        a
           :right       b
           :nums-used   (concat (:nums-used a) (:nums-used b))
           :nums-unused (remove-all (:nums-unused a) (:nums-used b))
           :score       (score-for target v)})))))

;;(trace/trace-vars #_s-contains #_combine-values remove-first*)

(defn solve
  [target nums]
  (loop
      [found-values   {} ;; nums-unused : [ values]
       values-waiting (map (partial make-initial-vals target nums) nums)
       best-score     nil
       best-value     nil]
    ;(println "found" (medley/map-vals count found-values))
    (cond
      (or
       (zero? (or best-score 1))
       (empty? values-waiting))
      {:best-score best-score
       :best-value best-value
       :v          (:v best-value)}
      :else
      (let [next-v              (first values-waiting)
            next-v-unused       (:nums-unused next-v)
            matching-keys       (filter #(s-contains % (:nums-used next-v)) (keys found-values))
            matching-found      (mapcat #(get found-values %) matching-keys)
            additional-values-1 (doall (distinct (filter some?
                                                         (mapcat #(combine-values target next-v %) matching-found))))
            additional-values-2 (doall (distinct (filter some?
                                                         (mapcat #(combine-values target % next-v) matching-found))))
            all-additional      (concat additional-values-1 additional-values-2)
            _                   (println ;"found" (reduce + (map count (vals found-values)))
                                 "waiting" (count values-waiting)
                                 "dist" (frequencies (map :nums-unused all-additional))
                                 "new vals" (count additional-values-1) (count additional-values-2))
            new-vals            (doall
                                 (filter some? all-additional))]
        (recur (update found-values next-v-unused conj next-v)
               (concat (rest values-waiting) new-vals)
               (if (nil? best-score) (:score next-v) (min (:score next-v) best-score))
               (if (or (nil? best-value)
                       (< (:score next-v) best-score))
                 (do
                   (println "new best" (pprint next-v) "=" (:v next-v))
                   next-v)
                 best-value))))))

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
