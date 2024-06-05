(ns countdown.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk])
  (:gen-class))


;; lein run 930 50 75 10 5 1 7
;; takes about 5 mins

(defn pprint
  [s]
  (let [f (fn [x]
            (cond
              (= x *) "*"
              (= x +) "+"
              (= x -) "-"
              (= x /) "/"
              :else x))]
    (walk/postwalk f s)))

(defn evaluate
  "Returns distance to target"
  [target tree]
  (let [value (eval tree)]
    (merge
     (when (= target value)
       {:message "!!!!! WINNER !!!"})
     {:tree tree
      :target target
      :score (abs (- target value))
      :value value})))

(defn eval-all
  [target trees]
  (sort-by :score
           (for [t trees]
             (evaluate target t))))

(def gen-trees)

(def partitions-mem (memoize combo/partitions))

(defn gen-trees*
  [nums]
  (if (= 1 (count nums))
    [(first nums)]
    (let [partitions (partitions-mem nums :min 2 :max 2)]
      (apply concat
             (for [operation       [+ - * /]
                   [part-a part-b] (if (#{+ *} operation) ;; these are associative
                                     partitions
                                     (concat partitions (map reverse partitions)))]
               (let [[a-trees b-trees] (pvalues (gen-trees part-a) (gen-trees part-b))] ;; perhaps separate large/small calls
                 (for [part-a-tree a-trees
                       part-b-tree    b-trees
                       :when           (not (and (= operation /)
                                                 (zero? (eval part-b-tree))))]
                   (do
                     (list operation part-a-tree part-b-tree)))))))))

(def gen-trees (memoize gen-trees*))
;(def gen-trees ( identity gen-trees*))


(defn try-combination
  [target combination]
  (let [trees (gen-trees combination)
        best (first (eval-all target trees))]
    (println  "Using number set " combination  " =>   Result" (pprint best))
    (when (zero? (:score best))
      (System/exit 0))))

(defn -main
  [target & nums]
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (let [combinations (for [n           (range 1 (inc (count nums)))
                             combination (combo/combinations nums n)]
                         combination)]
      (doall (map #(try-combination target %) combinations)))))
