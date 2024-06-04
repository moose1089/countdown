(ns countdown.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk])
  (:gen-class))


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
  (println "Evaluating tree" (pprint tree))
  (let [value (eval tree)]
    {:tree tree
     :target target
     :score (abs (- target value))
     :value value}))

(defn eval-all
  [target trees]
  (sort-by :score
           (for [t trees]
             (evaluate target t))))

(defn gen-trees
  [nums]
  #_(println "gen-trees" nums)
  (if (= 1 (count nums))
    [(first nums)]
    (let [partitions     (combo/partitions nums :min 2 :max 2)
          all-partitions (concat partitions (map reverse partitions))]
      (for [operation [+ - * /]
            [part-a part-b] all-partitions
            part-a-tree (gen-trees part-a)
            part-b-tree (gen-trees part-b)
            :when (not
                   (and (= operation /)
                        (zero? (eval part-b-tree))))]
        (list operation part-a-tree part-b-tree)))))

;;['(+ 5 3)]


(defn -main
  "I don't do a whole lot ... yet."
  [target & nums]
  (let [target (Integer/parseInt target)
        nums (map #(Integer/parseInt %) nums)
        trees (gen-trees nums)]
    (println "Aiming for" target "with" nums)
    (println "Result" (pprint (first (eval-all target trees)))))
;  (+ 5 (* 3 7))
  )