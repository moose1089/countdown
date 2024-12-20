(ns countdown.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.tools.trace :as trace]
            [clojure.walk :as walk])
  (:gen-class))

;; lein run 930 50 75 10 5 1 7
;; takes about 7s

(defn pprint [s]
  (let [f {* "*" + "+" - "-" / "/"}]
    (clojure.walk/prewalk-replace f s)))

(def gen-shapes)

(defn gen-shapes*
  "Represent number positions by :n. Generates all tree shapes possible with n
  numbers. The second postion is the number of unbound numbers on the left hand
  branch"
  [n]
  (if ( = 1 n)
    [:n]
    (distinct
     (apply concat
            (for [i (range 1 n)]
              (apply concat
                     (for [operation [+ - * /]]
                       (let [[a-trees b-trees] [(gen-shapes i) (gen-shapes (- n i))]]
                         (for [part-a-tree a-trees
                               part-b-tree b-trees]
                           (list operation i part-a-tree part-b-tree))))))))))

(def gen-shapes (memoize gen-shapes*))

(def populate-tree)

(defn populate-tree*
  [numbers shape]
  (cond
    (= 1 (count numbers)) (first numbers)
    :else (let [[op n left right]  shape]
            (list op
                  (populate-tree (take n numbers) left)
                  (populate-tree (drop n numbers) right)))))

(def populate-tree (memoize populate-tree*))

(defn populate-trees
  [combination shape]
  (let [permuations (combo/permutations combination)]
    (for [p permuations]
      (populate-tree p shape))))

(defn get-function-for-shape
  "Return a function that provides score for combination for this shape "
  [shape target num-args]
  (let [args            (mapv #(symbol (str "arg_" %)) (range num-args))
        shape-with-args (populate-tree* args shape)
        shaped-fn       (eval (concat '(fn) [args] [shape-with-args]))]
    (fn [args]
      (let [value (try
                    (apply shaped-fn args)
                    (catch ArithmeticException e ;; div by 0
                      nil))]
        (merge
         (when (= target value)
           {:message "!!!!! WINNER !!!"})
         {:tree   (populate-tree* args shape)
          :target target
          :score  (when value (abs (- target value)))
          :value  value})))))

(defn solve
  [target nums]
  (doall
   (reduce
    (fn [a i]
      (if (or
           (nil? a)
           (and
            (:score i)
            (< (:score i) (:score a))))
        (do
          (print "\nNew best" (pprint i) "")
          (flush)
          (if (zero? (:score i))
            (reduced i)
            i))
        a))
    nil
    (apply concat
           (for [i     (range 1 (inc (count nums)))
                 shape (gen-shapes i)]
             (let [f (get-function-for-shape shape target i)]
               (map f (combo/permuted-combinations nums i))))))))

(defn -main
  [target & nums]
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (solve target nums))
  (shutdown-agents)
  )
