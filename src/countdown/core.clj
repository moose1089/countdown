(ns countdown.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprint]
            [clojure.tools.trace :as trace]
            [clojure.walk :as walk])
  (:gen-class))

;; lein run 930 50 75 10 5 1 7
;; takes about 5 mins

;; improvement by codestral.
(defn pprint [s]
  (let [f {* "*" + "+" - "-" / "/"}]
    (clojure.walk/prewalk-replace f s)))

(defn evaluate
  "Returns distance to target"
  [^long target ^clojure.lang.IPersistentMap tree]
  (let [value (if (number? tree)
                tree
                (try (eval tree)
                     (catch ArithmeticException e ;; div by 0
                       nil))
                )]
    (merge
     (when (= target value)
       {:message "!!!!! WINNER !!!"})
     {:tree tree
      :target target
      :score (when value (abs (- target value)))
      :value value})))

(defn eval-all
  [target trees]
;  (println "eval-all trees " trees )
  (sort-by :score
           (filter :score  ;; removes invalids due to div by 0
                   (for [t trees]
                     (evaluate target t)))))

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
               (let [[a-trees b-trees] [(gen-trees part-a) (gen-trees part-b)]]
                 (for [part-a-tree a-trees
                       part-b-tree b-trees
                       :when       (not (and (= operation /)
                                             (zero? (eval part-b-tree))))]
                   (do
                     (list operation part-a-tree part-b-tree)))))))))

(def gen-trees (memoize gen-trees*))

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
    (and
     (= 1 (count numbers))) (first numbers)
    :else (let [[op n left right]  shape]
            (list op
                  (populate-tree (take n numbers) left)
                  (populate-tree (drop n numbers) right)))))

(def populate-tree (memoize populate-tree*))

(defn populate-trees
  [combination shape]
  ;; (println "populate-trees call" combination (pprint shape))
  (let [permuations (combo/permutations combination)
        r (for [p permuations]
            (populate-tree p shape))]
    r
    ))

(defn try-combination
  [target combination]
  (let [trees (gen-trees combination)
        best  (first (eval-all target trees))]
    (println  "Using number set " combination  " =>   Result" (pprint best))
    (when (zero? (:score best))
      (System/exit 0))))

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

(defn method-3
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
          (print "\nNew best" (pprint i) "\n")
          (flush)
          (if (zero? (:score i))
            (reduced i)
            i))
        a))
    nil
    (apply concat
           (for [i (range 1 (inc (count nums)))]
             (apply concat
                    (for [shape (gen-shapes i)]
                      (let [f (get-function-for-shape shape target i)]
                        (map f (combo/permuted-combinations nums i))))))))))

(defn -main
  [target & nums]
  ;;  method 3
  (let [target (Integer/parseInt target)
        nums   (map #(Integer/parseInt %) nums)]
    (println "Aiming for" target "with" nums)
    (method-3 target nums))
  ;; method 2
  #_(let [combinations (for [n           (range 1 (inc (count nums)))
                             combination (combo/combinations nums n)]
                         combination)]
      (doall (map #(try-combination-2 target %) combinations)))


  ;;  method 1
  #_(when-not true
    (let [target (Integer/parseInt target)
          nums   (map #(Integer/parseInt %) nums)]
      (println "Aiming for" target "with" nums)
      (let [combinations (for [n           (range 1 (inc (count nums)))
                               combination (combo/combinations nums n)]
                           combination)]
        (doall (pmap #(try-combination target %) combinations)))))
  (shutdown-agents)
  )
