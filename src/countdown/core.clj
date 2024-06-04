(ns countdown.core
  (:gen-class))


(defn evaluate
  "Returns distance to target"
  [target tree]
  (println "Evaluating tree" tree)
  (let [value (eval tree)]
    {:tree tree
     :score (abs (- target value))
     :value value}))

(defn eval-all
  [target trees]
  (sort-by :score
           (for [t trees]
             (evaluate target t))))

(defn gen-trees
  [nums]
  ['(+ 5 3)])


(defn -main
  "I don't do a whole lot ... yet."
  [target & nums]
  (let [target (Integer/parseInt target)
        trees (gen-trees nums)]
    (println "Aiming for" target "with" nums)
    (println "Result" (first (eval-all target trees))))
;  (+ 5 (* 3 7))
  )
