(ns sudoku.solve)

(def test-board
  [[0 0 3 0 2 0 6 0 0]
   [9 0 0 3 0 5 0 0 1]
   [0 0 1 8 0 6 4 0 0]
   [0 0 8 1 0 2 9 0 0]
   [7 0 0 0 0 0 0 0 8]
   [0 0 6 7 0 8 2 0 0]
   [0 0 2 6 0 9 5 0 0]
   [8 0 0 2 0 3 0 0 9]
   [0 0 5 0 1 0 3 0 0]])
 
(defn low-dat
  ([pos board] (low-dat pos board true))
  ([[x y] board include-xy?]
     (for [ix (range 9) :when (if include-xy?
                                true
                                (not= ix x))]
       (get-in board [y ix]))))
 
(defn col-dat
  ([pos board] (col-dat pos board true))
  ([[x y] board include-xy?]
     (for [iy (range 9) :when (if include-xy?
                                true
                                (not= iy y))]
       (get-in board [iy x]))))
 
(defn box-dat
  ([pos board] (box-dat pos board true))
  ([[x y] board include-xy?]
     (let [x-list (map #(+ (* 3 (quot x 3)) %) (range 3))
           y-list (map #(+ (* 3 (quot y 3)) %) (range 3))]
       (for [iy y-list ix x-list :when (if include-xy?
                                         true
                                         (not (and (= ix x)
                                                   (= iy y))))]
         (get-in board [iy ix])))))
 
 
(defn get-num [[x y] board]
   (nth (nth board y) x))
 
 
(require '[clojure.set :as set])
 
(defn candidates [pos board]
  (->> ((juxt low-dat col-dat box-dat) pos board false)
       (flatten ,,)
       (remove #(or (coll? %) (zero? %)) ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))
 
(defn boardify [raw-data]
  (partition 9 raw-data))
 
(defn solved? [board]
  (not-any? false?
            (map #(and (not-any? coll? %)
                       (not-any? zero? %))
                 board)))
 
 
;; rule 1
;; 
(defn step-rule-1 [board]
  (boardify
   (for [y (range 9) x (range 9)]
     (let [num (get-num [x y] board)]
       (if (or (set? num)
               (zero? num))
         (let [can (candidates [x y] board)]
           (if (= (count can) 1)
             (first can)
             can))
         num)))))
 
(defn step-rule-1 [board]
  (boardify
   (for [y (range 9) x (range 9)]
     (let [num (get-num [x y] board)]
       (if (or (set? num)
               (zero? num))
         (let [can (candidates [x y] board)]
           (if (= (count can) 1)
             (first can)
             num))
        num)))))
 
;; apply rule
;;
 
(defn run-rule-1 [board]
  (let [next (step-rule-1 board)]
    (cond (solved? next) {:solved true :board next}
          (= board next) {:solved false :board next}
          :t (do (print next)
                 (recur next)))))

;(pprint (step-rule-1 (step-rule-1 (step-rule-1 (step-rule-1
;(step-rule-1 (step-rule-1 (step-rule-1 (step-rule-1 (step-rule-1
;(step-rule-1 test-board)))))))))))
 
;; rule-2
(defn step-rule-2 [board]
  (for [y (range 9) x (range 9)]
    (let [dat (get-num [x y] board)]
      (if (set? num)
        (let )))))
