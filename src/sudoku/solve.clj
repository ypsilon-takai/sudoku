(ns sudoku.solve
  (:use [clojure.pprint])
  (:require [clojure.set :as set]))


;;
(defn solved? [board]
  (not-any? #(or (coll? %) (zero? %)) board))


;; utils
(defn get-val [board pos]
  (get board pos))

(defn set-val [board pos num]
  (assoc board pos num))

(defn row-pos-list 
  ([pos] (row-pos-list pos true))
  ([[x y] include-pos?]
     (for [ix (range 9) :when (if include-pos?
                                true
                                (not= ix x))]
       [ix y])))

(defn col-pos-list
  ([pos] (col-pos-list pos true))
  ([[x y] include-pos?]
     (for [iy (range 9) :when (if include-pos?
                                true
                                (not= iy y))]
       [x iy])))
 
(defn box-pos-list
  ([pos] (box-pos-list pos true))
  ([[x y] include-pos?]
     (let [x-list (map #(+ (* 3 (quot x 3)) %) (range 3))
           y-list (map #(+ (* 3 (quot y 3)) %) (range 3))]
       (for [iy y-list ix x-list :when (if include-pos?
                                         true
                                         (not (and (= ix x)
                                                   (= iy y))))]
         [ix iy]))))

;; rule 1
;;  if there is only one possible number, place it there.

(defn candidates [pos board]
  (->> ((juxt row-pos-list col-pos-list box-pos-list) pos true)
       (apply concat ,,)
       (distinct ,,)
       (map #(get-val board %) ,,)
       (remove #(or (coll? %) (zero? %)) ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

(defn step-rule-1 [board]
  (for [y (range 9) x (range 9)]
     (let [num (get-val board [x y])]
       (if (or (set? num)
               (zero? num))
         (let [can (candidates [x y] board)]
           (if (= (count can) 1)
             (first can)
             can))
         num))))
 

;; apply rule
(defn run-rule-1 [board]
  (let [next (step-rule-1 board)]
    (cond (solved? next) {:solved true :board next}
          (= board next) {:solved false :board next}
          :t (do (pprint next)
                 (recur next)))))


 
;; rule-2 
(defn step-rule-2 [board]
  (for [y (range 9) x (range 9)]
    (let [dat (get-num [x y] board)]
      (if (set? num)
        (let )))))
