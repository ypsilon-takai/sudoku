(ns sudoku.solve
  (:use [clojure.pprint])
  (:require [clojure.set :as set]))


;;
(defn solved? [board]
  (not-any? #(or (coll? %) (zero? %)) board))


;; utils
(defn get-num [board pos]
  (get board pos))

(defn set-num [board pos num]
  (assoc board pos num))


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
 


;; rule 1
;;  if there is only one possible number, place it there.


(defn candidates [pos board]
  (->> ((juxt low-dat col-dat box-dat) pos board true)
       (flatten ,,)
       (remove #(or (coll? %) (zero? %)) ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

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
