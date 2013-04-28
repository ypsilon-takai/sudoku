(ns sudoku.solve
  (:use [clojure.pprint]
        [sudoku.util])
  (:require [clojure.set :as set]))


;;
(defn solved? [board]
  (not-any? #(or (coll? %) (zero? %)) (vals board)))


;; utils
(defn get-val [board pos]
  (get board pos))

(defn set-val [board pos value]
  (assoc board pos value))

(defn take-away [board pos num]
  (set-val board pos (disj (get-val board pos) num)))

;; positions
(defn row-pos-list
  "Get same low positions with the point."
  ([pos] (row-pos-list pos true))
  ([[x y] include-pos?]
     (for [ix (range 9) :when (if include-pos?
                                true
                                (not= ix x))]
       [ix y])))

(defn col-pos-list
  "Get same column positions with the point."
  ([pos] (col-pos-list pos true))
  ([[x y] include-pos?]
     (for [iy (range 9) :when (if include-pos?
                                true
                                (not= iy y))]
       [x iy])))
 
(defn box-pos-list
  "Get positions which in the same box with the point."
  ([pos] (box-pos-list pos true))
  ([[x y] include-pos?]
     (let [x-list (map #(+ (* 3 (quot x 3)) %) (range 3))
           y-list (map #(+ (* 3 (quot y 3)) %) (range 3))]
       (for [iy y-list ix x-list :when (if include-pos?
                                         true
                                         (not (and (= ix x)
                                                   (= iy y))))]
         [ix iy]))))

(defn neibour-pos-list [pos]
  "Get same low, same col, same box posisions."
  (->> ((juxt row-pos-list col-pos-list box-pos-list) pos false)
       (apply concat ,,)
       (distinct ,,)))

;; filter
(defn non-fixed-pos
  ([board] (non-fixed-pos board (keys board)))
  ([board pos-list]
     (->> pos-list
          (filter #(set? (get-val board %)) ,,))))

(defn fixed-pos
  ([board] (fixed-pos board (keys board)))
  ([board pos-list]
     (->> pos-list
          (filter #(number? (get-val board %)) ,,))))


;; create candidate list
(defn candidates [pos board]
  (->> (neibour-pos-list pos)
       (map #(get-val board %) ,,)
       (remove #(or (coll? %) (zero? %)) ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

(defn update-candidate [board]
  (let [updater (fn [pos]
                  (let [num (get-val board pos)]
                    (if (or (set? num)
                            (zero? num))
                      (candidates pos board)
                      num)))]
    (reduce #(set-val %1 %2 (updater %2)) board (non-fixed-pos board))))


;; rule 1
;;  if there is only one possible number, place it there.
(defn rule-1 [board pos]
  (let [val-at-pos (firrst (get-val board pos))
        neibour-pos (neibour-pos-list pos )]
    (->> (set-val board pos val-at-pos)
         (reduce (fn [board pos val-delete]
                   )))))

(defn apply-rule-1 [board]
  (loop [board board
         new-fixed-pos (->> (non-fixed-pos board)
                            (filter #(= (count (get-val board %)) 1) ,,))]
))


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
