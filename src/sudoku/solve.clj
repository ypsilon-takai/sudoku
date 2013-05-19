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
(defn fix-the-number
  "rule 1
     if there is only one possible number, place it."
  [board pos]
  (let [val-at-pos (first (get-val board pos))
        neibours (neibour-pos-list pos )]
    (reduce (fn [b p]
              (if (set? (get-val b p))
                (take-away b p val-at-pos)
                b))
            (set-val board pos val-at-pos)
            neibours)))

(defn apply-rule-1 [board]
  (let [fixing-pos (->> (non-fixed-pos board)
                        (filter #(= (count (get-val board %)) 1) ,,))]
    (if (empty? fixing-pos)
      board
      (recur (reduce fix-the-number board fixing-pos)))))


;; rule 2
(defn find-only-one
  "rule 2
     if there is only one cell which number n is in candidates, the
     cell's number is it."
  [board pos]
  (let [row-vals (map #(get-val board %) (row-pos-list pos false))
        col-vals (map #(get-val board %) (col-pos-list pos false))
        box-vals (map #(get-val board %) (box-pos-list pos false))]
    ))

(defn find-only-one
    "rule 2
     if there is only one cell which number n is in candidates, the
     cell's number is it."
    [board pos neibour-func]
    (let [not-in-neigbour (->> (neibour-func pos false)
                               (map #(get-val board %) ,,)
                               (filter set? ,,)
                               (reduce clojure.set/union ,,)
                               (clojure.set/difference (get-val board pos) ,,))]
      (if (empty? not-in-neigbour)
        board
        (reduce #(take-away %1 %2 (first not-in-neigbour)) board (neibour-func pos false)))))
