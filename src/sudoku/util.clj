(ns sudoku.util
  (:require [clojure.set :as set]))

(defn create-board
  "Create new board with initial state."
  [init]
  {:board init
   :history []})


(defn solved? [{board :board}]
  (not-any? coll? (vals board)))

(defn status [{board :board}]
  (let [still-as-candidate (filter set? (vals board))]
    (if (empty? still-as-candidate)
      :solved
      (if (some empty? still-as-candidate)
        :fail
        :notyet))))

;; utils
(defn get-val [{board :board} pos]
  (get board pos))

(defn set-board-val [raw-board pos value]
  (assoc raw-board pos value))

(defn set-val
  ([board pos value]
   (set-val board pos value nil))
  ([board pos value rule]
   {:board (set-board-val (:board board) pos value)
    :history (conj (:history board)
                   {:board (:board board)
                    :rule rule})}))

(defn take-away
  ([board pos num]
   (take-away board pos num nil))
  ([board pos num rule]
   {:board (update-in (:board board) [pos] disj ,, num)
    :history (conj (:history board)
                   {:board (:board board)
                    :rule rule})}))

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

(defn neibor-pos-list [pos]
  "Get same low, same col and same box posisions."
  (->> ((juxt row-pos-list col-pos-list box-pos-list) pos false)
       (apply concat ,,)
       (distinct ,,)))

;; filter
(defn non-fixed-pos
  ([board] (non-fixed-pos board (keys (:board board))))
  ([board pos-list]
     (->> pos-list
          (filter #(set? (get-val board %)) ,,))))

(defn fixed-pos
  ([board] (fixed-pos board (keys (:board board))))
  ([board pos-list]
     (->> pos-list
          (filter #(number? (get-val board %)) ,,))))


;; create candidate list
(defn candidates [pos board]
  (->> (neibor-pos-list pos)
       (map #(get-val board %) ,,)
       (remove coll? ,,)
       (set ,,)
       (set/difference (set (range 1 10)) ,,)))

(defn update-candidate
  ([board]
   (update-candidate board nil))
  ([board rule]
   (let [updater (fn [pos]
                   (let [num (get-val board pos)]
                     (if (set? num) 
                       (candidates pos board)
                       num)))
         new-board (reduce #(set-board-val %1 %2 (updater %2))
                           (:board board)
                           (non-fixed-pos board))]
     {:board new-board
      :history (conj (:history board)
                     {:board (:board board)
                      :rule rule})})))


;; remove num from candidates
(defn remove-num
  ([board num pos-list]
   (remove-num board num pos-list nil))
  ([board num pos-list rule]
   (reduce #(take-away %1 %2 num rule)
           board
           (non-fixed-pos board pos-list))))

