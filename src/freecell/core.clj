(ns freecell.core
  (:gen-class)
  ;;(:require [clojure.math.combinatorics :as c])
  )

"(defn shuffle-cards
	[n]
	(c/nth-permutation (take 52 (iterate #(+ 1 %) 0)) n))"

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn init-board
  [cards]
  (assoc {:freecells [55 55 55 55] :aces [55 55 55 55]} :field
         (zipmap (range 8) (for [n (range 8)]
                             (map #(nth % n) (partition 8 8 (repeat 10 55) cards))))))

(defn card-suit
  [c]
  (quot c 13))

(defn card-val
  [c]
  (rem c 13))

(defn num2card
  [n]
  (let [suits [\u2667 \u2664 \u2665 \u2666]
        val [ " T " " 2 " " 3 " " 4 " " 5 " " 6 " " 7 " " 8 " " 9 " "10 " " V " " D " " K "]]
    (if (or (< n 0) (> n 51))
      "    "
      (str (nth suits (card-suit n)) (nth val (card-val n))))))

(defn print-board
  [board]
  (let [field (board :field)
        max-col (reduce max (map count (map field (range 8))))]
    (println (map num2card (board :freecells)) (map num2card (board :aces)) "\n")
    (for [n (range max-col)]
      (do
        ;;(println (field n))
        (println (map #(num2card (nth % n 55)) (map field (range 8)))))))) ;; печатает не в том направлении - надо перевернуть на 90 градусов


(defn can-put-over?
  [c1 c2] ;;c1 put over c2
  (let [allowed-suits [false false true true
                       false false true true
                       true true false false
                       true true false false]
        suit-index (+ (card-suit c1) (* (card-suit c2) 4))]
    (do (println (num2card c1) (num2card c2))
        (and (= 1 (- (card-val c2) (card-val c1))) (nth allowed-suits suit-index)))))

(defn move-board  ;; надо нумеровать колонки в map { 1 [], 2 [], ...}
  [board e2 e4]
  (let [field (get board :field)
        col1 (vec (take-while #(> 52 %) (field e2)))
        col2 (vec (take-while #(> 52 %) (field e4)))
        c1 (last col1)
        c2 (last col2)]
    (println col1 col2)
    (if (can-put-over? c1 c2)
      (let [new-col1 (take (dec (count col1)) col1)
            new-col2 (conj col2 c1)]
        (println new-col1 new-col2)
        (assoc board :field (assoc field e2 new-col1, e4 new-col2))) ;; не будет работать с пустыми колонками - заменить на assoc
      board)))

(defn random-board
  []
  (loop [x 52 board [] cards (range 52)]
    (if (<= x 0)
      board
      (do
        (let [n (nth cards (rand-int (count cards)))]
          (recur (dec x)
                 (conj  board n)
                 (remove #(= n %) cards)))))))
(defn some-func
  []
  (println "something"))

