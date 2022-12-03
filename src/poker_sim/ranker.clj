(ns poker-sim.ranker
  (:require [clojure.math.combinatorics :as combo]
            [poker-sim.core :as core]))

(def total-combinations 2598960)

; card's number => its actual number
(def card-num-ace-high
  {:2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9 :10 10
   :jack 11 :queen 12 :king 13 :ace 14})
(def card-num-ace-low
  (assoc card-num-ace-high :ace 1))

(defn hand-to-nums
  "Returns a vector of just the numbers in the hand."
  [hand ace-high]
  (if ace-high
    (vec (map (comp card-num-ace-high second) hand))
    (vec (map (comp card-num-ace-low second) hand))))

; each function defines an ordering on the hands given the type
; then we can just multiply by total-combinations

(defn- straight?
  "Returns whether the cards are a straight."
  [hand]
  (defn increasing-by-one?
    "Given a sorted list, returns if the elements are monotonically increasing by exactly one from
     left to right."
    [nums]
    (let [lo (first nums)
          hi (last nums)]
      (= (range lo (inc hi)) nums)))
  
  (let [ace-high (sort (hand-to-nums hand true))
        ace-low (sort (hand-to-nums hand false))]
    (or (increasing-by-one? ace-high)
        (increasing-by-one? ace-low))))

(defn- straight
  [hand]
  ; the base ranking is a multiple of total-combinations
  (let [base-ranking (* total-combinations 8)
        ace-high (sort (hand-to-nums hand true))
        ace-low (sort (hand-to-nums hand false))]
    (+ base-ranking (max (last ace-high) (last ace-low)))))

(defn- straight-flush?
  [hand]
  (and (core/same-suit? hand)
       (straight? hand)))

(defn- straight-flush
  "Returns the ranking number for the hand given that it is a straight flush."
  [hand]
  (straight hand))

(defn- four-of-a-kind?
  [hand]
  ; the frequency map of numbers in the hand should contain a 4
  (core/in? (vals (frequencies (map second hand))) 4))

(defn- four-of-a-kind
  [hand]
  1)

(defn- full-house?
  [hand]
  false)
(defn- full-house
  [hand]
  1)

(defn- flush?
  [hand]
  false)
(defn- flush
  [hand]
  1)

(defn- three-of-a-kind?
  [hand]
  false)
(defn- three-of-a-kind
  [hand]
  1)

(defn- two-pair?
  [hand]
  false)
(defn- two-pair
  [hand]
  1)

(defn- one-pair?
  [hand]
  false)
(defn- one-pair
  [hand]
  1)

(defn- high-card
  [hand]
  1)

(defn- rank [hand]
  (cond (straight-flush? hand) (straight-flush hand)
        (four-of-a-kind? hand) (four-of-a-kind hand)
        (full-house? hand) (full-house hand)
        (flush? hand) (flush hand)
        (straight? hand) (straight hand)
        (three-of-a-kind? hand) (three-of-a-kind hand)
        (two-pair? hand) (two-pair hand)
        (one-pair? hand) (one-pair hand)
        :else (high-card hand)))

(defn- gen-rankings [])

(def rankings (gen-rankings))

;; load hand ranking into memory
;; parallelize?

;; straight flush
;; four of a kind
;; full house
;; flush
;; straight
;; three of a kind
;; two pair
;; one pair
;; high card

; (println (count (combo/combinations core/deck 5)))

