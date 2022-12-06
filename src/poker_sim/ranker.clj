(ns poker-sim.ranker
  (:require [clojure.math.combinatorics :as combo]
            [poker-sim.core :as core]
            [clojure.set :as set]))

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

(defn- rank-increasing
  "Given a vector of card nums in increasing order, returns the rank"
  [cards]
  (defn expt [x n]
    (reduce * (repeat n x)))
  (defn card-pow [idx itm]
    (* itm (expt 13 idx)))
  (reduce + (map-indexed card-pow cards)))

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
  (and (straight? hand)
       (flush? hand)))

(defn- straight-flush
  "Returns the ranking number for the hand given that it is a straight flush."
  [hand]
  (straight hand))

(defn- four-of-a-kind?
  [hand]
  (let [freqs (frequencies (map second hand))]
    (= (sort (vals freqs)) '(1 4))))

(defn- four-of-a-kind
  [hand]
  (let [freqs (frequencies (map second hand))
        inv-freqs (set/map-invert freqs)
        four (card-num-ace-high (inv-freqs 4)) ; the card that was repeated 4 times
        one (card-num-ace-high (inv-freqs 1))]
    (+ (* four 13) one)))

(defn- full-house?
  [hand]
  (let [freqs (frequencies (map second hand))
        vals (vals freqs)]
    (= (sort vals) '(2 3))))

(defn- full-house
  [hand]
  (let [freqs (frequencies (map second hand))
        inv-freqs (set/map-invert freqs)
        three (card-num-ace-high (inv-freqs 3))
        two (card-num-ace-high (inv-freqs 2))]
    (+ (* three 13) two)))

(defn- flush?
  [hand]
  (core/same-suit? hand))
(defn- flush
  [hand]
  (high-card hand))

(defn- three-of-a-kind?
  [hand]
  ; we don't need to check the other 2 cards because we alr know its not a full house
  (let [freqs (frequencies (map second hand))]
    (= (sort (vals freqs)) '(1 1 3))))
(defn- three-of-a-kind
  [hand]
  (let [freqs (frequencies (map second hand))
        inv-freqs (set/map-invert freqs)
        triplet (inv-freqs 3)
        three (card-num-ace-high triplet)
        remaining-cards (remove #(= (second %) triplet) hand)
        remaining (sort (hand-to-nums remaining-cards true))]
    (+ (* three (* 13 13)) (rank-increasing remaining))))

(defn- two-pair?
  [hand]
  (let [freqs (frequencies (map second hand))]
    (= (sort (vals freqs)) '(1 2 2))))
(defn- two-pair
  [hand]
  (defn- rank-inc ; kind of hacky: add an offset to the index (power of 13)
    [cards offset]
    (defn expt [x n]
      (reduce * (repeat n x)))
    (defn card-pow [idx itm]
      (* itm (expt 13 (+ idx offset))))
    (reduce + (map-indexed card-pow cards)))
  
  (let [freqs (frequencies (map second hand))
        inv-freqs (set/map-invert freqs)
        single (inv-freqs 1)
        single-rank (card-num-ace-high single)
        remaining-cards (remove #(= (second %) single) hand)
        remaining (sort (hand-to-nums remaining-cards true))]
    (+ (rank-inc remaining 1) single-rank)))

(defn- one-pair?
  [hand]
  (let [freqs (frequencies (map second hand))]
    (= (sort (vals freqs)) '(1 1 1 2))))
(defn- one-pair
  [hand]
  (let [freqs (frequencies (map second hand))
        inv-freqs (set/map-invert freqs)
        pair (inv-freqs 2)
        two (card-num-ace-high pair)
        remaining-cards (remove #(= (second %) pair) hand)
        remaining (sort (hand-to-nums remaining-cards true))]
    (+ (* two (* 13 13 13)) (rank-increasing remaining))))

(defn- high-card
  [hand]
  (let [ace-high (-> hand
                     (hand-to-nums true)
                     sort)] ; get card ranks in increasing order
    (rank-increasing ace-high)))

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

(defn- generate-rankings []
  (reduce (fn [acc curr] (assoc acc (vec curr) (rank curr)))
          {}
          (combo/combinations core/deck 5)))

(defn- save-ranker!
  "Saves the ranker to a file. Warning: this will take a while..."
  []
  (spit "ranker.edn" (with-out-str (pr (generate-rankings)))))

(defn load-ranker
  "Loads the ranker from disk."
  []
  (read-string (slurp "ranker.edn")))

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


