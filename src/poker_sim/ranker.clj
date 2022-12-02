(ns poker-sim.ranker
  (:require [clojure.math.combinatorics :as combo]
            [poker-sim.core :as core]))

(def total-combinations 2598960)

(defn- straight-flush?
  [hand]
  false)

; each function defines an ordering on the hands given the type
; then we can just multiply by total-combinations
(defn- straight-flush
  "Returns the ranking number for the hand given that it is a straight flush."
  [hand]
  false)

(defn- one-pair?
  [hand]
  ())

(defn- rank [hand]
  (cond (straight-flush? hand) (straight-flush hand)
        (four-of-a-kind? hand) (four-of-a-kind)
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

