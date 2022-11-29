(ns poker-sim.core
  (:gen-class)
  (:require [clojure.string :as string]))

(def deck
  #{[:clover :ace] [:clover :2] [:clover :3] [:clover :4] [:clover :5] [:clover :6] [:clover :7] [:clover :8] [:clover :9] [:clover :10] [:clover :jack] [:clover :queen] [:clover :king]
   [:spade :ace] [:spade :2] [:spade :3] [:spade :4] [:spade :5] [:spade :6] [:spade :7] [:spade :8] [:spade :9] [:spade :10] [:spade :jack] [:spade :queen] [:spade :king]
   [:diamond :ace] [:diamond :2] [:diamond :3] [:diamond :4] [:diamond :5] [:diamond :6] [:diamond :7] [:diamond :8] [:diamond :9] [:diamond :10] [:diamond :jack] [:diamond :queen] [:diamond :king]
   [:heart :ace] [:heart :2] [:heart :3] [:heart :4] [:heart :5] [:heart :6] [:heart :7] [:heart :8] [:heart :9] [:heart :10] [:heart :jack] [:heart :queen] [:heart :king]})

(defn card? [item]
  (contains? deck item))

(defn all-cards? [coll]
  (every? card? coll))

(defn complete-game
  [known-hands community-cards]
  (letfn [()]
    (let [])))

; (complete-game [[] []] [[:clover :ace] [:heart 3]])

; remove known cards
; shuffle cards
; take the required amount

(defn simulate
  [num-simulations num-players known-hands community-cards]
  [])

(defn check-args
  [args]
  (let [params
        ((comp vec concat) [(int (first args)) (int (second args))]
                           (load-string (str "[" (string/join (drop 2 args)) "]")))]
    (assert (= 4 (count params)))
    (let [num-simulations (first params)
          num-players (second params)
          known-hands (get params 2)
          community-cards (get params 3)]
      (assert (and (number? num-simulations) (number? num-players)))
      (assert (<= (count known-hands) num-players)) 
      (assert (<= (count community-cards) 5))
      
      (assert (every? #(<= 2 (count %)) known-hands))
      (assert (every? all-cards? known-hands))
      
      (assert (apply distinct? (concat
                                (apply concat known-hands)
                                community-cards)))
      
      (assert (all-cards? community-cards))
      params)))

(defn -main 
  [& args] 
  (try (println (apply simulate (check-args args)))
       (catch Exception e (println e) "Usage: ./core.clj <n> <known hands> <community cards>")))
