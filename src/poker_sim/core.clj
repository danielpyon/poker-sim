(ns poker-sim.core
  (:gen-class)
  (:require [clojure.string :as string])
  (:require [clojure.set :as set]))

; card functions
(def deck
  #{[:clover :ace] [:clover :2] [:clover :3] [:clover :4] [:clover :5] [:clover :6] [:clover :7] [:clover :8] [:clover :9] [:clover :10] [:clover :jack] [:clover :queen] [:clover :king]
   [:spade :ace] [:spade :2] [:spade :3] [:spade :4] [:spade :5] [:spade :6] [:spade :7] [:spade :8] [:spade :9] [:spade :10] [:spade :jack] [:spade :queen] [:spade :king]
   [:diamond :ace] [:diamond :2] [:diamond :3] [:diamond :4] [:diamond :5] [:diamond :6] [:diamond :7] [:diamond :8] [:diamond :9] [:diamond :10] [:diamond :jack] [:diamond :queen] [:diamond :king]
   [:heart :ace] [:heart :2] [:heart :3] [:heart :4] [:heart :5] [:heart :6] [:heart :7] [:heart :8] [:heart :9] [:heart :10] [:heart :jack] [:heart :queen] [:heart :king]})

(defn shuffle-cards
  "Returns a random permutation of the given cards."
  [cards]
  (vec (shuffle cards)))

(defn remove-cards [deck cards]
  "Removes a set of cards from a deck of cards."
  (set/difference deck cards))

(defn drop-cards [cards n]
  "Returns everything after the first n cards."
  (vec (drop n cards)))

(defn take-cards [cards n]
  "Returns the first n cards."
  (vec (take n cards)))

(defn split-cards [cards n]
  "Splits the cards into the first n and the rest. First n cards are stored in the first element of
   the returned vector. Rest of the cards are stored in the second element."
  [(take-cards cards n) (drop-cards cards n)])

(defn combine-hands [& hands]
  "Given two hands (each a vector of cards), returns the combined hand."
  (vec (apply concat hands)))

(defn partition-cards [cards n]
  "Partition the cards into groups of n."
  (vec (map vec (partition n cards))))

(defn card? [item]
  (contains? deck item))

(defn all-cards? [coll]
  (every? card? coll))

; utility functions
(defn flatten-once [coll]
  (vec (apply concat coll)))

(defn zip [a b]
  (vec (map vector a b)))

(defrecord GameState [community hands])
(defrecord PlayerStats [win lose tie total])

(defn complete-game
  "Completes the game by filling in remaining cards with random ones. Returns a map with key
   :community for community cards, and :hands for the player hands."
  [num-players known-hands community-cards]
  
  (defn complete-community
    "Completes the community cards given the shuffled cards and the known community cards. Returns
     a vector whose first element are the complete community cards and whose second element are the
     remaining cards available."
    [cards community]
    (let [required (- 5 (count community)) ; the number of cards required
          [rest deck] (split-cards cards required)] ; the rest of the community cards & the remaining deck 
      [(combine-hands community rest) deck])) ; return complete community cards and the rest of the deck
  
  (defn complete-two
    "Completes the player hands that need two cards. Returns a vector whose first element are the
     complete player hands (for players that needed two cards) and whose second element are the
     remaining cards available."
    [cards hands players] ; cards: cards available, hands: known player hands, players: number of players total
    (let [required (- players (count (filter #(>= (count %) 1) hands)))
          completed (take-cards (partition-cards cards 2) required)] ; take the required number of pairs of cards
      [completed (drop-cards cards (* required 2))])) ; we took required*2 cards
  
  (defn complete-one
    [cards hands]
    (let [required (filter #(= (count %) 1) hands)
          num-required (count required)
          completed (-> cards
                        (partition-cards 1)
                        flatten-once
                        (take-cards num-required)
                        (zip (flatten-once required)))]
      [completed (drop-cards cards num-required)]))
  
  (defn complete-zero
    [hands]
    (vec (filter #(= (count %) 2) hands)))
  
  (defn cards-in-use [known-hands community-cards]
    (reduce combine-hands community-cards known-hands))
  
  (let [cards-initial (->> (cards-in-use known-hands community-cards)
                           (remove-cards deck)
                           shuffle-cards)
        
        [community cards-after-community] (complete-community cards-initial community-cards)
        [two cards-after-two] (complete-two cards-after-community known-hands num-players)
        [one _] (complete-one cards-after-two known-hands)
        zero (complete-zero known-hands)
        
        hands (combine-hands zero one two)]
    
    (map->GameState {:community community
                     :hands hands})))

(defn eval-game
  "Evaluate the given GameState and return a map of player hands to PlayerStats."
  [stats game-state] ; states is a map from player hands to PlayerStats
  )

(defn simulate
  ([num-simulations num-players known-hands community-cards stats]
   (if (>= num-simulations 1)
     (recur (dec num-simulations)
            num-players
            known-hands
            community-cards
            (eval-game stats (complete-game num-players
                                            known-hands
                                            community-cards)))
     
     stats)) ; just return stats if we are done simulating
  
  ([num-simulations num-players known-hands community-cards]
   (simulate num-simulations
             num-players 
             known-hands
             community-cards 
             {})))

(defn check-args
  [num-simulations num-players known-hands community-cards]
     (assert (and (number? num-simulations) (number? num-players)))
     (assert (<= (count known-hands) num-players)) 
     (assert (<= (count community-cards) 5))
     
     (assert (every? #(<= (count %) 2) known-hands))
     (assert (every? all-cards? known-hands))
     
     (assert (apply distinct? (concat (flatten-once known-hands)
                                      community-cards)))
     
     (assert (all-cards? community-cards))
     [num-simulations num-players known-hands community-cards])

(defn -main
  [& args]
  (let [n 100
        p 5
        kh [[[:heart :3] [:diamond :king]] [[:spade :7] [:clover :jack]] [[:heart :ace]]]
        cc [[:diamond :2] [:spade :queen] [:heart :8]]]
    (apply simulate (check-args n p kh cc))))
