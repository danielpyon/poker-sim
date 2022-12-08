(ns poker-sim.core
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [poker-sim.ranker :as ranker]
            [clojure.math.combinatorics :as combo]))

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

(defn same-suit? [hand]
  (and (all-cards? hand)
       (apply = (map first hand))))

; utility functions
(defn flatten-once [coll]
  (vec (apply concat coll)))

(defn zip [a b]
  (vec (map vector a b)))

(defn in? [coll elm]
  (not (nil? (some #(= % elm) coll))))

(defrecord GameState [community hands])
(defrecord PlayerStats [win lose tie total])

(defn player-win [stats]
  (let [curr (:win stats)]
    (assoc stats :win (inc curr))))

(defn player-lose [stats]
  (let [curr (:lose stats)]
    (assoc stats :lose (inc curr))))

(defn player-tie [stats]
  (let [curr (:tie stats)]
    (assoc stats :tie (inc curr))))

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

(def game-ranker (ranker/load-ranker))

(defn update-stats
  "Returns new stats given list of hands and indices of winners"
  [stats hands winners] ; note: hands must be the hands BEFORE complete-game
  (let [winning-hands (set (map #(get hands %) winners))]
    (defn update-player [acc curr]
      (if (contains? winning-hands curr)
        (assoc acc curr
               ((if (= 1 (count winners))
                 player-win
                 player-lose)
                (acc curr)))
        (assoc acc curr (player-lose (acc curr)))))
    
    (reduce update-player
            stats
            hands)))

(defn eval-game
  "Evaluate the given GameState and return a map of player hands to PlayerStats."
  [stats known-hands game-state] ; stats is a map from player hands to PlayerStats
  
  (defn indexes-of [e coll]
    (keep-indexed #(if (= e %2) %1) coll))
  
  (defn max-keys
    "Returns a list of x's such that (k x) is maximized"
    [k items]
    (let [scores (map k items) ; apply k to each item
          max-val (apply max scores)]
      (indexes-of max-val scores)))
  
  (defn best-hand
    "Returns the best hand given community cards and a hand"
    [community hand]
    (let [all-cards (into community hand)
          possibilities (combo/combinations all-cards 5)]
      (apply (partial max-key #(game-ranker (set %)))
             possibilities)))
  
  (let [community (:community game-state)
        hands (:hands game-state)
        best-hands (map #(best-hand community %) hands)
        winners (max-keys (comp game-ranker set) ; the indices of winning hands
                          best-hands)]
    (update-stats stats known-hands winners)))

(defn simulate
  ([num-simulations num-players known-hands community-cards stats]
   (if (>= num-simulations 1)
     (recur (dec num-simulations)
            num-players
            known-hands
            community-cards
            (eval-game stats known-hands
                       (complete-game num-players
                                      known-hands
                                      community-cards)))
     
     stats)) ; just return stats if we are done simulating
  
  ([num-simulations num-players known-hands community-cards]
   (let [default-stat (map->PlayerStats {:win 0
                                         :lose 0
                                         :tie 0
                                         :total num-simulations})
         initial-stats (into {} (zip known-hands
                                     (repeat (count known-hands)
                                             default-stat)))]
     (simulate num-simulations
               num-players
               known-hands
               community-cards
               initial-stats))))

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
  (let [n 10000
        p 3
        kh [[[:heart :3] [:diamond :king]] [[:spade :7] [:clover :jack]] [[:heart :ace] [:spade :5]]]
        cc [[:diamond :2] [:spade :queen] [:heart :8]]]
    (apply simulate (check-args n p kh cc))))
