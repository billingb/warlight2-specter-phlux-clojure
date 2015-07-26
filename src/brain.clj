(ns brain)
(require 'clojure.set)

(defn regions
    [state]
        (vals (:regions state)))

(defn ours?
    [region]
    (= :us (:owner region)))

(defn enemy?
    [region]
    (= :them (:owner region)))

(defn neutral?
    [region]
    (= :neutral (:owner region)))

(defn neighbours
    [state region]
    (map (fn [region_id] (get-in state [:regions region_id])) (:neighbours region)))

(defn super_region
    [state region]
    (get-in state [:super_regions (:super_region_id region)]))

;; ----- picking regions

(defn pick_starting_region
    [state ids]
    (let [regions (select-keys (state :regions) ids)]
        (:id (val (first (sort (fn [r1 r2]
                                   (let [sr1 (super_region state (val r1))
                                         sr2 (super_region state (val r2))
                                         sr1_score (:score sr1)
                                         sr2_score (:score sr2)]
                                       (if (= sr1_score sr2_score)
                                           (compare (:id sr1) (:id sr2))
                                           (compare sr1_score sr2_score))
                                       ))
                               regions)
                         )))
        )
    )

;; ----- placement and attacking

(defn place_armies
    [state]
    (let [regions   (filter ours? (regions state))
          region    (rand-nth regions)
          placement {:region region :armies (:starting_armies state)}]
        [placement]))

(defn random_movement
    [state region]
    (let [neighbours  (neighbours state region)
          destination (rand-nth neighbours)
          armies      (dec (:armies region))
          movement    {:from region :to destination :armies armies}]
        movement))

(defn attack
    [state] 
    (->> (regions state)
        (filter ours?)
        (filter (fn [region] (> (:armies region) 1)))
        (map (partial random_movement state))))
