(ns brain)
(require 'clojure.set)

(defn regions
    [state]
        (vals (:regions state)))

(defn ours?
    [region]
    (= :us (:owner region)))

(defn not_ours?
  [region]
  (not (= :us (:owner region))))

(defn enemy?
    [region]
    (= :them (:owner region)))

(defn neutral?
    [region]
    (= :neutral (:owner region)))

;(defn uncaptured_super?
;  [state super_region_id regions_by_super]
;  (> (get-in state [:super_regions (key (first regions_by_super)) :region_count]) (count (val (first regions_by_super)) )))

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
(defn super_regions_with_capture_count
  [state regions_group_super]
  (get-in
    (reduce
      (fn
        [state [region_group_key region_group_val]]
        (assoc-in state [:super_regions region_group_key :region_capture_count] (count region_group_val)))
      state regions_group_super)
    [:super_regions]))

(defn super_captured?
  [super_region]
  (or
    (not (contains? super_region :region_capture_count))
    (not= (get-in super_region [:region_count]) (get-in super_region [:region_capture_count]))))

; Find the super region that is the highest priority. This is the region with the lowest
; score that has not been captured yet.
(defn place_armies
    [state]
    (let [regions   (filter ours? (regions state))
          super_region_id_neighbors (set (map :super_region_id (flatten (map (fn [region] (neighbours state region)) regions))))
          regions_by_super (group-by :super_region_id regions)
          super_regions_with_capture (super_regions_with_capture_count state regions_by_super)
          super_regions_to_capture (sort-by :score < (filter (fn [sr] (get-in super_region_id_neighbors [(:id sr)])) (filter super_captured? (vals super_regions_with_capture))))
          regions_to_capture (filter (fn [region] (and (not (ours? region)) (= (:id (first super_regions_to_capture)) (:super_region_id region)))) (vals (:regions state)))
          regions_ids_capture_with (clojure.set/intersection (set (map :id regions)) (set (flatten (map :neighbours regions_to_capture))))
          region    (first (filter (fn [region] (= (first regions_ids_capture_with) (:id region))) regions))
          placement {:region region :armies (:starting_armies state)}]
        [placement]))

(defn random_movement
    [state region]
    (let [neighbours  (neighbours state region)
          enemy_neighbours (filter not_ours? neighbours)
          destination (if (empty? enemy_neighbours)
                        (rand-nth neighbours)
                        (first (sort (fn [r1 r2] (compare ((super_region state r1) :score) ((super_region state r2) :score))) enemy_neighbours)))
          armies      (dec (:armies region))
          movement    {:from region :to destination :armies armies}]
        movement))

(defn biggest_neighbour
  [regions region]
    (let [neighbours (filter (fn [r] (some (fn [id] (= id (:id r))) (:neighbours region))) regions)]
       (if (empty? neighbours)
         {}
         (first (sort-by :armies > neighbours))))
  )

(defn biggest_neighbour_armies
  [state region]
  (let [regions (filter not_ours? (regions state))
        neighbour (biggest_neighbour regions region)]
    (if (empty? neighbour)
      0
      (:armies neighbour)
      ))
  )

(defn attack
    [state] 
    (->> (regions state)
         (filter ours?)
         (filter (fn [region] (> (:armies region) 1)))
         (filter (fn [region] (>
                               (:armies region)
                               (+ 1 (biggest_neighbour_armies
                                      state
                                      region)))))
        (map (partial random_movement state))))
