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

(defn compare_owner
  [o1 o2]
  (cond
    (= o1 o2) 0
    (= :them o1) 1
    (= :them o2) -1
    (= :neutral o1) 1
    (= :neutral o2) -1
    :else 0
    )
  )

(defn compare_regions
  [r1 r2 state]
  (let [sr1 (super_region state r1)
        sr2 (super_region state r2)
        sr1_score (:score sr1)
        sr2_score (:score sr2)]
    (if (= sr1_score sr2_score)
      (if (= (:owner r1) (:owner r2))
        (compare (:id sr1) (:id sr2))
        (compare_owner (:owner r2) (:owner r1)))
      (compare sr2_score sr1_score))
    )
  )

;; ----- picking regions
(defn pick_starting_region
  [state ids]
  (let [regions (select-keys (state :regions) ids)]
    (:id (val (first (sort (fn [r1 r2]
                             (compare_regions (val r1) (val r2) state))
                           regions)
                     )))
    )
  )

;; ----- placement and attacking
(defn super_regions_with_capture_count
  [state]
  (get-in
    state
    [:super_regions]))

(defn super_captured?
  [super_region]
  (or
    (not (contains? super_region :region_capture_count))
    (not= (get-in super_region [:region_count]) (get-in super_region [:region_capture_count]))))

(defn needs_armies?
  [state region_id]
  (let [region (get-in state [:regions region_id])
        armies (:armies region)]
    (some
      (fn [neighbour] (or
                        (and (enemy? neighbour) (<= (- armies (:armies neighbour)) 8))
                        (and (neutral? neighbour) (<= (- armies (:armies neighbour)) 3))
                        ))
      (neighbours state region)))
  )

(defn needed_armies
  [neighbours region]
  (apply max (conj (map (fn [neighbour]
                          (case (:owner neighbour)
                            :them (+ (- (:armies neighbour) (:armies region)) 8)
                            :neutral (+ (- (:armies neighbour) (:armies region)) 3)
                            :us 0))
                        neighbours) 0)))

(defn create_placements
  [state region_ids armies]
  (let [region (get-in state [:regions (first region_ids)])
        remaining_region_ids (rest region_ids)
        placement_armies (needed_armies (neighbours state region) region)
        remaining_armies (- armies placement_armies)]
    (if (< remaining_armies 0)
      [{:region region :armies armies}]
      (if (empty? remaining_region_ids)
        [{:region region :armies armies}]
        (if (> placement_armies 0)
          (conj (create_placements state remaining_region_ids remaining_armies) {:region region :armies placement_armies})
          (create_placements state remaining_region_ids remaining_armies))
        )))
  )


(defn build_placements
  [state region_ids armies]
  (create_placements state region_ids armies)
  )

; Find the super region that is the highest priority. This is the region with the highest
; score that has not been captured yet.
(defn place_armies
  [state]
  (let [regions (filter ours? (regions state))
        super_region_id_neighbors (set (map :super_region_id (flatten (map (fn [region] (neighbours state region)) regions))))
        super_regions_with_capture (super_regions_with_capture_count state)
        super_regions_to_capture (sort-by :score > (filter (fn [sr] (get-in super_region_id_neighbors [(:id sr)])) (filter super_captured? (vals super_regions_with_capture))))
        regions_to_capture (filter
                             (fn [region] (and
                                            (not (ours? region))
                                            (some #(= (:super_region_id region) %) (map :id super_regions_to_capture))))
                             (vals (:regions state)))
        sorted_regions_to_capture (sort (fn [r1 r2] (compare ((super_region state r2) :score) ((super_region state r1) :score))) regions_to_capture)
        region_ids_capture_with (filter (fn [rid] (ours? (get-in state [:regions rid]))) (distinct (flatten (map :neighbours sorted_regions_to_capture))))
        region_ids_need_armies (filter (fn [rid] (needs_armies? state rid)) region_ids_capture_with)
        placements (build_placements state
                                     (if (not (empty? region_ids_need_armies))
                                       region_ids_need_armies
                                       region_ids_capture_with)
                                     (:starting_armies state))
        ]
    placements))

(defn region_movement
  [state region]
  (let [neighbours (neighbours state region)
        enemy_neighbours (filter not_ours? neighbours)
        destination (if (empty? enemy_neighbours)
                      (rand-nth neighbours)
                      (first
                        (sort
                          (fn [r1 r2] (compare_regions r1 r2 state))
                          (shuffle enemy_neighbours))))
        armies (if (> (count enemy_neighbours) 1)
                 (min (dec (:armies region)) (max (+ 10 (destination :armies)) (/ (:armies region) 2)))
                 (dec (:armies region)))
        movement {:from region :to destination :armies armies}]
    movement))

(defn biggest_enemy_neighbour
  [regions region]
  (let [neighbours (filter (fn [r] (some (fn [id] (= id (:id r))) (:neighbours region))) regions)]
    (if (empty? neighbours)
      {}
      (first (filter enemy? (sort-by :armies > neighbours)))))
  )

(defn biggest_enemy_neighbour_armies
  [state region]
  (let [regions (filter not_ours? (regions state))
        neighbour (biggest_enemy_neighbour regions region)]
    (if (empty? neighbour)
      0
      (:armies neighbour)
      ))
  )

(defn attack
  [state]
  (->> (regions state)
       (filter ours?)
       (filter (fn [region] (> (:armies region) 2)))
       (filter (fn [region] (>
                              (:armies region)
                              (+ 1 (biggest_enemy_neighbour_armies
                                     state
                                     region)))))
       (map (partial region_movement state))
       (filter (fn [move]
                 (<
                   (:armies (:to move))
                   (:armies move))))))

(defn update_super_region_capture_count
  [state]
  (let [grouped_regions (group-by :super_region_id (filter ours? (regions state)))]
    (reduce
      (fn
        [state super_region_id]
        (assoc-in state [:super_regions super_region_id :region_capture_count] (if (= 0 (count grouped_regions)) 0 (count (grouped_regions super_region_id)))))
      state (keys (:super_regions state))))
  )

(defn update_super_region_scores
  [state]
  (reduce (fn
            [state super_region_id]
            (let [reward (get-in state [:super_regions super_region_id :reward])
                  capture_count (get-in state [:super_regions super_region_id :region_capture_count])
                  region_count (get-in state [:super_regions super_region_id :region_count])
                  contains_wasteland (contains? (get-in state [:super_regions super_region_id]) :wasteland)
                  uncaptured_regions (- region_count capture_count)]
              (assoc-in state
                        [:super_regions super_region_id :score]
                        (if (= 0 reward)
                          -10
                          (if (= capture_count region_count)
                            reward
                            (- (- (/ reward uncaptured_regions)
                                  (if contains_wasteland 0.5 0))
                               (* 0.05 uncaptured_regions)))
                          )
                        ))
            )
          state (keys (:super_regions state)))
  )

(defn round_update
  [state]
  (-> state
      (update_super_region_capture_count)
      (update_super_region_scores)))

