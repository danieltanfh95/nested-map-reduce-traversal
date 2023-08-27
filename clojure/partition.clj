
;; https://github.com/josevalim/nested-map-reduce-traversal/tree/master
;; the problem with this question is that it assumes a third level of grouping (groups of sections)
;; but uses a boolean flag for it
(defn assign-sequential-id [list-of-items]
  (map-indexed (fn [i item] (assoc item :position (inc i))) list-of-items))

(defn process [sections]
  (->> sections
       ;; first we mark each section as having its own id
       (assign-sequential-id)
       ;; group the sections by the flag
       (partition-by :reset_lesson_position)
       ;; assign each lesson a unique id within the section group
       (mapcat (fn [section-group]
                 (let [all-lessons (->> section-group
                                        ;; flatten the lessons within a section group as a list
                                        (mapcat (fn [{:keys [lessons position]}] (map #(assoc % :section position) lessons)))
                                        (assign-sequential-id)
                                        ;; collect them by section
                                        (group-by :section)
                                        )]
                   (map (fn [sections]
                          ;; replace the lessons with the pre-processed ones and remove unneeded keys
                          (assoc sections :lessons (->> (get all-lessons (:position sections))
                                                        (map #(dissoc % :section)))))
                        section-group))
                 ))
       (into [])))

(deftest test-sections
  (println (process [{:title                 "Getting started"
                      :reset_lesson_position false
                      :lessons               [{:name "Welcome"}
                                              {:name "Installation"}]}
                     {:title                 "Basic operator"
                      :reset_lesson_position false
                      :lessons               [{:name "Addition / Subtraction"}
                                              {:name "Multiplication / Division"}]}
                     {:title                 "Advanced topics"
                      :reset_lesson_position true
                      :lessons               [{:name "Mutability"}
                                              {:name "Immutability"}]}]))
  (is (= (process [{:title                 "Getting started"
                    :reset_lesson_position false
                    :lessons               [{:name "Welcome"}
                                            {:name "Installation"}]}
                   {:title                 "Basic operator"
                    :reset_lesson_position false
                    :lessons               [{:name "Addition / Subtraction"}
                                            {:name "Multiplication / Division"}]}
                   {:title                 "Advanced topics"
                    :reset_lesson_position true
                    :lessons               [{:name "Mutability"}
                                            {:name "Immutability"}]}])
         [{:title                 "Getting started"
           :reset_lesson_position false,
           :lessons               [{:name "Welcome" :position 1}
                                   {:name "Installation" :position 2}],
           :position              1}
          {:title                 "Basic operator"
           :reset_lesson_position false,
           :lessons               [{:name "Addition / Subtraction" :position 3}
                                   {:name "Multiplication / Division" :position 4}],
           :position              2}
          {:title                 "Advanced topics"
           :reset_lesson_position true,
           :lessons               [{:name "Mutability" :position 1}
                                   {:name "Immutability" :position 2}],
           :position              3}])
