(ns clojure-dla.core
  (:require [clojure.core.typed :refer [ann] :as t]))

(t/defalias Coordinate (t/HVec [Integer Integer]))
(t/defalias Coordinate (t/HVec [t/AnyInteger t/AnyInteger]))

(t/defalias FixedLocations (t/Set Coordinate))

(t/defalias LaunchAngle Double)

(t/defalias LaunchAngles (t/Seq LaunchAngle))

(t/defalias Direction (t/U ':up ':left ':right ':down))

(t/defalias Directions (t/Seq Direction))

(t/defalias Radius Double)


(t/defn initialise
  [] :- (t/Option FixedLocations)
  (set [[0 0]]))

;; discretized circle of specified radius centered around (0,0)
;; just use cos and sin to get x and y coordinate, add 0.5 and apply floor

(t/defn spawn
  [angle :- LaunchAngle
   radius :- Radius] :- Coordinate
   [(int (+ 0.5 (* radius (Math/cos angle))))
    (int (+ 0.5 (* radius (Math/sin angle))))])

(t/defn fix
  [coordinate :- Coordinate
   locations :- FixedLocations] :- FixedLocations
   (conj locations coordinate))

(t/defn walk
  [coordinate :- Coordinate
   direction :- Direction] :- Coordinate
   (case direction
     :up [(first coordinate) (dec (second coordinate))]
     :down [(first coordinate) (inc (second coordinate))]
     :left [(inc (first coordinate)) (second coordinate)]
     :right [(dec (first coordinate)) (second coordinate)]
     :default (throw (IllegalArgumentException. "Invalid direction specified in call to walk function"))))

(t/defn is-fixed?
  [locations :- FixedLocations
   coordinate :- Coordinate] :- Boolean
   (locations coordinate))

;; I'm not sure how to specify the type for using a set as a function
(t/defn any-fixed-neighbours?
  [coordinate :- Coordinate
   locations :- FixedLocations] :- (t/Option Boolean)
   (some (t/fn [location :- Coordinate] :- Boolean
           (or (= location (walk coordinate :up))
               (= location (walk coordinate :down))
               (= location (walk coordinate :left))
               (= location (walk coordinate :right))))
         locations))

(t/defn tick
  "Launches a new particle and walks it until it fixes to the existing lattice."
  [locations :- FixedLocations
   directions :- Directions
   launch-angles :- LaunchAngles] :- (t/HVec [FixedLocations Directions LaunchAngles])
  (if-let [launch-angle (first launch-angles)]
    (if-let [fixed-locations (drop-while (t/ann-form (fn [[location _]] (not (any-fixed-neighbours? location locations)))
                                                     [(t/HVec [Coordinate Directions]) -> Boolean])
                                         (iterate (t/fn [[location directions] :- (t/HVec [Coordinate Directions])] :- (t/HVec [Coordinate Directions])
                                                    (if (seq directions)
                                                      [(walk location (first directions))
                                                       (rest directions)]
                                                      (throw (IllegalStateException. "Empty directions sequence (likely because it has been exhausted."))))
                                                  [(spawn launch-angle 10.0) directions]))]
      (if-let [location-pair (first fixed-locations)]
        (if-let [remaining-directions (second location-pair)]
          (if-let [location (first location-pair)]
            [(fix location locations) directions launch-angles]
            (throw (IllegalStateException. "Illegal location pair passed")))
          (throw (IllegalStateException. "No remaining directions element in vector")))
        (throw (IllegalStateException. "ERROR: Fixed locations has no first element")))
      [locations directions launch-angles])
    (throw (IllegalStateException. "Empty launch angles sequence."))))
