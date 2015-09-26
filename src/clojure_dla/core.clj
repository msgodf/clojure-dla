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

(t/defn outside-kill-radius?
  [coordinate :- Coordinate
   kill-radius :- Radius]
  (> (Math/sqrt (double (+ (* (first coordinate) (first coordinate))
                           (* (second coordinate) (second coordinate)))))
     kill-radius))

(t/defn step
  "Consume a direction from the provided sequence, apply it to the provided
   location, and return an updated location.

   It takes a vector, because it's written to be used with clojure.core/iterate,
   which takes a single value."
  [[location directions] :- (t/HVec [Coordinate Directions])] :- (t/HVec [Coordinate Directions])
  (if (seq directions)
    [(walk location (first directions))
     (rest directions)]
    (throw (IllegalStateException. "Empty directions sequence (likely because it has been exhausted."))))
(t/defn tick
  "Launches a new particle and walks it until it fixes to the existing lattice."
  [locations :- FixedLocations
   directions :- Directions
   launch-angles :- LaunchAngles] :- (t/HVec [FixedLocations Directions LaunchAngles])
  (if-let [launch-angle (first launch-angles)]
    (if-let [fixed-locations (->> [(spawn launch-angle 10.0) directions]
                                  (iterate step)
                                  (drop-while (t/fn [[location _] :- (t/HVec [Coordinate Directions])] :- Boolean
                                                (not (or (any-fixed-neighbours? location locations)
                                                         (outside-kill-radius? location 15.0))))))]
      (if-let [[location remaining-directions] (first fixed-locations)]
        (if (outside-kill-radius? location 15.0)
          [locations directions (rest launch-angles)]
          [(fix location locations) directions (rest launch-angles)])
        (throw (IllegalStateException. "No output from walk. This shouldn't occur.")))
      [locations directions launch-angles])
    (throw (IllegalStateException. "Empty launch angles sequence."))))
