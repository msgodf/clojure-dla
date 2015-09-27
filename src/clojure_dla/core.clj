(ns clojure-dla.core
  (:require [clojure.core.typed :refer [ann] :as t]))

(t/defalias Coordinate (t/HVec [t/AnyInteger t/AnyInteger]))

(t/defalias FixedLocations (t/Set Coordinate))

(t/defalias LaunchAngle Double)

(t/defalias LaunchAngles (t/Seq LaunchAngle))

(t/defalias Direction (t/U ':up ':left ':right ':down))

(t/defalias Directions (t/Seq Direction))

(t/defalias Radius Double)


(t/defn initialise
  [] :- FixedLocations
  #{[0 0]})

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
  [[x y] :- Coordinate
   direction :- Direction] :- Coordinate
  (case direction
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(inc x) y]
    :right [(dec x) y]
    :default (throw (IllegalArgumentException. "Invalid direction specified in call to walk function"))))

(t/defn is-fixed?
  [locations :- FixedLocations
   coordinate :- Coordinate] :- Boolean
   (locations coordinate))

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
   location, and return an updated location along with the remaining directions.

   It takes a vector, because it's written to be used with clojure.core/iterate,
   which takes a single value."
  [[location directions] :- (t/HVec [Coordinate Directions])] :- (t/HVec [Coordinate Directions])
  (if (seq directions)
    [(walk location (first directions))
     (rest directions)]
    (throw (IllegalStateException. "Empty directions sequence (likely because it has been exhausted."))))

(t/defn keep-stepping
  [locations :- FixedLocations
   kill-radius :- Radius
   [location _] :- (t/HVec [Coordinate Directions])] :- Boolean
   (not (or (any-fixed-neighbours? location locations)
            (outside-kill-radius? location kill-radius))))

(t/defn tick
  "Launches a new particle and walks it until it fixes to the existing lattice."
  [locations :- FixedLocations
   directions :- Directions
   launch-angles :- LaunchAngles] :- (t/HVec [FixedLocations Directions LaunchAngles])
  (let [launch-radius 10.0
        kill-radius 15.0]
    (if-let [launch-angle (first launch-angles)]
      (if-let [fixed-locations (->> [(spawn launch-angle launch-radius) directions]
                                    (iterate step)
                                    (drop-while (partial keep-stepping locations kill-radius)))]
        (if-let [[location remaining-directions] (first fixed-locations)]
          (if (outside-kill-radius? location kill-radius)
            [locations directions (rest launch-angles)]
            [(fix location locations) directions (rest launch-angles)])
          (throw (IllegalStateException. "No output from walk. This shouldn't occur.")))
        [locations directions launch-angles])
      (throw (IllegalStateException. "Empty launch angles sequence.")))))

(t/defn random-direction
  [] :- Direction
  (rand-nth [:up :down :left :right]))

(t/defn random-directions
  [] :- Directions
  (repeatedly random-direction))

(t/ann ^:no-check random-radius [-> Radius])
(defn random-radius
  []
   (rand Math/PI))

(t/defn random-radii
  [] :- (t/Seq Radius)
  (repeatedly random-radius))

(t/defn tick*
  [[locations directions launch-angles] :- (t/HVec [FixedLocations Directions LaunchAngles])] :- (t/HVec [FixedLocations Directions LaunchAngles])
  (tick locations directions launch-angles))

(t/defn run
  [number-of-particles :- Integer] :- (t/Option FixedLocations)
  (when-let [final-state (->> [(initialise) (random-directions) (random-radii)]
                              (iterate tick*)
                              (take number-of-particles)
                              (last))]
    (when final-state
      (first final-state))))

(t/defn display
  [locations :- FixedLocations
   width :- t/AnyInteger
   height :- t/AnyInteger] :- nil
   (doall
    (t/for [y :- t/AnyInteger (range height)] :- nil
           (prn (clojure.string/join ""
                                     (t/for [x :- t/AnyInteger (range width)] :- String
                                            (if (is-fixed? locations [(- x (int (/ width 2)))
                                                                      (- y (int (/ height 2)))])
                                              "#"
                                             " "))))))
   nil)
