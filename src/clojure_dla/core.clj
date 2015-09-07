(ns clojure-dla.core
  (:require [clojure.core.typed :refer [ann] :as t]))

(t/defalias Coordinate (t/HVec [Integer Integer]))

(t/defalias FixedLocations (t/Seq Coordinate))

(t/defalias LaunchAngle t/Num)

(t/defalias LaunchAngles (t/Seq LaunchAngle))

(t/defalias Direction (t/U ':up ':left ':right ':down))

(t/defalias Directions (t/Seq Direction))

(t/defalias Radius t/Num)

(ann initialise [-> FixedLocations])

(ann spawn [LaunchAngle Radius -> Coordinate])

(ann fix [Coordinate FixedLocations -> FixedLocations])

(ann walk [Coordinate Direction -> Coordinate])

(ann any-fixed-neighbours? [Coordinate FixedLocations -> Boolean])

(ann tick [FixedLocations Directions LaunchAngles -> (t/HVec [FixedLocations Directions LaunchAngles])])
