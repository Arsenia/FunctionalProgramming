(ns lr1.core
  (:gen-class)
  (:require [clojure.test :refer :all])
  
  )
(use 'clojure.java.io)

(def Ra 4)
(def Rb (* 1.5 Ra))
(def EHigh 0.5)
(def ELow 0.15)

(defn not-nil?
      [element]
      (not (nil? element))
)

(defn compare-desc 
      [a b]
      (compare (:potential b) (:potential a))
)

;convert class object string property to number
(defn parse-class-object-property
      [str]
      (if (re-find #"^-?\d+\.?\d*$" str)
        (read-string str)
      )
)

;read class object properties from the class object string
(defn parse-class-object-string
      [inputStr]
      (->> (clojure.string/split inputStr #",")
           (map clojure.string/trim)
           (butlast) 
           (map parse-class-object-property)
      )
)

;remove nil elements from object properties
(defn prepare-class-object-properties
      [propertyList]
      (filter not-nil? propertyList)
)

;create class 'object' containing all class properties
(defn create-class-object
      [propertyList lineNum]
      {:properties (into [] propertyList) 
       :potential -1
       :line lineNum }
)

(defn read-properties
      [fileName]
      (let [content (slurp fileName)]
        (reduce (fn 
                    [collection elem]
                    (let [parsed-string (prepare-class-object-properties (parse-class-object-string elem))]
                      (conj collection (create-class-object parsed-string (+ 1 (count collection))))
                    )
                )
                [] (clojure.string/split-lines content)
        )
      )
)

;calculate euclidian distance between 2 elements
(defn euclidian-distance
      [elementA elementB]
      (->> (map - elementA elementB) 
           (map #(* % %))
           (apply +)
      )
)

;calculate hamming code for 2 elements
(defn hamming-distance
      [elementA elementB]
      (count (filter true? (map not= elementA elementB)))
)
 
;calculate module distance = |x-s| + |y-t| for 2 elements
(defn module-distance
      [elementA elementB]
      (->> (map - elementA elementB)   
        (map #(Math/abs %))
        (apply +)
      )
)
 
(defn calculate-one-component-potential
      [distance]
      (Math/exp (- (* (/ 4 (* Ra Ra)) distance)))
)


(defn add-distance-to-element-properties
      [element distance]
      (assoc element :potential distance)
)

(defn assign-potential 
      [element elements distanceCalc]
      ( 
        add-distance-to-element-properties element 
        (reduce #(+ %1  (calculate-one-component-potential (distanceCalc (:properties element) (:properties %2)))) 
                  0 elements
        )
   )
)


(defn assign-potentials 
      [elements distanceCalc]
      (map #(assign-potential %1 elements distanceCalc) elements)
)

(defn adjusted-potencial
      [distance element classCenter]
      (- (:potential element) (* (:potential classCenter)(Math/exp (- (* (/ 4 (* Rb Rb)) distance))))
      )
)

(defn adjust-potential
      [element classCenter distanceCalc]
      (let [distance (distanceCalc (:properties element) (:properties classCenter))]
         (assoc element :potential ( adjusted-potencial  distance element classCenter))
      )
)

(defn adjust-potentials
      [elements classCenter distanceCalc]
      (->> (map #(adjust-potential %1 classCenter distanceCalc) elements) 
           (sort compare-desc)
      )
)

(defn calculate-min-distance
      [element elements distanceCalc]
      (->> (map #(distanceCalc (:properties element) (:properties %1)) elements)
           (apply min)
      )
)

(defn calculate-sorted-initial-potentials
      [classElements distanceCalc] 
      (->> (assign-potentials classElements distanceCalc) 
           (sort compare-desc)
      )
)  

(defn find-object-classes
      [classElements distanceCalc]
      (let [initialPotentials (calculate-sorted-initial-potentials classElements distanceCalc)
            firstCenter       (first initialPotentials)
            restPotentials    (rest initialPotentials)]  
          (loop [centers [firstCenter] elements restPotentials]
                 (let [adjustedPoints (adjust-potentials elements (first centers) distanceCalc)
                       newCenter      (first adjustedPoints)]
                (cond
                  (> (:potential newCenter) (* EHigh (:potential firstCenter)))          
                      (recur (cons newCenter centers) (rest adjustedPoints))
                      
                  (< (:potential newCenter) (* ELow (:potential firstCenter)))  
                      (sort compare-desc centers)
                      
                  (>= (+ (/ (calculate-min-distance newCenter centers distanceCalc) Ra) (/ (:potential newCenter) (:potential firstCenter))) 1) 
                      (recur (cons newCenter centers) (rest adjustedPoints))
                      
                  :else (recur centers (cons (assoc newCenter :potential 0) (rest adjustedPoints)))
                )     
            )
          ) 
      )
)

;returns distance function depending on the given distance type
(defn get-distance-calc 
      [distanceType]
      (if (= distanceType "h") hamming-distance 
         (if (= distanceType "e") euclidian-distance 
            (if (= distanceType "m") module-distance nil)
         )
      )  
)

;check input file name and distance type.
(defn check-args 
      [fileName distanceType]
      (def distanceCalc (get-distance-calc distanceType))       
      (if (not-nil? distanceCalc)   
        (if (= (.exists (as-file fileName)) true)
           true 
           (println (format "File %s doesn't exist" fileName))
        )
        (println (format "Distance %s is not supported" distanceType))
       )
)

;program entry point
(defn -main 
      [fileName distanceType]
      (if (= (check-args fileName distanceType) true)   
        (let [classObjects (read-properties fileName)
              distanceCalc (get-distance-calc distanceType)]
          (find-object-classes classObjects distanceCalc) 
        )
      )
 )

;tests
(deftest distance-method-input
(is (= nil (get-distance-calc "o")))  
(is (not= nil (get-distance-calc "e")))
(is (not= nil (get-distance-calc "h")))   
(is (not= nil (get-distance-calc "m"))) 
)

(deftest euclidian-distance-test
(is (= 0 (euclidian-distance [2,2] [2,2])))
(is (= 50 (euclidian-distance [0,0] [5,5]))) 
(is (= 75 (euclidian-distance [0,0,0] [5,5,5]))) 
)

(deftest hamming-distance-test
(is (= 0 (hamming-distance [2,2] [2,2])))
(is (= 2 (hamming-distance [0,0] [1,1]))) 
(is (= 3 (hamming-distance [0,0,0] [1,1,1])))
(is (= 1 (hamming-distance [0,1] [1,1]))) 
(is (= 1 (hamming-distance [1,0] [1,1]))) 
)

(deftest module-distance-test
(is (= 0 (module-distance [0,0] [0,0])))
(is (= 3 (module-distance [1,1] [2,3]))) 
(is (= 9 (module-distance [1,1,1] [3,4,5])))
(is (= 3 (module-distance [2,3] [1,1]))) 
(is (= 2 (module-distance [3,3] [4,4]))) 
)

(deftest  parse-class-object-property-test
(is (= 5 ( parse-class-object-property "5")))
(is (= nil ( parse-class-object-property "5+")))
(is (= -5 ( parse-class-object-property "-5")))
(is (= 5.5 ( parse-class-object-property "5.5")))
)

(deftest prepare-class-object-properties-test
(is (= [1, 2, 3] (prepare-class-object-properties [1, 2, nil, 3, nil])))
(is (= [] (prepare-class-object-properties [nil, nil, nil])))
(is (= [] (prepare-class-object-properties [])))
(is (= [1, 2, 3] (prepare-class-object-properties [1,2,3])))
)


