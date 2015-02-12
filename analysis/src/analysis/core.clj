(ns analysis.core
  "Analyze the survey result"
  (:use [clojure.string :only [split]])
  (:use [clojure.java.io]))

(use '[incanter.core :only [view save]])
(use 'incanter.charts)

(declare parse)

(defn read-result
  [filename]
  (with-open [rdr (reader filename)]
    (doall
      (for [line (drop 1 (line-seq rdr))] 
        (let [fields (split line #"\t")] 
          (parse fields))))))

(defn parse
  [fields]
  {
   :course (cond
             (.contains (fields 1) "CSCI 4020") "COMPILER"
             (.contains (fields 1) "CSCI 2020") "SYSDEV"
             (.contains (fields 1) "CSCI 3055") "PL"
             :else "UNK")
   :social (cond
             (.contains (fields 2) "blackboard") "BB"
             :else "G+")
   :content (cond
              (.contains (fields 4) "powerpoint") "PP"
              :else "HTML")
   :napkin (cond
             (.contains (fields 5) "neutral") 0
             (.contains (fields 5) "illustrative") 1
             :else -1)
  })

(defn distinct-courses
  [results]
  (sort (set (map :course results))))

(defn labels
  "labels of a map"
  [g]
  (sort (keys g)))

(defn counts
  [g]
  (doall
    (for [k (labels g)]
      (count (g k)))))

(defn plot
  "group by content preference by course"
  [results attr]
  (println attr)

  ; overall content preference
  (let [g (group-by attr results)
        x (pie-chart (labels g) (counts g))]
    (set-title x "CONTENT")
    (save x (format "%s.png" (name attr))))

  ; by-course content preference
  (doall
    (for [c (distinct-courses results)]
      (let [r (filter #(= c (:course %)) results)
            g (group-by attr r)
            x (pie-chart (labels g) (counts g))]
        (set-title x (format "%s (%s)" (name attr) c))
        (save x (format "%s-%s.png" (name attr) c))))))

(defn -main
  [filename]
  (let [results (read-result filename)]
    (plot results :content)
    (plot results :social)))

