(ns vrd-calc.core
  (:require [dk.ative.docjure.spreadsheet :as spr]
            [clojure.algo.generic.functor :as func])
  (:import (org.apache.poi.xssf.usermodel XSSFWorkbook)))


(defn make-col-map-range
  [lower upper]
  (zipmap (map (comp keyword str)
               (range lower upper))
          (map (comp keyword str char)
               (iterate inc (+ lower (int \A))))))

(defrecord Drafter [name wins losses draws])

(defn parse-vrd [workbook sheet-name]
  (let [row-headers 1
        num-drafters 8
        re-record #"([0-7])W - ([0-7])L( - )?([0-7])?D?"
        record-seq (fn [[_ w l _ d]]
                     (->> [w l d]
                          (remove nil?)
                          (filter (partial re-matches #"[\d]"))
                          (map read-string)))
        cell-values (->> workbook
                         (spr/select-sheet sheet-name)
                         (spr/cell-seq)
                         (map spr/read-cell))
        names (->> cell-values (drop row-headers) (take num-drafters))
        record-strs (->> cell-values
                         (drop (+ row-headers num-drafters))
                         (drop-while nil?)
                         (take num-drafters))
        records (map (comp record-seq (partial re-matches re-record))
                     record-strs)
        drafters (map (fn [n [w l d]] (->Drafter n w l (or d 0)))
                      names
                      records)
        picks (->> cell-values
                   (drop-while (complement (partial = 1.0))) ;; first draft row
                   (remove nil?)
                   (partition 10)
                   (map (comp rest butlast)))]
    {:drafters drafters
     :picks    picks}))

(defn load-vrds
  "Given a workbook and a sequence of draft numbers (1, 2, ..),
   loads and parses the draft as a Vrd."
  [workbook draft-num-seq]
  (map (partial parse-vrd workbook)
       (map (partial str "VRD") draft-num-seq)))

(defn draft-by-pick-order
  "Takes a draft as a seq of 8-card lists (draft rounds) and creates a list
  of the picks in order."
  [draft]
  (mapcat #(concat (first %) (reverse (second %)))
          (partition 2 draft)))

(defn draft->map-card-to-pick
  "Takes a draft as a list of cards in pick order and creates a map of card
  to pick number."
  [draft]
  (apply merge (map #(hash-map %1 %2)
                    draft
                    (iterate inc 1))))

(defn vrds->map-card-to-pick-order-seq
  "Takes seq of VRDs and creates a map of cards (in all drafts) to a seq of
  pick numbers."
  [vrds]
  (let [pick-order-maps (map (comp draft->map-card-to-pick
                                   draft-by-pick-order
                                   :picks)
                             vrds)
        card-map (into {} (map (juxt identity (constantly '()))
                               (mapcat keys pick-order-maps)))]
    (apply merge-with conj card-map pick-order-maps)))

(defn avg-pick
  "Takes a map of card to pick order seq and returns a map of card to average
   pick."
  [card-to-pick-order-seq]
  (func/fmap #(/ (reduce + %) (count %)) card-to-pick-order-seq))