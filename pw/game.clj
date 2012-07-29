(ns pw.game
  (:use [clojure.string :only (join trim split-lines split)]
        [clojure.contrib.math :only (sqrt ceil abs)]))

(defn sq [x] (* x x))

(defstruct fleet
  :owner :num-ships :src :dest
  :total-trip-length :turns-remaining)

(defstruct planet :id :x :y :owner :num-ships :growth-rate)

(defn debug [& args]
  (.println java.lang.System/err args))

(defn remove-ships [game planet n]
  (update-in game [:planets (:id planet) :num-ships] - n))

(defn add-fleet [game src dest n distance]
  (let [new-fleet (struct fleet 1 n (src :id) (dest :id) distance distance)
        new-fleets (conj (game :fleets) new-fleet)]
    (assoc game :fleets new-fleets)))

(defn dist [src dest]
  (let [dx (- (src :x) (dest :x))
        dy (- (src :y) (dest :y))]
    (int (ceil (sqrt (+ (sq dx) (sq dy)))))))

(defstruct planet-wars-game :planets :fleets)

(defn my? [x] (== 1 (x :owner)))
(defn neut? [x] (== 0 (x :owner)))
(defn enemy? [x] (< 1 (x :owner)))

(defn my-planets [game] (filter my? (game :planets)))
(defn enemy-planets [game] (filter enemy? (game :planets)))
(defn neut-planets [game] (filter neut? (game :planets)))
(defn captured-planets [game] (remove neut? (:planets game)))

(defn ships [player game]
  (+
   (reduce + (for [p (:planets game) :when (== player (:owner p))] (:num-ships p)))
   (reduce + (for [f (:fleets game) :when (== player (:owner f))] (:num-ships f)))))

(defn finish-turn [] (prn 'go))

(defn to_i [^String x] (Integer. x))
(defn to_d [^String x] (Double. x))

(defn s-to-planet [tokens id]
  (apply struct planet id
         (map #(%1 %2) [to_d to_d to_i to_i to_i] tokens)))

(defn s-to-fleet [tks] (apply struct fleet (map to_i tks)))

(defn clean [s] (when-first [s (split s #"#")] (trim s)))

(defn parse-game-state [s]
  (loop [planets [] fleets [] pid 0 lines (map clean (split-lines s))]
    (if-let [[t & tks] (try (split (clean (first lines)) #" ")
                            (catch Exception e nil))]
      (cond
       (= "P" t) (recur (conj planets (s-to-planet tks pid)) fleets
                        (inc pid)
                        (rest lines))

       (= "F" t) (recur planets (conj fleets (s-to-fleet tks)) pid
                        (rest lines)))
      (struct planet-wars-game planets fleets))))

(defn my-prod [game]
  (reduce + (map :growth-rate (my-planets game))))

(defn enemy-prod [game]
  (reduce + (map :growth-rate (enemy-planets game))))
(defn advance-fleet [fleet]
  (update-in fleet [:turns-remaining] dec))

(defn opponent [owner]
  (condp == owner
      1 2
      2 1
      0))

(defn resolve-battle [owner ships-on-planet my-ships enemy-ships]
  (if (not= owner 0)
    (let [a (if (== owner 1) my-ships (- my-ships))
          b (if (== owner 2) enemy-ships (- enemy-ships))
          new-ships (+ ships-on-planet a b)]
      [(if (neg? new-ships) (opponent owner) owner) (abs new-ships)])
    (let [max-ships (max ships-on-planet my-ships enemy-ships)]
      (cond
       (== max-ships ships-on-planet) [owner (- max-ships (max my-ships enemy-ships))]
       (== my-ships enemy-ships) [owner 0]
       (== max-ships my-ships) [1 (- my-ships (max ships-on-planet enemy-ships))]
       :else [2 (- enemy-ships (max ships-on-planet my-ships))]))))

(defn forward1 [planet game]
  (loop [ships-on-planet (:num-ships planet)
         owner (:owner planet)
         turns 0
         fleets (->> (:fleets game)
                     (filter #(== (:id planet) (:dest %))))]
    (if (empty? fleets)
      (merge planet {:num-ships ships-on-planet
                     :owner owner
                     :turns turns
                     :old-owner (:owner planet)
                     })
      (let [arrived-fleets (filter #(== (:turns-remaining %) 1) fleets)
            my-ships (reduce + (->> arrived-fleets (filter my?) (map :num-ships)))
            enemy-ships (reduce + (->> arrived-fleets (filter enemy?) (map :num-ships)))
            [new-owner new-ships] (resolve-battle owner
                                                  (if (== owner 0)
                                                    ships-on-planet
                                                    (+ ships-on-planet (:growth-rate planet)))
                                                  my-ships
                                                  enemy-ships)]
        (recur new-ships
               new-owner
               (inc turns)
               (->> fleets
                    (remove #(== (:turns-remaining %) 1))
                    (map advance-fleet)))))))

(def forward (memoize forward1))

(defn set-distances [planets dist-array]
  (doseq [p1 planets p2 planets]
    (aset dist-array (p1 :id) (p2 :id) (dist p1 p2))))

