(ns pw.engine
  (:use pw.game))

(def *distance* (make-array Integer/TYPE 40 40))

(defn depart
  ([game orders1 orders2] (depart (depart game orders1) orders2))
  ([game [[src dest ships] & rest]]
     (if src
       (let [d (aget *distance* src dest)
             g1 (update-in game [:planets src :num-ships] - ships)
             g2 (update-in g1 [:fleets] conj
                           (struct fleet (:owner ((:planets game) src)) ships src dest d d))]
         (recur g2 rest))
       game)))

(defn advance [game]
  (let [new-fleets (into [] (map #(update-in % [:turns-remaining] dec) (:fleets game)))
        g1 (assoc game :fleets new-fleets)
        new-planets (into [] (map #(update-in % [:num-ships] + (if (neut? %) 0 (:growth-rate %))) (:planets game)))
        g2 (assoc g1 :planets new-planets)]
    g2))

(defn step [planet game]
  (loop [ships-on-planet (:num-ships planet)
         owner (:owner planet)
         fleets (->> (:fleets game)
                     (filter #(and (zero? (:turns-remaining %))
                                   (== (:id planet) (:dest %)))))]
    (if (empty? fleets)
      planet
      (let [my-ships (reduce + (->> fleets (filter my?) (map :num-ships)))
            enemy-ships (reduce + (->> fleets (filter enemy?) (map :num-ships)))
            [new-owner new-ships] (resolve-battle owner
                                                  ships-on-planet
                                                  my-ships
                                                  enemy-ships)]
        (merge planet {:owner new-owner :num-ships new-ships})))))

(defn arrive [game]
  (let [new-planets (into [] (map #(step % game) (:planets game)))
        g1 (assoc game :planets new-planets)
        new-fleets (into [] (remove #(zero? (:turns-remaining %)) (:fleets game)))
        g2 (assoc g1 :fleets new-fleets)]
    g2))

(defn update [game orders1 orders2]
  (->> (depart game orders1 orders2) advance arrive))

(defn winer [game]
  (let [p1 (ships 1 game)
        p2 (ships 2 game)]
    (cond
     (and (zero? p1) (zero? p2)) :draw
     (zero? p1) :player2
     (zero? p2) :player1
     :else nil)))

(let [game (parse-game-state (slurp (str "maps/map1.txt")))]
  (let [orders1 ((find-var 'pw.bot/do-turn) game 1)
        orders2 ((find-var 'pw.bot1/do-turn) (reverse-owner game) 1)
        new-game (update game orders1 orders2) ]
    new-game))

(defn reverse-owner [game]
  (let [new-planets (map #(update-in % [:owner] opponent) (:planets game))
        new-fleets (map #(update-in % [:owner] opponent) (:fleets game))
        g1 (assoc game :planets new-planets)
        g2 (assoc g1 :fleets new-fleets)]
    g2))

(defn run [bot1 bot2 map]
  (require [bot1 :as 'b1] [bot2 :as 'b2] :reload)
  (let [game (parse-game-state (slurp (str "maps/" map)))]
    (set-distances (:planets game) *distance*)
    (loop [game game
           turn 1]
      (let [w (winer game)]
        (if (or w (> turn 200))
          (print w)
	  (do ;(print game)
	      (let [orders1 ((find-var 'pw.bot/do-turn) game turn)
		    orders2 ((find-var 'pw.bot1/do-turn) (reverse-owner game) turn)
		    new-game (update game orders1 orders2)]
		(recur new-game (inc turn)))))))))

(run 'pw.bot 'pw.bot1 "map1.txt")