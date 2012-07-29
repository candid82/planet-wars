(ns pw.bot1
  (:use pw.game
	[clojure.contrib.math :only (ceil)]))

;(defmacro time-err [expr]
;  `(let [start# (. System (nanoTime))
;         ret# ~expr]
;     (.println java.lang.System/err (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
;     ret#))

;; (def *prof* (java.util.Hashtable.))

;; (defn print-prof []
;;   (doseq [v (sort-by #(second (.getValue %)) *prof*)]
;;     (.println java.lang.System/err (str "" v))))

;; (defmacro defn [fn-name args body]
;;   (let [f (name fn-name)]
;;     (do (.put *prof* f [0 0.0])
;;         `(clojure.core/defn ~fn-name ~args
;;            (let [r# (.get *prof* ~f) start# (. System (nanoTime)) ret# ~body]
;;              (.put *prof* ~f
;;                    [(inc (r# 0)) (+ (r# 1)
;;                                     (/ (double (- (. System (nanoTime)) start#)) 1000000.0))])
;;              ret#)))))


(def *distance* (make-array Integer/TYPE 40 40))
(def *orders*)
(def *avg-bonus* (make-array Integer/TYPE 1))
(def *bonus-turns* 50)

(defn distance [src dest]
  (aget *distance* (src :id) (dest :id)))

(defn avg-bonus [p game]
 (let [others (remove #(== (:id %) (:id p)) (:planets game))
	d (reduce + (map #(distance % p) others))
	c (count others)
	avg-distance (int (/ d c))]
   (- (* (:growth-rate p) (- *bonus-turns* avg-distance))
      (:num-ships p) 1)))

(defn set-avg-bonus [game]
 (let [n (neut-planets game)
	b (reduce + (map #(avg-bonus % game) n))
	c (count n)]
   (debug (int (/ b c)))
   (aset *avg-bonus* 0 (int (/ b c)))))

(defn ships-to-get1 [dest turns game]
  (let [t (reduce + (map #(int (/ (:num-ships %) (distance % dest)))
                         (remove #(== (:id %) (:id dest)) (enemy-planets game))))]
    (if (neut? dest)
      (+ 1

         (dest :num-ships))
      (+ 1
         (int (/ t 3))
         (dest :num-ships)
         (max 0 (* (dest :growth-rate) (- turns (:turns dest))))))))

(def ships-to-get (memoize ships-to-get1))

(defn geo-risk [p game]
  (reduce + (for [e (enemy-planets game)]
              (/ (:num-ships e) (distance p e)))))

(defn with-geo-risks [game]
  (assoc game :planets
         (into [] (map #(if (my? %)
                          (assoc % :geo-risk (geo-risk % game))
                          %) (:planets game)))))

(defn geo [p game]
  (let [a (reduce + (for [e (enemy-planets game) :when (not= (:id e) (:id p))]
                      (/ (:num-ships e) (sq (distance p e)))))
        b (reduce + (for [e (my-planets game) :when (not= (:id e) (:id p))]
                      (/ (:num-ships e) (sq (distance p e)))))]
                                        ;(debug (:id p) (double a) (double b) (double (/ b (max a 1/1000000))))
    (/ b (max a 1/1000000))))

(defn with-geo [game]
  (assoc game :planets
         (into [] (map #(assoc % :geo (geo % game)) (:planets game)))))

(defn bonus [planet distance ships]
  (let [prod (* (planet :growth-rate) (- *bonus-turns* distance))
        b (if (neut? planet)
            (- prod ships)
            (* 2 prod))]
                                        ;(debug planet distance ships b)
    ;(int (* b (:geo planet)))
    b))

(defn ships-available1 [planet game]
  (loop [b (:num-ships planet)
         a 0]
    (let [x (int (ceil (/ (+ a b) 2)))]
      (if (== x b) (- (:num-ships planet) x)
          (let [c (assoc planet :num-ships x)
                p (forward c game)]
            (if (my? p)
              (recur x a)
              (recur b x)))))))

(def ships-available (memoize ships-available1))

;; (defn ships-available [planet game]
;;   (- (:num-ships planet) (enemy-ships-to-planet planet game)))

(defn geo-filter [p]
  (if (neut? p) (< 1.2 (:geo p)) (< 1 (:geo p))))

(defn planets-to-attack [game]
  (let [p (map #(forward % game)
	       (filter geo-filter (:planets game)))]
    (if (> (my-prod game) (enemy-prod game))
      (filter enemy? p)
      (remove my? p))))

(defn available-planets [turns-waited game]
  (filter #(pos? (+ (ships-available % game)
		    (* turns-waited (:growth-rate %))))
	  (remove :used (my-planets game))))

(defn probe-planet [target ships-needed turns-waited game]
  (let [all-planets (sort-by #(distance % target) (available-planets turns-waited game))]
    (loop [orders []
           planets (if (== 0 (:old-owner target))
                     (remove #(> (:turns target) (+ turns-waited (distance % target))) all-planets)
                     all-planets)
           total-ships 0]
      (if (empty? planets)
        nil
        (let [p (first planets)
              d (+ turns-waited (distance p target))
              ships-required (- (ships-needed target d game) total-ships)
              ships (min ships-required (+ (* turns-waited (:growth-rate p)) (ships-available p game)))
              order [p target ships]]
          (if (== ships ships-required)
            (if (pos? (+ total-ships ships))
              {:target target
               :orders (conj orders order)
               :distance d
               :ships (+ total-ships ships)}
              nil)
            (recur (if (pos? ships) (conj orders order) orders)
                   (rest planets)
                   (+ total-ships (max 0 ships)))))))))

(defn orders-to-attack [game]
  (let [p (planets-to-attack game)]
    (if-let [orders
	     (for [i (range 1)]
	       (for [t p :let [order (probe-planet t ships-to-get i game)] :when order] order))]
      (apply concat orders))))

(defn best-order [orders]
  (if (empty? orders)
    nil
    (let [bonuses (for [o orders
                        :let [b (bonus (o :target) (o :distance) (o :ships))]
                        :when (> b (* 5/7 (aget *avg-bonus* 0)))] [o b])]
      (if (empty? bonuses)
        nil
        (first (apply max-key second bonuses))))))

(defn issue-single-order [[src dest n] game]
  (let [s (ships-available src game)
	ships (min n s)
	newsrc (if (== ships s) (assoc src :used true) src)]
    (if (pos? ships)
      (do
					;(println (src :id) (dest :id) ships)
	(set! *orders* (conj *orders* [(:id src) (:id dest) ships]))
	(add-fleet (remove-ships (assoc-in game [:planets (:id src)] newsrc) newsrc ships)
		   newsrc dest ships (distance newsrc dest)))
      (assoc-in game [:planets (:id src)] newsrc))))

(defn issue-order [order game]
  (loop [g game orders (order :orders)]
    (if (empty? orders)
      g
      (recur (issue-single-order (first orders) g) (rest orders)))))

(defn attack [game]
  (loop [game game]
    (if-let [order (best-order (orders-to-attack game))]
      (recur  (issue-order order game))
      game)))

(defn planet-score [p game]
  (reduce + (for [e (enemy-planets game)] (/ (:num-ships e) (sq (distance p e))))))

(defn get-best-planet [game]
  (if-let [m (seq (my-planets game))]
    (apply min-key #(:geo %) m)))

;; (defn move [game]
;;   (if-let [best-planet (get-best-planet game)]
;;     (doseq [p (remove #(== (:id best-planet) (:id %)) (my-planets game))]
;;       (let [s (ships-available p game)]
;;         (if (> s 1)
;;           (issue-single-order [p best-planet (int (/ s 2))] game))))))

(defn move [game]
  (if-let [m (seq (my-planets game))]
    (doseq [src m]
      (if-let [recipients (seq (filter #(> (:geo-risk %) (* 1.2 (:geo-risk src))) m))]
        (let [s (ships-available src game)
              dest (apply min-key #(distance src %) recipients)]
          (if (> s 1)
            (issue-single-order [src dest (int (* s 0.5))] game)))))))

(defn expand [game]
  (if (>= (my-prod game) (enemy-prod game))
    game
    (if-let [n (seq (neut-planets game))]
      (if-let [m (seq (my-planets game))]
	(let [dest (apply max-key :geo n)
	      src (apply min-key #(distance % dest) m)
	      s (ships-available src game)]
        (if (> s 1)
          (issue-single-order [src dest (int (* s 0.5))] game)
          game))
	game)
      game)))

;; (defn move [game]
;;   (if-let [m (seq (my-planets game))]
;;     (if-let [recipients (seq (filter #(> 1.2 (:geo %)) m))]
;;       (doseq [src (filter #(< 1.2 (:geo %)) m)]
;;         (let [s (ships-available src game)
;;               dest (apply min-key #(distance src %) recipients)]
;;           (if (> s 1)
;;             (issue-single-order [src dest (int (* s 0.5))] game)))))))

(defn power-attack [game]
  (if-let [m (seq (my-planets game))]
    (if-let [e (seq (enemy-planets game))]
      (let [src (apply max-key #(ships-available % game) m)
            target (apply min-key #(distance % src) e)
            s (ships-available src game)]
        (if (> s 20)
          (issue-single-order [src target (int (* s 0.5))] game)
          game))
      game)
    game))

(defn do-turn [game turn]
  (when (== turn 1)
    (set-distances (:planets game) *distance*)
    (set-avg-bonus game))
  (binding [*orders* []]
    (->> game with-geo with-geo-risks attack move)
    *orders*))

;; (defn do-turn [game]
;;   (let [a (my-prod game) b (enemy-prod game)]
;;     (if (or
;;          (> a (+ 15 b))
;;          (<= a (+ 5 b)))
;;       (-> game defend attack)
;;       (-> game defend))))

;; (use '[clojure.contrib.duck-streams :only (read-lines)])
;; (def *game* (parse-game-state (join \newline (read-lines "maps/map2.txt"))))
;; *game*
;; (set-distances (*game* :planets))
;; (def *target* ((*game* :planets) 7))
;; *target*
;; *distance*
;; (distance (first (sort-by #(distance % *target*) (my-planets *game*))) *target*)
;; (ships-to-get *target* 7)
;; (sort-by #(distance % *target*) (my-planets *game*))
;; (probe-planet *target* *game*)


(clojure.core/defn -main [& args]
  (try
    (loop [pw "" first-turn true]
      (if-let [line (read-line)]
        (cond (= "go" line) (if-not (empty? pw)
                              (do (let [game (parse-game-state pw)]
                                    (when first-turn
                                      (set-distances (game :planets))
                                      (set-avg-bonus game))
                                    (do-turn game))
                                  (finish-turn)
                                        ;(print-prof)
                                  (recur "" false))
                              (do (finish-turn)
                                  (recur "" false)))
              :else (recur (str pw line "\n") first-turn))))
    (catch Exception e
      (do
        (.println java.lang.System/err (str e))
        (.printStackTrace e)
        (java.lang.System/exit 1))))
  (java.lang.System/exit 0))

;(-main)
