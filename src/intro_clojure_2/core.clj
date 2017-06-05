(ns intro-clojure-2.core
  [:use bakery.core])

(defn error [& args]
  (apply println [args])
  :error)

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))

(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))

(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))

(defn add-butter []
  (grab :butter)
  (add-to-bowl))

(defn scooped? [ingredient]
  (cond
   (= ingredient :milk)
   true
   (= ingredient :flour)
   true
   (= ingredient :sugar)
   true
   :else
   false))

(defn squeezed? [ingredient]
  (= ingredient :egg))

(defn simple? [ingredient]
  (= ingredient :butter))


(defn add-eggs [n]
  (dotimes [e n]
    (add-egg)))

(defn add-flour-cups [n]
  (dotimes [e n]
    (add-flour)))

(defn add-milk-cups [n]
  (dotimes [e n]
    (add-milk)))

(defn add-sugar-cups [n]
  (dotimes [e n]
    (add-sugar)))

(defn add-butters [n]
  (dotimes [e n]
    (add-butter)))

(defn add-squeezed
  ([ingredient]
     (add-squeezed ingredient 1))
  ([ingredient amount]
     (if (squeezed? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       (do
         (println "This function only works on squeezed ingredients. You asked me to squeeze" ingredient)
         :error))))

(defn add-scooped
  ([ingredient]
     (add-scooped ingredient 1))
  ([ingredient amount]
     (if (scooped? ingredient)
       (do
         (grab :cup)
         (dotimes [i amount]
          (scoop ingredient)
          (add-to-bowl))
         (release))
       (error "This function only works on scooped ingredients. You asked me to scoop" ingredient)
       )))

(defn add-simple
  ([ingredient]
     (add-simple ingredient 1))
  ([ingredient amount]
     (if (simple? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl))
       (error "This function only works on simple ingredients. You asked me to add" ingredient)
       )))

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
     (cond
      (squeezed? ingredient)
      (add-squeezed ingredient amount)

      (simple? ingredient)
      (add-simple ingredient amount)

      (scooped? ingredient)
      (add-scooped ingredient amount)

      :else
      (error "I do not have the ingredient" ingredient)
      )))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :butter 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(def fridge-ingredients #{:milk :egg :butter})

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(def pantry-ingredients #{:flour :sugar})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def scooped-ingredients #{:flour :sugar :milk})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the pantry. You asked me to fetch" ingredient))))
         
(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the fridge. You asked me to fetch" ingredient))))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (cond
     (from-fridge? ingredient)
     (fetch-from-fridge ingredient amount)
     (from-pantry? ingredient)
     (fetch-from-pantry ingredient amount)
     :else
     (error "This function only works on ingredients found in either the pantry or the fridge.  You asked me to fetch" ingredient))))
         
(defn -main []
  (bake-cake)
  (bake-cookies)
  (fetch-from-pantry :flour 45)
  (fetch-from-fridge :milk 10)
  (status))
