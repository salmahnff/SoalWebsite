{}
(ns coba)

;GEN-OPTION
(defn gen-options [xs]
  (let [alphabet (mapv str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        xs-part  (map vec (shuffle (partition 2 xs)))
        xs-pair  (mapv #(conj %2 %) (range) xs-part)
        xs-true  (filterv #(= true (% 0)) xs-pair)]
    {:options xs-pair
     :jawaban (alphabet (get-in xs-true [0 2]))}))

;MAPPING
(defn mapping
  [maps]
  (let [{:keys [topic bahas soal]} maps
        {:keys [soal-text options]} soal]
    {:topic topic 
     :bahas bahas
     :soal (merge
            {:soal-text soal-text}
            (gen-options options))
          :problem-id (clojure.string/replace (.toString (java.util.UUID/randomUUID)) #"-" "")}))


;GENERATE
(defn generate [i xs]
  (->> xs
       (repeatedly i)
       (#(if (every? map? %)
           (map mapping %)
           (throw (Exception. "input must be a seq of maps"))))
       vec))

;contoh cara pakai
;CONTOH MATH 1
(generate
 10
 (fn []
   (let [topic :math

         a (rand-int 20)
         b (rand-int 20)

         soal (str "berapakah hasil dari " a " * " b)
         pb (* a b)
         p1 (+ a 100)
         p2 (+ a 50)
         p3 (+ a 1000)] 
     
     {:topic topic
      :bahas "Gampang aja, tinggal pakai konsep perkalian biasa, ya"
      :soal {:soal-text soal
             :options [true pb
                       false p1
                       false p2
                       false p3]}})))

;CONTOH MATH 2
(generate
 1
 (fn []
   (let [topic :math

         pb 122
         p1 125
         p2 130
         p3 200
 
         soal (str "Jika c adalah sisi miring dari sebuah segitiga siku-siku, manakan nilai c yang sesuai jika diketahui kedua sisi lainnya adalah 22 dan 120 ")]
     {:topic topic
      :bahas "Ingat dalam segitiga siku-siku berlaku teorema pythagoras."
      :soal {:soal-text soal
             :options [true pb
                       false p1
                       false p2
                       false p3]}})))

;CONTOH MATH 3
(generate
 5
 (fn [] 
   (let [topic :math

         a (rand-nth (vec (filter even? (range 30))))
         b (/ a 2)
         pb (+ b 6)
         p1 (/ (+ a 6) 2)
         p2 (rand-nth (remove #{p1 pb 0} (range 50)))
         p3 (rand-nth (remove #{p1 pb p2 0} (range 50)))
        

         soal (str "jika umur kakak adalah " a " dan umur adek adalah setengah umur kakak, berapakah umur adek 6 tahun lagi?")
         pem (str "jawabannya adalah " pb " yaa")]
     {:topic topic
      :bahas "Gampang aja, kerjain secara bertahap ya."
      :soal {:soal-text soal
             :options [true pb
                       false p1
                       false p2
                       false p3]}})))


;CONTOH VERBAL LOGIC 1
(generate
 5
 (fn []
   (let [topic :verbal-logic
         a (rand-nth ["gon" "killua" "eren" "levi"])
         b (rand-nth ["beli" "jual" "bikin" "bawa"])
         c (rand-nth ["nasi" "karpet" "bakiak" "sajadah"])
         d (rand-nth ["dengan" "bersama" "bareng" "untuk"])
         e (rand-nth ["babeh" "emak" "kakak" "adik"])

         soal (rand-nth [(str a " adalah kata...")
                         (str a " merupakan kata...") 
                         (str a " ialah kata...")])
         pb "kata benda konkret."
         p1 "kata benda abstrak."
         p2 "bukan kata benda."]
     {:topic topic
      :bahas "Kata benda konkret adalah kata benda yang dapat ditangkap panca indera, sedangkan kata benda abstrak tidak."
      :soal {:soal-text (str a " " b " " c " " d " " e \. " " soal)
             :options [true pb,
                       false p1
                       false p2]}})))

;CONTOH VERBAL LOGIC 2
(generate
 10
 (fn []
   (let [topic :verbal-logic
         a (rand-nth ["rumah" "lukisan" "topi" "laptop" "lukisan" "handphone"])
         b (rand-nth ["mewah" "mahal" "hitam" "kotor" "norak" "rapi" "berantakan"])

         soal (rand-nth [" bentuk di atas adalah..." " bentuk di atas yaitu..."])
         pb "nonpernyataan."
         p1 "pernyataan."]
     {:topic topic
      :bahas "Pernyataan itukan kalimat yang bernilai benar atau salah. jadi kalau suatu itu bukan kalimat, ya bukan pernyataan yaaa. Kalimat itu minimal ada predikat dan subjek, kalau frasa itu menempati satu posisi aja, misalnya subjek, jadi bukan kalimat."
      :soal {:soal-text (str a " " b \. " " soal)
             :options [true pb
                       false p1]}})))

;CONTOH VERBAL LOGIC 3
(generate
  5
  (fn []
    (let [topic :verbal-logic
          a (rand-nth ["Joni" "Aulia" "Rachel" "Jose" "Messi" "Leo"])
          b (rand-nth ["supir" "pilot" "ayah" "pembalap" "montir" "nelayan"])
          c (rand-nth ["laki-laki" "orang dewasa" "orang yang kuat" "orang yang berkecukupan" "orang yang punya uang" "orang yang terampil"])
          d (rand-nth [(str a " adalah seorang " b "," " jadi " a " itu " c \.)])

          soal (rand-nth ["Kalimat manakah yang diperlukan untuk melengkapi argumen?"
                          "Untuk melengkapi argumen, kalimat mana yang diperlukan?"])

          pb (str "semua " b " itu " c ".")
          p1 (str "sebagian " b " itu " c ".")
          p2 (str "sebagian " c " itu " b ".")
          p3 "argumen sudah lengkap"]

      {:topic topic
       :bahas "Gampang aja, perhatikan susunan kalimatnya ya. "
       :soal {:soal-text (str a " adalah seorang " b "," " jadi " a " itu " c \. " " soal)
              :options [true pb
                        false p1
                        false p2
                        false p3]}})))

;CONTOH BAHASA INGGRIS
(generate
 8
 (fn []
   (let [topic :english
         a ["pemanggang roti" "penanak nasi" "mesin cuci baju" "kulkas" "setrika" "kompor gas" "senter" "televisi"]
         b ["toaster" "rice cooker" "washing machine" "refrigerator" "iron" "gas stove" "flashlight" "television"]
           
         a-b (rand-nth (partition 2 (interleave a b)))
         
         soal (str "Apa bahasa inggris " (nth a-b 0) " ?")

         pb (nth a-b 1)
         p1 (rand-nth (remove #{pb} b))
         p2 (rand-nth (remove #{pb p1} b))
         p3 (rand-nth (remove #{pb p2} b))]
     
     {:topic topic
      :bahas (str "Dalam bahasa inggris, " (nth a-b 0) " disebut " (nth a-b 1) " yaa.")
      :soal {:soal-text soal
             :options [true pb 
                       false p1
                       false p2
                       false p3]}})))
