(ns overtone-columbus-clojure.core
  (:use [overtone.live]
        [overtone.inst.piano]
        ))
;(use 'overtone.live)

;; Repl integration
(def letters [ \a \b \c \d \e \f \g \h \i \j \k 
                     \l \m \n \o \p \q \r \s \t \u \v
                     \w \x \y \z \  ])
(nth letters 3)

(def secret [ 4 17 8 2 26 8 18 26 2 14 14 11 ])

(map (fn [x] (+ 1 x)) secret) 

;; the deep philosophical truths of clojure     
(apply str (map (fn [x] (nth letters x)) secret) )


(definst sine-wave [frequency 440] (sin-osc frequency))
(sine-wave)
(stop)

;; turn it off!
(definst boop [frequency 440 duration 1]
  (let [envelope (line 1 0 duration :action FREE)]
          (* envelope (sin-osc frequency))))

(boop)
(boop :duration 3) 
(boop :duration 1/2)









;; some other waveforms
(definst saw-wave [frequency 440 duration 1]
  (let [envelope (line 1 0 duration :action FREE)]
          (* envelope (saw frequency))))

(saw-wave)

(definst triangle-wave [frequency 440 duration 1]
  (let [envelope (line 1 0 duration :action FREE)]
          (* envelope (lf-tri frequency))))

(triangle-wave)

(for [i (range 110 440)] (at (+ (now) (* i 5)) 
                             (triangle-wave i)))

(definst square-wave [frequency 440 duration 1]
  (let [envelope (line 1 0 duration :action FREE)]
          (* envelope (square frequency))))

(square-wave)







;; instruments
(boop)
;(use 'overtone.inst.piano)
(piano 69)

(boop 262)
(boop 329)
(boop 392)

(boop 69)












;;i. midi
(boop (midi->hz (note :c4)))
(boop (midi->hz (note :e4)))
(boop (midi->hz (note :g4)))

(piano (note :c4))
(piano (note :e4))
(piano (note :g4))

;;j. intervals
(defn interval [note interval] 
  (let [ intervals { :unison         0
                     :min-second     1
                     :maj-second     2
                     :min-third      3
                     :maj-third      4
                     :perfect-fourth 5
                     :aug-fourth     6
                     :dim-fifth      6
                     :perfect-fifth  7
                     :min-sixth      8
                     :maj-sixth      9
                     :min-seventh    10
                     :maj-seventh    11
                     :octave         12}
        ] (+ (intervals interval) note)))
(interval 69 :min-second)

;; scales
(defn play-seq [note-seq]
  (doseq [note note-seq]
    (piano note)
      (Thread/sleep 250)))

(play-seq (range 69 81))

(def major-scale-ints [:unison :maj-second 
                       :maj-third :perfect-fourth 
                       :perfect-fifth
                       :maj-sixth :maj-seventh :octave])

(play-seq (map #(interval (note :c4) %) major-scale-ints))

(play-seq (scale :c4 :major))















;; chords
(doseq [note (chord :c4 :major)] (piano note))
(doseq [note (chord :c4 :minor)] (piano note))

(def major-triad-formula [:unison :maj-third :perfect-fifth])
(def minor-triad-formula [:unison :min-third :perfect-fifth])

(defn chord-from-formula [tonic formula] 
  (map #(interval (note tonic) %) formula))

(chord-from-formula :c4 major-triad-formula)

(defn play-chord [a-chord]
  (doseq [note a-chord] (piano note)))

(play-chord (chord-from-formula :A4 major-triad-formula))
(play-chord (chord :A4 :major))

(let [time (now)]
  (at time (play-chord (chord :g3 :major)))
  (at (+ 2000 time) (play-chord (chord :c3 :major)))
  (at (+ 3000 time) (play-chord (chord :d3 :major)))
  (at (+ 4300 time) (play-chord (chord :g3 :major))))

(defn play-progression [chords]
  (if (empty? chords) nil
    (doseq []
      (play-chord (first chords))
      (Thread/sleep 1000)
      (play-progression (rest chords)))))

(play-progression [(chord :d3 :major7) (chord :a3 :major7) (chord :a3 :major7) (chord :f3 :major7)])
;; autumn leaves
(def autumn-leaves [(chord :c4 :major7) 
                    (chord :f4 :major7) 
                    (chord :bb4 :major7)
                    (chord :eb4 :major7)
                    (chord :a4 :7-5)
                    (chord :d3 :major7)
                    (chord :g3 :major7)])

(play-progression autumn-leaves)











(defn triplet
    "subdivide two time intervals by 3, and return the time interval
    at position. "
    [a b position]
    (+ a (* position (/ (- b a) 3) )))

(def met (metronome (* 200 2)))

(defn melody [nome]
  (let [beat (nome)]
    ;; Intro
    (at (nome beat) (piano (note :F#4)))
    (at (nome beat) (piano (note :E5)))
    (at (nome (+ 1 beat)) (piano (note :F#4)))
    (at (nome (+ 1 beat)) (piano (note :E5)))
    (at (nome (+ 3 beat)) (piano (note :F#4)))
    (at (nome (+ 3 beat)) (piano (note :E5)))
    (at (nome (+ 5 beat)) (piano (note :F#4)))
    (at (nome (+ 5 beat)) (piano (note :C5)))
    (at (nome (+ 6 beat)) (piano (note :F#4)))
    (at (nome (+ 6 beat)) (piano (note :E5)))
    (at (nome (+ 8 beat)) (piano (note :G4)))
    (at (nome (+ 8 beat)) (piano (note :B4)))
    (at (nome (+ 8 beat)) (piano (note :G5)))
    (at (nome (+ 12 beat)) (piano (note :G4)))
    ;; A
    (at (nome (+ 16 beat)) (piano (note :C5)))
    (at (nome (+ 16 beat)) (piano (note :E4)))
    (at (nome (+ 19 beat)) (piano (note :G4)))
    (at (nome (+ 19 beat)) (piano (note :C4)))
    (at (nome (+ 22 beat)) (piano (note :E4)))
    (at (nome (+ 22 beat)) (piano (note :G3)))
   
    (at (nome (+ 25 beat)) (piano (note :A4)))
    (at (nome (+ 25 beat)) (piano (note :C4)))
    (at (nome (+ 27 beat)) (piano (note :B4)))
    (at (nome (+ 27 beat)) (piano (note :D4)))
    (at (nome (+ 29 beat)) (piano (note :Bb4)))
    (at (nome (+ 29 beat)) (piano (note :Db4)))
    (at (nome (+ 30 beat)) (piano (note :A4)))
    (at (nome (+ 30 beat)) (piano (note :C4)))
   
    (at (nome (+ 32 beat)) (piano (note :G4)))
    (at (nome (+ 32 beat)) (piano (note :C4)))
    ;(at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 1) (piano (note :G4)))
    ;(at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 1) (piano (note :C4)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 2) (piano (note :E5)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 2) (piano (note :G4)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 3) (piano (note :G5)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 3) (piano (note :D5)))
    (at (nome (+ 36 beat)) (piano (note :A5)))
    (at (nome (+ 36 beat)) (piano (note :C5)))
    (at (nome (+ 38 beat)) (piano (note :F5)))
    (at (nome (+ 38 beat)) (piano (note :A4)))
    (at (nome (+ 39 beat)) (piano (note :G5)))
    (at (nome (+ 39 beat)) (piano (note :B4)))

    (at (nome (+ 41 beat)) (piano (note :E5)))
    (at (nome (+ 41 beat)) (piano (note :A4)))
    (at (nome (+ 43 beat)) (piano (note :C5)))
    (at (nome (+ 43 beat)) (piano (note :E4)))
    (at (nome (+ 44 beat)) (piano (note :D5)))
    (at (nome (+ 44 beat)) (piano (note :F4)))
    (at (nome (+ 45 beat)) (piano (note :B4)))
    (at (nome (+ 45 beat)) (piano (note :D4)))

    (at (nome (+ 48 beat)) (piano (note :C5)))
    (at (nome (+ 48 beat)) (piano (note :E4)))
    (at (nome (+ 51 beat)) (piano (note :G4)))
    (at (nome (+ 51 beat)) (piano (note :C4)))
    (at (nome (+ 54 beat)) (piano (note :E4)))
    (at (nome (+ 54 beat)) (piano (note :G3)))
   
    (at (nome (+ 57 beat)) (piano (note :A4)))
    (at (nome (+ 57 beat)) (piano (note :C4)))
    (at (nome (+ 59 beat)) (piano (note :B4)))
    (at (nome (+ 59 beat)) (piano (note :D4)))
    (at (nome (+ 61 beat)) (piano (note :Bb4)))
    (at (nome (+ 61 beat)) (piano (note :Db4)))
    (at (nome (+ 62 beat)) (piano (note :A4)))
    (at (nome (+ 62 beat)) (piano (note :C4)))
   
    (at (nome (+ 64 beat)) (piano (note :G4)))
    (at (nome (+ 64 beat)) (piano (note :C4)))
    ;(at (triplet (nome (+ 64 beat)) (nome (+ 35 beat)) 1) (piano (note :G4)))
    ;(at (triplet (nome (+ 64 beat)) (nome (+ 35 beat)) 1) (piano (note :C4)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 2) (piano (note :E5)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 2) (piano (note :G4)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 3) (piano (note :G5)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 3) (piano (note :D5)))
    (at (nome (+ 68 beat)) (piano (note :A5)))
    (at (nome (+ 68 beat)) (piano (note :C5)))
    (at (nome (+ 70 beat)) (piano (note :F5)))
    (at (nome (+ 70 beat)) (piano (note :A4)))
    (at (nome (+ 71 beat)) (piano (note :G5)))
    (at (nome (+ 71 beat)) (piano (note :B4)))

    (at (nome (+ 73 beat)) (piano (note :E5)))
    (at (nome (+ 73 beat)) (piano (note :A4)))
    (at (nome (+ 75 beat)) (piano (note :C5)))
    (at (nome (+ 75 beat)) (piano (note :E4)))
    (at (nome (+ 76 beat)) (piano (note :D5)))
    (at (nome (+ 76 beat)) (piano (note :F4)))
    (at (nome (+ 77 beat)) (piano (note :B4)))
    (at (nome (+ 77 beat)) (piano (note :D4)))
    
    (apply-by (nome (+ 88 beat)) melody nome [])))

(melody met)

(defn minor-melody [nome]
  (let [beat (nome)]
    ;; Intro
    (at (nome beat) (piano (note :F#4)))
    (at (nome beat) (piano (note :Eb5)))
    (at (nome (+ 1 beat)) (piano (note :F#4)))
    (at (nome (+ 1 beat)) (piano (note :Eb5)))
    (at (nome (+ 3 beat)) (piano (note :F#4)))
    (at (nome (+ 3 beat)) (piano (note :Eb5)))
    (at (nome (+ 5 beat)) (piano (note :F#4)))
    (at (nome (+ 5 beat)) (piano (note :C5)))
    (at (nome (+ 6 beat)) (piano (note :F#4)))
    (at (nome (+ 6 beat)) (piano (note :Eb5)))
    (at (nome (+ 8 beat)) (piano (note :G4)))
    (at (nome (+ 8 beat)) (piano (note :B4)))
    (at (nome (+ 8 beat)) (piano (note :G5)))
    (at (nome (+ 12 beat)) (piano (note :G4)))
    ;; A
    (at (nome (+ 16 beat)) (piano (note :C5)))
    (at (nome (+ 16 beat)) (piano (note :Eb4)))
    (at (nome (+ 19 beat)) (piano (note :G4)))
    (at (nome (+ 19 beat)) (piano (note :C4)))
    (at (nome (+ 22 beat)) (piano (note :Eb4)))
    (at (nome (+ 22 beat)) (piano (note :G3)))
   
    (at (nome (+ 25 beat)) (piano (note :A4)))
    (at (nome (+ 25 beat)) (piano (note :C4)))
    (at (nome (+ 27 beat)) (piano (note :B4)))
    (at (nome (+ 27 beat)) (piano (note :D4)))
    (at (nome (+ 29 beat)) (piano (note :Bb4)))
    (at (nome (+ 29 beat)) (piano (note :Db4)))
    (at (nome (+ 30 beat)) (piano (note :A4)))
    (at (nome (+ 30 beat)) (piano (note :C4)))
   
    (at (nome (+ 32 beat)) (piano (note :G4)))
    (at (nome (+ 32 beat)) (piano (note :C4)))
    ;(at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 1) (piano (note :G4)))
    ;(at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 1) (piano (note :C4)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 2) (piano (note :Eb5)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 2) (piano (note :G4)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 3) (piano (note :G5)))
    (at (triplet (nome (+ 32 beat)) (nome (+ 35 beat)) 3) (piano (note :D5)))
    (at (nome (+ 36 beat)) (piano (note :A5)))
    (at (nome (+ 36 beat)) (piano (note :C5)))
    (at (nome (+ 38 beat)) (piano (note :F5)))
    (at (nome (+ 38 beat)) (piano (note :A4)))
    (at (nome (+ 39 beat)) (piano (note :G5)))
    (at (nome (+ 39 beat)) (piano (note :B4)))

    (at (nome (+ 41 beat)) (piano (note :Eb5)))
    (at (nome (+ 41 beat)) (piano (note :A4)))
    (at (nome (+ 43 beat)) (piano (note :C5)))
    (at (nome (+ 43 beat)) (piano (note :Eb4)))
    (at (nome (+ 44 beat)) (piano (note :D5)))
    (at (nome (+ 44 beat)) (piano (note :F4)))
    (at (nome (+ 45 beat)) (piano (note :B4)))
    (at (nome (+ 45 beat)) (piano (note :D4)))

    (at (nome (+ 48 beat)) (piano (note :C5)))
    (at (nome (+ 48 beat)) (piano (note :Eb4)))
    (at (nome (+ 51 beat)) (piano (note :G4)))
    (at (nome (+ 51 beat)) (piano (note :C4)))
    (at (nome (+ 54 beat)) (piano (note :Eb4)))
    (at (nome (+ 54 beat)) (piano (note :G3)))
   
    (at (nome (+ 57 beat)) (piano (note :A4)))
    (at (nome (+ 57 beat)) (piano (note :C4)))
    (at (nome (+ 59 beat)) (piano (note :B4)))
    (at (nome (+ 59 beat)) (piano (note :D4)))
    (at (nome (+ 61 beat)) (piano (note :Bb4)))
    (at (nome (+ 61 beat)) (piano (note :Db4)))
    (at (nome (+ 62 beat)) (piano (note :A4)))
    (at (nome (+ 62 beat)) (piano (note :C4)))
   
    (at (nome (+ 64 beat)) (piano (note :G4)))
    (at (nome (+ 64 beat)) (piano (note :C4)))
    ;(at (triplet (nome (+ 64 beat)) (nome (+ 35 beat)) 1) (piano (note :G4)))
    ;(at (triplet (nome (+ 64 beat)) (nome (+ 35 beat)) 1) (piano (note :C4)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 2) (piano (note :Eb5)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 2) (piano (note :G4)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 3) (piano (note :G5)))
    (at (triplet (nome (+ 64 beat)) (nome (+ 67 beat)) 3) (piano (note :D5)))
    (at (nome (+ 68 beat)) (piano (note :A5)))
    (at (nome (+ 68 beat)) (piano (note :C5)))
    (at (nome (+ 70 beat)) (piano (note :F5)))
    (at (nome (+ 70 beat)) (piano (note :A4)))
    (at (nome (+ 71 beat)) (piano (note :G5)))
    (at (nome (+ 71 beat)) (piano (note :B4)))

    (at (nome (+ 73 beat)) (piano (note :Eb5)))
    (at (nome (+ 73 beat)) (piano (note :A4)))
    (at (nome (+ 75 beat)) (piano (note :C5)))
    (at (nome (+ 75 beat)) (piano (note :Eb4)))
    (at (nome (+ 76 beat)) (piano (note :D5)))
    (at (nome (+ 76 beat)) (piano (note :F4)))
    (at (nome (+ 77 beat)) (piano (note :B4)))
    (at (nome (+ 77 beat)) (piano (note :D4)))
    
    (apply-by (nome (+ 88 beat)) melody nome [])))

(minor-melody met)
























;(defn bassline [nome]
;  (let [beat (nome)]
;    ;; Intro
;    (at (nome beat) (piano (note :D3)))
;    (at (nome (+ 1 beat)) (piano (note :D3)))
;    (at (nome (+ 3 beat)) (piano (note :D3)))
;    (at (nome (+ 5 beat)) (piano (note :D3)))
;    (at (nome (+ 6 beat)) (piano (note :D3)))
;    (at (nome (+ 12 beat)) (piano (note :G3)))
;    
;    ;; A
;    (at (nome (+ 1 beat)) (piano (note :D3)))
;    (at (nome (+ 3 beat)) (piano (note :D3)))
;    (at (nome (+ 5 beat)) (piano (note :D3)))
;    
;    (apply-by (nome (+ 88 beat)) bassline nome [])))
;(bassline met)
;
;(defn section [nome]
; (melody nome) 
; (bassline nome))
;
;(section met)

