(defun Kreiraj_Tablu (n)
  (make-list (* n n n) :initial-element  '-))

(defun ista (i j stanje)
                            (equal (nth i stanje)(nth j stanje)))


(defun puno (i stanje)
  (not (equal (nth i stanje) '-)))

(defun ima (potez i stanje)
  (equal (nth i stanje) potez))

(defun sledbenici (pred sled potez slob i)
  (cond ((null sled) '())
        ((and (equal (car sled) '-) slob)
         (cons (append pred (list potez) (cdr sled))
               (sledbenici (append pred (list(car sled)))
                           (cdr sled)  potez '() (if (equalp i 3) '0 (1+ i)) )))
         (t (sledbenici (append pred (list (car sled)))
                        (cdr sled) potez (if (equalp i 3) 't slob ) (if (equalp i 3) '0 (1+ i)) ))))

(defun nova-stanja (stanje potez)
  (sledbenici '() stanje  (if potez 'x 'o) 't '0))

(defun stampaj-indekse (indeks n)
  (cond ((equal indeks n) '())
        (t ( if (< indeks 10) (format t "~a  " indeks) (format t "~a " indeks))(stampaj-indekse (1+ indeks) n))))


(defun stampaj (tabla hor ver ind inkr)
  (cond ((< hor 0) '())
        ((>= (- hor inkr) n)(format t "   ")(stampaj tabla hor (1+ ver) 0 (mod (1+ inkr) n)))
        ((< (- hor inkr) 0)(format t "   ")(stampaj tabla hor (1+ ver) 0 (mod (1+ inkr)n)))
        ((> ver (- (* n n) 1 )) (format t "~%")(stampaj tabla (- hor 1) 0 0 0))
        (t (format t"~a  "(nth (+(* ver n)(- hor inkr)) tabla))(stampaj tabla hor(1+ ver)0(mod(1+ inkr)n)))
        )
  )

(defun st (tab)
  (format t "~%")
  (stampaj-indekse 0 (* n n))
  (format t "~%")
  (stampaj tab (+ n n -2) 0 0 0)
  (stampaj-indekse 0 (* n n))
  )

(defun postavi (stanje oznaka indeks i)
  (cond ((null stanje) '())
        ((and (zerop indeks) (equal (car stanje) '-))
         (cons oznaka (cdr stanje)))
        ((and (zerop indeks) (puno indeks stanje) (> i 0)) (cons (car stanje) (postavi (cdr stanje) oznaka indeks (- i 1)) ))
        ((zerop indeks) stanje)
        (t (cons (car stanje) (postavi (cdr stanje) oznaka (1- indeks) i)))))

(defun unesi (stanje igrac)
  (progn (format t "~%Unesite potez (vrsta kolona) ~a :" (if igrac 'X 'O))
    (let* ((potez (read))
           (oznaka (if igrac 'x 'o))
           (val (if (or (null (car potez)) (null (cadr potez)) (not(atom (car potez))) (not(atom (cadr potez)))) (unesi stanje igrac))))
              (if val (unesi stanje igrac)
              	 (let* ((izlaz (postavi stanje oznaka (+ (* n n (- (car potez) 1)) (* n(-(cadr potez) 1 ))) (- n 1) ))) 
              	  (if (or (equal stanje izlaz)   (< (car potez) 1) (> (car potez) 4)  (< (cadr potez) 1) (> (cadr potez) 4) ) (unesi stanje igrac) izlaz)) )
           )
   
     ))

(defun 4Connect()
  (format t "~5%Loading...R2D2(Connect 4 igrica)...~%~%")
    (format t "~%~%Unesite zeljenu dimenziju table ?~% (n mora da bude paran broj)~%> ")
  (setq n (read))
  (if (=(mod n 2)1)  (4Connect))
  (if(> n 6) (4connect))
  (format t "~%~%Zelim da igram~%(X = 1 | O = 2)~%> ")
  (setq play (read))
  (cond ((equal play 1) (format t "~2%Izabrali ste da igrate X~%~%" ) (setq player1 T))
        ((equal play 2) (format t "~2%Izabrali ste da igrate O~%~%" ) (setq player1 '()))
        (t (format t "~2%Niste nista izbrali od navedenog~%~%" ) (4Connect)))
  (format t "~%Unesite r ako racunar igra prvi (r ili c):~%> ")
  (setq auto (read))
  (cond ((equal play 'r) (format t "~2%Izabrali ste da prvi igra racunarX~%~%" ) (setq auto T))
        ((equal play ç) (format t "~2%Izabrali ste da prvi igrate viO~%~%" ) (setq auto '()))
        (t (format t "~2%Niste nista izbrali od navedenog~%~%" ) (4Connect)))
  
  (igraj (Kreiraj_Tablu n) player1)
  
  )



(defun igraj(stanje igrac)
  (let* ((nstanje (if igrac (unesi stanje igrac) (car (minmax stanje 3 igrac t))))
         (progn (st nstanje))
         (if (not (kraj nstanje))
             (let* ((nnstanje (if igrac (car(minmax nstanje 4 (not igrac) t)) (unesi nstanje (not igrac)))))
               (progn (st nnstanje))
               (if (not (kraj nnstanje))
                   (igraj nnstanje igrac)(format t "~%~%Pobedio igrac ~a ~%>" (if igrac 'O 'X)))) (format t "~%~%Pobedio igrac ~a ~%>" (if igrac 'X 'O))))))
  


(defun kraj (stanje)
  (loop for i from 0 to (* n n) do
        (if (kraj-hor-desno stanje i) (return-from kraj (kraj-hor-desno stanje i))))
  (loop named outer for i from 0 to n do
        (loop for j from 0 to n do
              (if (kraj-hor-napred stanje (+(* i n) j)) (return-from kraj (kraj-hor-napred stanje (+(* i n) j))) ))
        )
  (loop for i from 0 to (1- (* n n n)) by n do
        (if(kraj-ver stanje i) (return-from kraj (kraj-ver stanje i)))
        )
  (loop for i from 0 to (* n (- n 1) n) by (* n n) do
        (if (kraj-dia-napred stanje i) (return-from kraj (kraj-dia-napred stanje i)))
        )
  (loop for i from 0 to (* n(- n 1)) by n do
        (if (kraj-dia-levo stanje i) (return-from kraj (kraj-dia-levo stanje i)))
        )
  (loop for i from (*(* n n) (- n 1)) to (* n n n ) by n do
        (if (kraj-dia-desno stanje i) (return-from kraj (kraj-dia-desno stanje i)))
        )
  (loop for i from (* n (- n 1)) to (+(*(* n n) (- n 1)) (* n (- n 1)) ) by (* n n) do
        (if (kraj-dia-nazad stanje i) (return-from kraj (kraj-dia-nazad stanje i)))
        )
  (loop for i from 0 to n do
        (if (kraj-hor-dia-lev stanje i) (return-from kraj (kraj-hor-dia-lev stanje i)))
        )
  (loop for i from (* n  n (- n 1)) to (+(* n  n (- n 1)) n ) do
        (if (kraj-hor-dia-des stanje i) (return-from kraj (kraj-hor-dia-des stanje i)))
        )	
  (if (kraj-dia-gldl stanje 0) (kraj-dia-gldl stanje 0))
  (if (kraj-dia-gldd stanje (* n n(- n 1))) (kraj-dia-gldd stanje (* n n(- n 1))))
  (if (kraj-dia-glgl stanje (* n (- n 1))) (kraj-dia-glgl stanje (* n (- n 1))))
  (if (kraj-dia-glgd stanje (+(* n n(- n 1))(* n (- n 1)))) (kraj-dia-glgd stanje (+(* n n(- n 1))(* n (- n 1)))))

  )
(defun kraj-hor-desno (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (* n n ) ) stanje) (ista ind (+ ind (* n n 2)) stanje) (ista ind (+ ind (* n n 3)) stanje)) (if (ima 'x ind stanje) '10 '-10))
        (t '())
        )
  )
(defun kraj-hor-napred (stanje ind)
     (cond ((and (puno ind stanje) (ista ind (+ ind n) stanje) (ista ind (+ ind (* n 2)) stanje) (ista ind (+ ind (* n 3)) stanje)) (if (ima 'x ind stanje) '10 '-10))
        (t '())
        )
  )

(defun kraj-ver (stanje ind)
     (cond ((and (puno ind stanje) (ista ind (+ ind 1) stanje) (ista ind (+ ind 2) stanje) (ista ind (+ ind 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
        (t '())
        )
  )

(defun kraj-dia-napred (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind n 1) stanje) (ista ind (+ ind (* n 2) 2) stanje) (ista ind (+ ind (* n 3) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )

(defun kraj-dia-levo (stanje ind)
    (cond ((and (puno ind stanje) (ista ind (+ ind (*(* n n ) 1) 1) stanje) (ista ind (+ ind  (*(* n n ) 2) 2) stanje) (ista ind (+ ind  (*(* n n ) 3) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )
(defun kraj-dia-desno (stanje ind)
      (cond ((and (puno ind stanje) (ista ind (+ ind (-(*(* n n ) 1)) 1) stanje) (ista ind (+ ind  (-(*(* n n ) 2)) 2) stanje) (ista ind (+ ind  (-(*(* n n ) 3)) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )
(defun kraj-dia-nazad (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (-(* n 1)) 1) stanje) (ista ind (+ ind  (-(* n 2)) 2) stanje) (ista ind (+ ind (-(* n 3)) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
              (t '())
              )
  )

(defun kraj-dia-gldl (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (* n n 1 )(* n 1) 1) stanje) (ista ind (+ ind (* n n 2 )(* n 2) 2) stanje) (ista ind (+ ind (* n n 3 )(* n 3) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )
(defun kraj-dia-gldd (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (-(* n n 1)) (* n 1) 1 ) stanje) (ista ind (+ ind (-(* n n 2)) (* n 2) 2 ) stanje) (ista ind (+ ind (-(* n n 3)) (* n 3) 3 ) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )

(defun kraj-dia-glgl (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (* n n 1) (-(* n 1)) 1 ) stanje) (ista ind (+ ind (* n n 2) (-(* n 2)) 2 ) stanje) (ista ind (+ ind (* n n 3) (-(* n 3)) 3 ) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )

(defun kraj-dia-glgd (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (-(* n n 1)) (-(* n 1)) 1 ) stanje) (ista ind (+ ind (-(* n n 2)) (-(* n 2)) 2) stanje) (ista ind (+ ind (-(* n n 3)) (-(* n 3)) 3) stanje)) (if (ima 'x ind stanje) '10 '-10))
            (t '())
            )
  )
(defun kraj-hor-dia-lev (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (* n n 1) (* n 1)) stanje) (ista ind (+ ind (* n n 2) (* n 2)) stanje) (ista ind (+ ind (* n n 3) (* n 3)) stanje)) (if (ima 'x ind stanje) '10 '-10))
          (t '())
          )
  )

(defun kraj-hor-dia-des (stanje ind)
  (cond ((and (puno ind stanje) (ista ind (+ ind (-(* n n 1)) (* n 1)) stanje) (ista ind (+ ind (-(* n n 2)) (* n 2)) stanje) (ista ind (+ ind  (-(* n n 3)) (* n 3)) stanje)) (if (ima 'x ind stanje) '10 '-10))
          (t '())
          )
  )

(defun max-stanje (lsv)
  (max-stanje-i (cdr lsv)(car lsv))
)

(defun max-stanje-i (lsv stanje-vrednosti)
(cond 
  ((null lsv) stanje-vrednosti)
  ((> (cadar lsv)(cadr stanje-vrednost))
        (max-stanje-i (cdr lsv) (car lsv)))
  (t (max-stanje-i (cdr lsv) stanje-vrednosti))
))

(defun min-stanje (lsv)
  (min-stanje-i (cdr lsv)(car lsv))
)

(defun min-stanje-i (lsv stanje-vrednosti)
(cond 
  ((null lsv) stanje-vrednosti)
  ((< (cadar lsv)(cadr stanje-vrednost))
        (min-stanje-i (cdr lsv) (car lsv)))
  (t (min-stanje-i (cdr lsv) stanje-vrednosti))
))

(defun minmax (stanje dubina moj-potez start)
  (cond (( or (zerop dubina) (null lp)) (list stanje(or (kraj stanje) (proceni-stanje stanje))))
        (t ((let 
                ((lp (nova-stanja stanje moj-potez))
                 (f (if moj-potez 'max-stanje 'min-stanje)))
              (cond 
               (start (apply f (list (mapcar (lambda (x) (minmax x (1- dubina) (not moj-potez) '())) lp))))
               (t (cons stanje (cdr (apply f (list (mapcar (lambda (x) (minmax x (1- dubina)(not moj-potez) '())) lp))))))))))))

              
(defun proceni-stanje (stanje)
 (proceni-hor stanje) )

(defun proceni-hor (stanje)
  (+ (if (and (ista 0 1 stanje) (ista 0 2 stanje) (ima 'x 0 stanje)) 1 0)
     (if (and (ista 0 1 stanje) (ista 0 2 stanje) (ista 0 3 stanje) (ima 'x 0 stanje)) 3 0)
     (if (and (ista 1 2 stanje) (ima 'x 1 stanje)) 1 0)
     (if (and (ista 0 1 stanje) (ima 'o 0 stanje)) '-1 0)
     (if (and (ista 0 2 stanje) (ima 'o 0 stanje)) '-1 0)
     (if (and (ista 1 2 stanje) (ima 'o 1 stanje)) '-1 0)
     (if (and (ista 3 4 stanje) (ima 'x 3 stanje)) 1 0)
     (if (and (ista 3 5 stanje) (ima 'x 3 stanje)) 1 0)
     (if (and (ista 4 5 stanje) (ima 'x 4 stanje)) 1 0)
     (if (and (ista 3 4 stanje) (ima 'o 3 stanje)) '-1 0)
     (if (and (ista 3 5 stanje) (ima 'o 3 stanje)) '-1 0)
     (if (and (ista 4 5 stanje) (ima 'o 4 stanje)) '-1 0)
     (if (and (ista 6 7 stanje) (ima 'x 6 stanje)) 1 0)
     (if (and (ista 6 8 stanje) (ima 'x 6 stanje)) 1 0)
     (if (and (ista 7 8 stanje) (ima 'x 7 stanje)) 1 0)
     (if (and (ista 6 7 stanje) (ima 'o 6 stanje)) '-1 0)
     (if (and (ista 6 8 stanje) (ima 'o 6 stanje)) '-1 0)
     (if (and (ista 7 8 stanje) (ima 'o 7 stanje)) '-1 0)))

(4connect)
