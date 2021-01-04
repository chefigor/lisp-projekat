(defun Kreiraj_Tablu (n)
  (make-list (* n n n) :initial-element  '-))

(defun ista (i j stanje)
                            (equal (nth i stanje)(nth j stanje)))


(defun puno (i stanje)
  (not (equal (nth i stanje) '-)))

(defun ima (potez i stanje)
  (equal (nth i stanje) potez))

(defun sledbenici (pred sled potez)
  (cond ((null sled) '())
        ((equal (car sled) '-)
         (cons (append pred (list potez) (cdr sled))
               (sledbenici (append pred (list(car sled)))
                           (cdr sled)  potez)))
         (t (sledbenici (append pred (list (car sled)))
                        (cdr sled) potez))))

(defun nova-stanja (stanje potez)
  (sledbenici '() stanje  potez))

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
           (izlaz (postavi stanje oznaka (+ (* n n (- (car potez) 1)) (* n(-(cadr potez) 1 ))) (- n 1) )))
      (if (equal stanje izlaz) (unesi stanje igrac) izlaz))))

(defun 4Connect()

  (format t "~5%Loading...R2D2(Connect 4 igrica)...~%~%")
    (format t "~%~%Unesite zeljenu dimenziju table ?~% (n mora da bude paran broj)~%> ")
  (setq n (read))
  (if (=(mod n 2)1)  (4Connect))
  (if(> n 6) (4connect))
               (format t "~%~%Zelim da igram~%(X = 1 | O = 2)~%> ")
  (setq play (read))
              (cond ((equal play 1) (format t "~2%Izabrali ste da igrate prvi~%~%" ) (setq player1 T))
                    ((equal play 2) (format t "~2%Izabrali ste da igrate drugi~%~%" ) (setq player1 '()))
                    (t (format t "~2%Niste nista izbrali od navedenog~%~%" ) (4Connect)))
  (igraj (Kreiraj_Tablu n) player1)
  )



(defun igraj(stanje igrac)
  (let* ((nstanje (unesi stanje igrac)))
    (progn (st nstanje))
    (if (not (kraj nstanje))
        (let* ((nnstanje (unesi nstanje (not igrac))))
          (progn (st nnstanje))
          (if (not (kraj nnstanje))
          (igraj nnstanje igrac)(format t "~%~%Pobedio igrac ~a ~%>" (if igrac 'X 'O)))) (format t "~%~%Pobedio igrac ~a ~%>" (if igrac 'X 'O)))))



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


(4connect)
