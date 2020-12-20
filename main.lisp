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
               (sledbenici1 (append pred (list(car sled)))
                           (cdr sled)  potez)))
         (t (sledbenici1 (append pred (list (car sled)))
                        (cdr sled) potez))))

(defun nova-stanja (stanje potez)
  (sledbenici '() stanje  potez))         

(defun stampaj-indekse (indeks n)
  (cond ((equal indeks n) '())
        (t ( if (< indeks 10) (format t "~a  " indeks) (format t "~a " indeks))(stampaj-indekse (1+ indeks) n)))))


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
  (progn (format t "~%Unesite potez (vrsta kolona) ~a :" igrac)
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
               (format t "~%~%Zelim da igram~%(prvi = 1 | drugi = 2)~%> ")
  (setq play (read))
              (cond ((equal play 1) (format t "~2%Izabrali ste da igrate prvi~%~%" ) (setq player1 T))  
                    ((equal play 2) (format t "~2%Izabrali ste da igrate drugi~%~%" ) (setq player1 '()))
                    (t (format t "~2%Niste nista izbrali od navedenog~%~%" ) (4Connect)))
  (igraj (Kreiraj_Tablu n) player1) 
  )


 
(defun igraj(stanje igrac)
  (let* ((nstanje (unesi stanje igrac)))
          (progn (st nstanje))
          (let* ((nnstanje (unesi nstanje '())))
            (progn (st nnstanje))
            (igraj nnstanje igrac))))


Kraj(stanje igrac ind dekr) dekr=4
If(ind <0 or ind > n*n*n) return false

if((nth ind stanje)==igrac) 
	{
	and(
		Kraj(stanje igrac ind+1 dekr--)
		Kraj(stanje igrac ind+n dekr--)
		Kraj(stanje igrac ind+n+1 dekr--)
		Kraj(stanje igrac ind+n*n dekr--)
		Kraj(stanje igrac ind+n*n+1 dekr--)
		)
		
	}
	
for i,n*n {
	if((nth i stanje) != '-')
	Kraj(stanje (nth i stanje) i 4 )
}
	
(defun kraj stanje
  (loop for i from 0 to (* n n) do
        (if (not(equal (nth i stanje) '-)) (kr stanje (nth i stanje) i 4) )
        )
  )
                                         
          
(defun kr (stanje igrac ind dekr)
  (cond ((> ind (* n n n)) '())
        ((< ind 0) '())
        ((equal (nth ind stanje) igrac)
         (and 
          Kr(stanje igrac (+ ind 1) (- dekr 1)) ;gore
          Kr(stanje igrac (+ ind n) (- dekr 1)) ;vrsta
          Kr(stanje igrac (+ ind n 1)(- dekr 1)) ;vrsta dijagonalno
          Kr(stanje igrac (+ ind (* n n )) (- dekr 1)) ;kolona
          Kr(stanje igrac (+ ind (* n n ) 1) (- dekr 1))
          )
         )
        (t '())
  )
(4connect)

;;test

