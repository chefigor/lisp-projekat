(defun Kreiraj_Tablu (n)
  (make-list (* n n n) :initial-element  '-))

(setq tabela (Kreiraj_Tablu 2))

(defun ista (i j stanje)
  (equal (nth i stanje)(nth j stanje)))

(defun puno (i stanje)
  (not (equal (nth i stanje) '-)))

(defun ima (potez i stanje)
  (equal (nth i stanje) potez))

(defun sledbenici (pred sled ind potez)
  (cond ((null sled) '())
        ((equal (car sled) '-)
         (cons (append pred (list potez) (cdr sled))
               (sledbenici (append pred (list(car sled)))
                           (cdr sled) (1+ ind) potez)))
         (t (sledbenici(append pred (list (car sled)))
                        (cdr sled) (1+ ind) potez)))))
;
(defun sledbenici1 (pred sled potez)
  (cond ((null sled) '())
        ((equal (car sled) '-)
         (cons (append pred (list potez) (cdr sled))
               (sledbenici1 (append pred (list(car sled)))
                           (cdr sled)  potez)))
         (t (sledbenici1 (append pred (list (car sled)))
                        (cdr sled) potez))))
;
(defun nova-stanja (stanje potez)
  (sledbenici1 '() stanje  potez))

; 
  
(nova-stanja (car(nova-stanja tabela 'x)) 'o)
        
;

(defun sva-stanja (tren sled potez)
  (cond ((null (car stanje) ) '())
        (t (sva-stanja ((nova-stanja (car stanje) potez) (if (equal potez 'x) 'o 'x))))))

(sva-stanja '(- - - - - - - -) 'o)


(setq n 4)

(defun stampaj-indekse (indeks n)
  (cond ((equal indeks n) '())
        (t ( if (< indeks 10) (format t "~a " indeks) (format t "~a " indeks))(stampaj-indekse (1+ indeks) n)))))

(defun stampaj-indekse (indeks n)
  (cond ((equal indeks n) '())
        (t ((format t "~a" indeks))))
(stampaj-indekse 0 16)
(stampaj-indekse 0 (* n n))

(defun zdravo2 (N) (format t "~%zdravo ~a" N))
(zdravo2 'Milane) 

(progn (stampaj-indekse 0 16) )

(defun stampaj (tabla hor ver ind)
  (cond ((> hor (+ n n -2)) '())
        ((> ver (- (* n n) 1 )) (format t "~%")(stampaj tabla (1+ hor) 0 0))
        ((< ind (* n n n))(stampaj tabla hor ver (1+ ind)))
        ;((and(equal (mod ind n) )(equal(mod 8 n) ver))(format t "~a" nth ind tabla)(stampaj tabla hor (1+ ver) 0))
        (t (princ "- ") (stampaj tabla hor (1+ ver) 0 ))
        )
)

(setq n 3)
(setq tabela (Kreiraj_Tablu n))
(stampaj tabela 0 0 0)

(untrace stampaj)

(defun st (tab)
  (format t "~%")(stampaj-indekse 0 (* n n))(format t "~%")(stampaj tab 0 0 0)(stampaj-indekse 0 (* n n)))
(st (car(nova-stanja tabela 'x)))
;(nova-stanja tabela 'x)














;;;;;;;;;;;;;; TEST;;;;;;;;;
(defun stampaj (tabla hor ver ind inkr)
  (cond ((< hor 0) '())
        ((>= (- hor inkr) n)(format t "  ")(stampaj tabla hor (1+ ver) 0 (mod (1+ inkr) n)))
        ((< (- hor inkr) 0)(format t "  ")(stampaj tabla hor (1+ ver) 0 (mod (1+ inkr)n)))
        ;((and (equalp (+(floor ind n)(mod ind n )) hor )(equalp (floor ind n) ver ))(format t "~a " (nth ind tabla))(stampaj tabla hor (1+ ver) 0 (mod (1+ inkr) n)))
        ((> ver (- (* n n) 1 )) (format t "~%")(stampaj tabla (- hor 1) 0 0 0))
        ;((< ind (* n n n))(stampaj tabla hor ver (1+ ind) inkr))
        (t (princ "- ") (stampaj tabla hor (1+ ver) 0 (mod(1+ inkr)n)))
        (t (format t"~a "(nth (+(* ver n)(- hor inkr)) tabla))(stampaj tabla hor(1+ ver)0(mod(1+ inkr)n)))
        )
  )



(defun st (tab)
  (format t "~%")
  (stampaj-indekse 0 (* n n))
  (format t "~%")
  (stampaj tab (+ n n -2) 0 0 0)
  (stampaj-indekse 0 (* n n))
)


(st (car(nova-stanja tabela 'x)))

(setq n 6)
(setq tabela (Kreiraj_Tablu n))

(untrace stampaj)
(trace stampaj)