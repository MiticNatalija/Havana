(load "Inference_engine.cl")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Napomena: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Funkcija start() iz faza 1 i 2 je zamenjena funkcijama koje slede (sve izmedju * i *)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pocetak igre
(defun havannah ()
     (format t "~%Dobrodosli u igru Havannah! :)~%Unesite duzinu stranice sestougaone table:")
     ;(setq igracNapotezu 'x)
     (setq duzina (read))
            (loop
                (if (checkDuzina) 
                    (return ) 
                       ( prog1
						(format t "~%Uneta je nevalidna dimenzija. Unesite zeljenu dimenziju ponovo.~%")
						(setq duzina (read)))  ; unosi sve dok dimenzija ne bude validna
                )
            )

    (setq lista (initLista))
    
    (let* (
            (racunar (progn (format t "~%Unesite r/c u zavisnosti od toga ko igra prvi, racunar/covek. ~%U slucaju nekorektnog unosa, smatra se da covek igra prvi.~% :")(read)))
            (auto (if (equal racunar 'r) t '()))
            (igrac t)
            (dubina (progn (format t "~%Unesite dubinu pretrazivanja:")(read)))
          )
    (printTabla)
    (igraj lista igrac auto dubina)
    )
)

; funkcija za regulisanje toka igre, 
; poziva se funkcija za unos poteza, ukoliko igra covek ili minimax ukoliko igra racunar, 
; kao i funkcija koja proverava da li se doslo do kraja igre
(defun igraj (stanje igrac auto dubina)
    (let* (
            (staroStanje stanje)
            (nstanje (if auto (car (minimax staroStanje stanje dubina igrac -500 500))
                              (unesi stanje igrac)))  
          )
          
    (progn (setq lista nstanje) (printTabla)
           (if (not (daLiJeKraj staroStanje nstanje (if igrac 'x 'o))) (let* ( (staroSStanje nstanje)(nnstanje (if auto (unesi nstanje (not igrac))
                                                                    (car (minimax staroSStanje nstanje dubina (not igrac) -500 500)))))
                                            (progn (setq lista nnstanje)(printTabla)
                                                   (if (not (daLiJeKraj staroSStanje nnstanje  (if igrac 'o 'x)))(igraj nnstanje igrac auto dubina) (format t "~%Kraj igre!Pobedio je igrac ~a" (if igrac 'o 'x)) )
    ))
    (format t "~%Kraj igre! Pobedio je igrac ~a." (if igrac 'x 'o)))))
)
; funkcija za unos poteza od strane coveka
(defun unesi (stanje igrac)
  (format t "~%Potez igrate unosom pozicije polja koje zelite da zauzmete, u obliku (C 3).~%")
    
           (setq potez (read))
           (loop
                (if (checkUnos) 
                    (return )  ; ako je potez true, validna pozicija, ne vrti se ovde
                       ( prog1
						(format t "~%Unet je nevalidan potez. Unesite zeljeni potez ponovo.~%")
						(setq potez (read)))  ; nevalidna pozicija, unosi potez opet sve dok ne bude validan
                )
            )
          
            (ubaciigrac (convertKljuc potez) (if igrac 'x 'o) lista) 
)
; funkcija koja ispituje da li se postiglo neko od ciljnih stanja
(defun daLiJeKraj (staroStanje novoStanje igracc)

    (let* (( poslednjiPotez (nadjiPoslednjiPotez staroStanje novoStanje) ))
    
    (cond ((testMost poslednjiPotez igracc) '0)
          ((testVila poslednjiPotez igracc) '0)
          ((testPrsten poslednjiPotez igracc) '0)
          (t '())
    )
    )
)
; funkcija koja nalazi poslednji uneti potez
; uvedena zbog funkcija testMost, testVila, testPrsten cija implementacija zahteva postojanje poslednjeg unetog poteza
(defun nadjiPoslednjiPotez (staroStanje novoStanje)
    (cond ((null staroStanje) '())
         ((equal (car staroStanje)(car novoStanje)) (nadjiPoslednjiPotez (cdr staroStanje) (cdr novoStanje)) )
         (t (caar novoStanje))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; proverava ispravnost unete duzine
(defun checkDuzina ()
    (if (numberp duzina) 
      (if (and (> duzina 2) (< duzina 13) ) t) 
      '()
    )
)
; inicijalizacija liste
(defun initLista ()
     (append (initPom1 '0 '0 ) (initPom2 duzina '1 )  )
)
; pomocna funkcija za inicijalizaciju gornje polovine table
(defun initPom1 (i j)
    (cond ((and (= i (- duzina 1) ) (= j (+ duzina i))) '())
          ((= j  (+ duzina i) ) (initPom1 (+ i 1) '0  ) )
          (t (cons (cons (cons  i  (list j)) (list '-)) (initPom1 i (+ j 1) )))
    )
)
; pomocna funkcija za inicijalizaciju donje polovine table
(defun initPom2 (i j)
    (cond  ((and (= i ( - (* duzina 2) 2) ) (= j ( - (* duzina 2) 1)  ) ) '() ) 
          ((= j  ( - (* duzina 2) 1) ) (initPom2 (+ i 1) (+ 2 (- i duzina)) ) )
          (t (cons (cons (cons i (list j)) (list '-)) (initPom2 i (+ j 1)  )))
    )
)
; stampa tablu 
(defun printTabla ()
    (printIndeksi )
    (loop for i from 0 to (- (* 2 duzina) 2)
    do(
        printRed i 
    )
    )
    (format t "~%~%")
)
; pomocna funkcija za stampanje indeksa prvih n kolona (iznad table)
(defun printIndeksi ()
    (format t "~%")
    (printRazmak duzina)
    (loop for i from 0 to (- duzina 1)
        do (format t "~a " i)
    )
)
; pomocna funkcija za stampanje i-tog reda zajedno sa indeksima reda i kolone
(defun printRed (i)
    (format t "~%")
    (format t "~a" (code-char (+ i 65)))
    (if (< i duzina) (printRazmak (- (- duzina i) 1)) (printRazmak (+ (- i duzina) 1)))
    (format t "~{~a ~}" (vratiRed i lista))
    
    (if (< i (- duzina 1)) (format t "~a" (+ duzina i)))
)
; pomocna funkcija za stampanje table u trazenom formatu
(defun printRazmak(b)
    (cond ((zerop b)'())
          (t (format t " " ) (printRazmak (- b 1)))
    )
)
; vraca listu vrednosti i-tog reda liste l
(defun vratiRed (i l)
    (cond ((null l) '())
    ((= i (caaar l)) (append (cdar l) (vratiRed i (cdr l))))
    (t(vratiRed i (cdr l)))
    )
)
; proverava ispravnost unetog poteza
(defun checkUnos ()
    (and (checkElTabla (convertKljuc potez ))
    (checkDostupnost (convertKljuc potez ) lista))
)


; provera da li je uneti potez (kljuc) u okviru granica table
;dodat uslov za negativne vrednosti - prvi red
(defun checkElTabla (el)
	(cond ((or (< (car el) 0) (< (cadr el) 0) )  '()) 
    ((> (car el) (- (* duzina 2) 2)) '())
	(   (< (car el) duzina)   (if (> (cadr el) (+ (1- duzina) (car el)))  '() t))
	(t (if (or (< (cadr el) (+ 1 (- (car el) duzina))) (> (cadr el) (- (* 2 duzina) 2))) '() t)))
)

; pomocna funkcija koja za kljuc k u listi l proverava da li je polje prazno ('-)
(defun checkDostupnost (k l)
    (if (equal k (caar l) ) (if (equal (cadar l) '- ) t '()) (checkDostupnost k (cdr l) ))
)
; funckija koja za kljuc k u listi l postavlja vrednost v ('x ili 'o)
(defun ubaciigrac (k v l)
    (cond ((not (checkElTabla k)) '()) ; ako je pozicija nevalidna, izadji
        ((equal k (caar l))  
                ( if (equal (cadar l) '-) ; ako je pozicija prazna, ubaci v kao vrednost
                        (setq lista (cons (cons (caar l) (list v)) (cdr l) )) 
                        (setq lista l)))  ; u suprotnom, lista ostaje ista
            (t (setq lista (cons (car l) (ubaciigrac k v (cdr l))) ))
)
 
)
; od poteza u obliku (A 0) unesenog sa tastature pravi kljuc u obliku (0 0)
(defun convertKljuc (potez)
   (append (list ( - (char-int (character (car potez)))  65) ) (list (char-int (character (cadr potez)) )  ))
)

;(start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;FAZA II;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;TRAZENJE;;;;;;;;;;;;
 ;iz argumenta potomci izdvaja one elemente koji nisu u argumentu cvorovi
 (defun novi-cvorovi (potomci cvorovi)
	(cond 
		((null potomci) '())
		((clan (car potomci) cvorovi)	(novi-cvorovi (cdr potomci) cvorovi))
		(t (cons (car potomci) (novi-cvorovi (cdr potomci) cvorovi)))
	)
 )

;vraca potomke od arg. cvor koji nisu obradjeni
(defun dodaj-potomke (cvor cvorovi vrednost)
	(let
    ((potomci (susedi cvor '0 vrednost)))
	(if (null potomci) '()  (novi-cvorovi potomci cvorovi) ))
)
;vraca put od stratnog cvora do cilja
;navodi se: (nadji-put '((3 1))  '(4 2) '() 'x) gde je (3 1) startni cvor
(defun nadji-put (l cilj cvorovi vrednost) 

	 (cond 
		((null l) '())
		((equal (car l) cilj) (list cilj))
		(t 
			(let* 
				(
					;dodaje se onaj koji se trenutno obradjuje u obradjene
					(cvorovi1 (append cvorovi (list (car l)))) 
				     ;dodaju se potomci, odnosno susedi koji nisu vec obradjeni
					(potomci1 (dodaj-potomke (car l) cvorovi1 vrednost))
					;izbacuje se prvi el iz neobradjenih,jer smo ga upravo obradili
					(l1 (append potomci1 (cdr l)))
				;pravi put
					(nadjeni-put (nadji-put  l1 cilj cvorovi1 vrednost))  
				)
				(cond 
					((null nadjeni-put) '())
					((clan (car nadjeni-put) potomci1) (cons (car l) nadjeni-put))
					(t nadjeni-put)
				)
			)
		)
	 )
 )	

;vraca susede zadatog cvora i to samo one koji imaju istu vrednost kao zadati cvor
(defun susedi (stanje brojac vrednost)
    
    (let* ((i (car stanje)) (j (cadr stanje)))
    (cond ((= brojac 6) '())
    ((= brojac 0) (if (and (checkElTabla (list (1+ i) j)) (checkigrac (list (1+ i) j) lista vrednost)) (cons (list (1+ i) j) (susedi stanje 1 vrednost) ) (susedi stanje 1 vrednost) )) 
    ((= brojac 1) (if (and (checkElTabla (list (1- i) j)) (checkigrac  (list (1- i) j) lista vrednost)) (cons (list (1- i) j) (susedi stanje 2 vrednost )) (susedi stanje 2 vrednost) ))
    ((= brojac 2) (if (and (checkElTabla (list i (1+ j))) (checkigrac (list i (1+ j)) lista vrednost)) (cons (list i (1+ j)) (susedi stanje 3 vrednost) ) (susedi stanje 3 vrednost) ))
    ((= brojac 3) (if (and (checkElTabla (list i (1- j))) (checkigrac (list i (1- j)) lista vrednost)) (cons (list i (1- j)) (susedi stanje 4 vrednost) ) (susedi stanje 4 vrednost)))
    ((= brojac 4) (if (and (checkElTabla (list (1- i) (1- j))) (checkigrac (list (1- i) (1- j)) lista vrednost)) (cons (list (1- i) (1- j)) (susedi stanje 5 vrednost) ) (susedi stanje 5 vrednost)))
    ((= brojac 5) (if (and (checkElTabla (list (1+ i) (1+ j))) (checkigrac (list (1+ i) (1+ j)) lista vrednost)) (cons (list (1+ i) (1+ j)) (susedi stanje 6 vrednost) ) (susedi stanje 6 vrednost) )) 
    )    
))
;proverava da li je na zadatom mestu zadata vrednost
(defun checkigrac (k l vrednost)
    (if (equal k (caar l) ) (if (equal (cadar l) vrednost ) t '()) (checkigrac k (cdr l) vrednost ))
)


;pomocna fja, proverava da li element pripada listi
 (defun clan (el lista)
	(cond
		((null lista) '())
		((if (equal el (car lista)) t (clan el (cdr lista))))
	)
 )


;;;;;;;;;;;;;;;;;;;;MOST;;;;;;;;;;;;;;;;;;;;;;;;;;;
;proverava da li je dobijen most
(defun testMost (potez vrednost)
 (let* 
  (  
	  (j1 (- duzina 1))
	  (j2 (- (* duzina 2) 2))
	  (put1 (nadji-put (list potez) '(0 0) '() vrednost)) 
	  (put2 (nadji-put (list potez) (list '0 j1) '() vrednost))
	  (put3 (nadji-put (list potez) (list j1 '0) '() vrednost)) 
	  (put4 (nadji-put (list potez) (list j1 j2) '() vrednost)) 
	  (put5 (nadji-put (list potez) (list j2 j1) '() vrednost)) 
	  (put6 (nadji-put (list potez) (list j2 j2) '() vrednost)) 
	 )
	(let* ((brojacInkrement 0))
	(if (not (null put1)) (incf brojacInkrement))
	(if (not (null put2)) (incf brojacInkrement))
	(if (not (null put3)) (incf brojacInkrement))
	(if (not (null put4)) (incf brojacInkrement))
	(if (not (null put5)) (incf brojacInkrement))
	(if (not (null put6)) (incf brojacInkrement))
	(if (> brojacInkrement 1) t '())

 )
 )
)


;;;;;;;;;;;;;;;;;;;;;;VILA;;;;;;;;;;;;;;;;;;

; na osnovu liste svih elemenata l vraca listu elemenata koji se nalaze na omotacu table, na stranama
; ii je prilikom prvog poziva = 1 a d je duzina stranice table
(defun listaOmotac (l ii d)
	(cond 
		( (null l) '() )
		((equal (caaar l) '0) (cons (caar l) (listaOmotac (cdr l) ii d )))
		((equal (caaar l) (- (* 2 duzina) 2) ) (cons (caar l) (listaOmotac (cdr l) ii d)))
		((equal (cadaar l) '0) (cons (caar l) (listaOmotac (cdr l) ii d)))
		((equal (cadaar l) (- (* 2 duzina) 2) )  (cons (caar l) (listaOmotac (cdr l) ii d)))

		((and (< ii (- duzina 2))(equal (caaar l) ii) (equal (cadaar l) d )) (cons (caar l) (listaOmotac (cdr l) (1+ ii) (1+ d))))
		((and (= ii (- duzina 2))(equal (caaar l) ii) (equal (cadaar l) d )) (cons (caar l) (listaOmotac (cdr l) (+ ii 2) 1 )))
		((and (> ii (- duzina 2))(equal (caaar l) ii) (equal (cadaar l) d )) (cons (caar l) (listaOmotac (cdr l) (1+ ii) (1+ d))))
		(t (listaOmotac (cdr l) ii d))
	)
)

; za prosledjenu listu elemenata omotaca, vraca listu elemenata koji su na omotacu a nisu temena
(defun omotacBezTemena(l)
	(cond 
		((null l) '())
		((equal (car l) '(0 0) ) (omotacBezTemena (cdr l) ) )
		((equal (car l) (list '0 (- duzina 1)) ) (omotacBezTemena (cdr l) ) )
		((equal (car l) (list (- duzina 1) '0) ) (omotacBezTemena (cdr l) ) )
		((equal (car l) (list (- duzina 1) (- (* duzina 2) 2)) ) (omotacBezTemena (cdr l) ) )
		((equal (car l) (list (- (* duzina 2) 2) (- duzina 1)) ) (omotacBezTemena (cdr l) ) )
		((equal (car l) (list (- (* duzina 2) 2) (- (* duzina 2) 2)) ) (omotacBezTemena (cdr l) ) )
		(t (cons (car l) (omotacBezTemena (cdr l)) ))
	)
)

; poziva testVilaPom koja zapravo proverava da li se doslo do ciljnog stanja
(defun testVila (potez vrednost)
    (testVilaPom potez vrednost (omotacBezTemena (listaOmotac lista 1 duzina)) '(0 0 0 0 0 0) 0)
)

;proverava da li je dobijena vila
;trazi put do svakog elementa svake stranice - bez temena i potrebno je da postoje putevi do bar 3 elementa koja pripadaju razlicitim stranama
(defun testVilaPom (potez vrednost l putevi brojacInkrement )
	(cond 
		((> brojacInkrement 2) t)
		((null l) '())
 		(t (let* 
 		 (  
	 		(put (nadji-put (list potez) (car l) '() vrednost))
            (put1 (if (and (not (null put))(equal (caar l) 0)) 1 0))
            (put2 (if (and (not (null put))(equal (caar l) (- (* 2 duzina) 2))) 1 0 ))
            (put3 (if (and (not (null put))(equal (cadar l) 0)) 1 0))
            (put4 (if (and (not (null put))(equal (cadar l) (- (* 2 duzina) 2))) 1 0))
            (put5 (if (and (not (null put))(= put1 0)(= put3 0)(< (caar l) (- duzina 1))) 1 0))
            (put6 (if (and (not (null put))(= put2 0)(= put4 0)(> (caar l) (- duzina 1))) 1 0))
            (putevi (bitwiseOR putevi (cons put1 (cons put2 (cons put3 (cons put4 (cons put5 (cons put6 '()))))))))
            (brojacInkrement (izbrojiJedinice putevi) )
	 		)
		(if  (< brojacInkrement 3) (testVilaPom potez vrednost (cdr l) putevi brojacInkrement ) t)
))))

; pomocna funkcija za operaciju or nad bitovima iz prosledjenih listi
(defun bitwiseOR (putevi1 putevi2)
    (cond 
      ((null putevi1) '())
      (t (let* 
            ( (prvi (if (equal (car putevi1) 1) t nil)) 
            (drugi (if (equal (car putevi2) 1) t nil)) )
          (append (if (or prvi drugi) (list '1) (list '0)) (bitwiseOR (cdr putevi1) (cdr putevi2)) )
          ))
    )
)

; pomocna funkcija koja vraca broj jedinica u listi 01001..
(defun izbrojiJedinice (putevi)
    (cond
        ((null putevi) 0)
        ((equal (car putevi) 1) (1+ (izbrojiJedinice (cdr putevi))))
        (t (izbrojiJedinice (cdr putevi)))
    )
)

;;;;;;;;;;;;PRSTEN;;;;;;;;;;;;;
;vraca potomke od arg. cvor koji nisu obradjeni, ali samo one koji nisu susedi prethodno obradjenog cvora
(defun dodaj-potomkePrsten (cvor cvorovi vrednost)
	(let* (
	(potomci (susedi cvor '0 vrednost))
	(potomci1 (if (> (length cvorovi) '1) (proveriPotomke potomci (car (cdr cvorovi)) vrednost) potomci))
	(potomci2 (if (< (length cvorovi) '5) (proveriPotomke potomci1 (car (last cvorovi)) vrednost )  potomci1))
	)
	(if (null potomci2) '()  (novi-cvorovi potomci2 cvorovi) )
	 )
)

;vraca listu iz potomci koji nisu potomci od cvora cvor
(defun proveriPotomke(potomci cvor vrednost)
	(cond 
	((null potomci) '() )
	((clan (car potomci)  (susedi cvor '0 vrednost))  (proveriPotomke (cdr potomci) cvor vrednost))
	(t(cons (car potomci) (proveriPotomke (cdr potomci) cvor vrednost)))
	)
)

;mala izmena u odnosu na originalni nadji-put
(defun nadji-putPrsten (l cilj cvorovi vrednost)  
	 (cond 
		((null l) '())
		((equal (car l) cilj) (list cilj))
		(t 
			(let* 
				(
					;ovde je promenjeno da se na pocetak ubacuje
					(cvorovi1 (append  (list (car l)) cvorovi )) 
					(potomci1 (dodaj-potomkePrsten (car l) cvorovi1 vrednost))
					(l1 (append potomci1 (cdr l)))
					(nadjeni-put (nadji-putPrsten  l1 cilj cvorovi1 vrednost ))  
				)
				(cond 
					((null nadjeni-put) '())
					((clan (car nadjeni-put) potomci1) (cons (car l) nadjeni-put))
					(t nadjeni-put)
				)
))))	


;lista parova nesusednih cvorova,gde su s svi susedi nekog cvora,pa se odatle izdvaja
(defun nesusedniSusedi (s vrednost) 
(cond ((null s) '())
      (t (append (nesusedno (car s) (cdr s) vrednost)  (nesusedniSusedi (cdr s) vrednost)))
)
)

(defun nesusedno(el lista vrednost) ;pomocna za nesusedniSusedi, obradjuje jedan element
(cond
    ((null lista) '())
	((not(clan el (susedi (car lista) '0 vrednost)))  (cons (list el (car lista))  (nesusedno el (cdr lista) vrednost) ))
	(t (nesusedno el (cdr lista) vrednost))
))

;za svaki par nesusednih suseda polaznog cvora potez trazi put koji je prsten (obezbedjeno u fji nadji-putPrsten)
(defun nadjiPrsten (potez nesus vrednost)
	(cond ((null nesus) '())
	((not (null (nadji-putPrsten (list (caar nesus)) (cadar nesus) (list potez) vrednost ))) t ) ;poziv fje: 1. cvor suseda je polazni, 2. cvor suseda je ciljni, a odigrani potez se stavlja u obradjene
	(t ( nadjiPrsten potez (cdr nesus) vrednost) )
	)
)

;pocetna fja koja nalazi sve parove nesusednih za pocetni cvor i zove dalje fje
(defun testPrsten (potez vrednost)
    (if (< (length (susedi potez '0 vrednost)) '2) '()
                    (let* ( (sus (susedi potez '0 vrednost)) (nesus (nesusedniSusedi sus vrednost))  )(nadjiPrsten potez nesus vrednost)))
)

;;;;;;;;;;;;;;;;;;;;;;;OPERATORI PROMENE STANJA;;;;;;;;;;;;;;;;;
; Funkcija iz tacke 1 sa slajda
(defun novoStanjeigrac (k v l)
	; ubaci x/o na k ali ne menja listu nego samo vrati ono novo stanje, sa unetim x/o
    (cond 
        ((null l) '())
        ((not (checkElTabla k)) '())
        ((equal k (caar l))  
                ( if (equal (cadar l) '-)
                        (cons (cons (caar l) (list v)) (cdr l) )
                        l))  ; u suprotnom, lista ostaje ista
        (t (cons (car l) (novoStanjeigrac k v (cdr l))) )
    )
)


; Funkcija iz tacke 2 sa slajda
(defun listaPotencijalnihStanja (l v prazni)
    (cond   
        ((null prazni) '())
        (t (cons (novoStanjeigrac (car prazni) v l)  (listaPotencijalnihStanja l v (cdr prazni)) ))
    )
)

; za prosledjenu listu vraca listu praznih polja
(defun listaPraznihPolja (l)
    (cond 
          ((null l) '())
          ((equal (cadar l) '-) (cons (caar l) (listaPraznihPolja (cdr l)) ))
          (t (listaPraznihPolja (cdr l)) )
    )
)

;broj slobodnih mesta za potez
;(defun slobodnaMesta (l)
  ;  (cond ((null l) 0 )
  ;  ((equal (cadar l) '-) (1+ (slobodnaMesta (cdr l))))
   ; (t (slobodnaMesta (cdr l)))
  ;  )
;)

;(trace testMost)
;(trace nadji-put)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FAZA III  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun minimax (staroStanje stanje dubina moj-potez alfa beta)
(cond
    ((zerop dubina) (list stanje (heuristika stanje moj-potez)))
    (t
     (setq lp (listaPotencijalnihStanja stanje (if moj-potez 'x 'o) (listaPraznihPolja stanje)))
     (cond
        ((null lp) (list stanje (heuristika stanje moj-potez)))
        (t (if moj-potez (igraMax '() alfa beta dubina lp moj-potez) (igraMin '() alfa beta dubina lp moj-potez))))
)))

;alfa-beta odsecanje, max igrac
(defun igraMax (stanje alfa beta dubina lp moj-potez)
    (cond
    ((null lp) (list stanje alfa))
    (t
        (let* 
            (
                (minStanje (minimax stanje (car lp) (- dubina 1) (if moj-potez 'x 'o) alfa beta))
                (a1 (apply 'max (list alfa (cadr minStanje))))
                (stanje1 (if (> a1 alfa) (car lp) stanje))
            )
         (if (< a1 beta) (igraMax stanje1 a1 beta dubina (cdr lp) moj-potez ) (list stanje1 a1))
            )
    )
    ) )

;alfa-beta odsecanje, min igrac
(defun igraMin (stanje alfa beta dubina lp moj-potez)
    (cond
            ((null lp) (list stanje beta))
    (t
        (let* 
            (
                (maxStanje (minimax stanje (car lp) (- dubina 1) (if moj-potez 'x 'o) alfa beta))
                (b1 (apply 'min (list beta (cadr maxStanje))))
                (stanje1 (if (< b1 beta) (car lp) stanje))
            )
            (if (> b1 alfa) (igraMin stanje1 alfa b1 dubina (cdr lp) moj-potez ) (list stanje1 b1))
        ))) )

;(defun proceni-stanje (stanje) 
 ;   (- 10 (random 20))
;)


(defun max-stanje (lsv)
  (max-stanje-i (cdr lsv) (car lsv)))

(defun max-stanje-i (lsv stanje-vrednost)
    (cond ((null lsv) stanje-vrednost)
        ((> (cadar lsv) (cadr stanje-vrednost))(max-stanje-i (cdr lsv) (car lsv)))
        (t (max-stanje-i (cdr lsv) stanje-vrednost))
    ))

(defun min-stanje (lsv)
 (min-stanje-i (cdr lsv) (car lsv))) 

(defun min-stanje-i (lsv stanje-vrednost)
    (cond ((null lsv) stanje-vrednost)
            ((< (cadar lsv) (cadr stanje-vrednost))(min-stanje-i (cdr lsv) (car lsv)))
            (t (min-stanje-i (cdr lsv) stanje-vrednost))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  IV FAZA  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;pomocne funkcije

;na osnovu trenutnog stanja, treba da vidi koja su prazna polja i da za svako od njih ispita vodi li u ciljno stanje protivnika
(defun !protivnikUPobedu (stanje igrac)

(let* ((praznapolja (listaPraznihPolja stanje))
        (trazenopolje (proveriPoljeZaPobedu praznapolja igrac)) );dobijemo sva prazna polja
        trazenopolje
)
)
;da li odigrano polje vodi u pobedu
(defun proveriPoljeZaPobedu(listapraznih igrac) ;vrati potez (1 1) koji vodi do pobede tog igraca
(cond
    ((null listapraznih) '())
    ((or (testMost (car listapraznih) igrac) (testVila (car listapraznih) igrac) (testPrsten (car listapraznih) igrac)) (car listapraznih))
    (t (proveriPoljeZaPobedu (cdr listapraznih) igrac))
)
)
;da li postoje susedi istog znaka
(defun !imaSusede (polje igrac)
  (if (null (susedi polje '0 igrac)) '() t )
)
(defun !proveraKrajCinjenica (vrsta kolona igrac)
    (let* 
    ((potez (cons vrsta (list kolona))))
    (or (testMost potez igrac) (testVila potez igrac) (testPrsten potez igrac))
    )
)

(defun !daLiJeTeme (vrsta kolona)
(cond 
    ((and (equal vrsta 0) (equal kolona 0)) t)
    ((and (equal vrsta 0) (equal kolona (1- duzina))) t)
    ((and (equal vrsta (1- duzina)) (equal kolona 0)) t)
    ((and (equal vrsta (1- duzina)) (equal kolona (- (* 2 duzina) 2))) t)
    ((and (equal vrsta (- (* 2 duzina) 2)) (equal kolona (1- duzina))) t)
    ((and (equal vrsta (- (* 2 duzina) 2)) (equal kolona (- (* 2 duzina) 2))) t)
    (t '())
))
;generise cinjenice od stanja
(defun generisiCinjenice (stanje)
  (cond ((null stanje) '())
    (t (let ((pom (cinjenicePom(car stanje))))
    (if (null pom) (generisiCinjenice (cdr stanje)) (cons (cinjenicePom (car stanje)) (generisiCinjenice (cdr stanje))))))))

(defun cinjenicePom (el)
  (cond ((null el) '())
        (t (cond ((equalp (car(reverse el))  'x) (list 'x (caar el) (cadar el)))
                  ((equalp (car(reverse el)) 'o) (list 'o (caar el) (cadar el)))
                  (t '())))))


 
(defun !eq (a b)
  (equal a b))
  (defun !manje (m n)
  (if (< m n) t '()))

(defun !vece (m n)
  (if (> m n) t '()))
  (defun =plus (a b)
    (+ a b)
  )
  (defun =minus (a b)
    (- a b)
  )
  (defun !ne (m n)
  (if (not (equal m n))  t '())
  )
  (defun =trenutnoStanje ()
  hstanje
  )
  (defun !spoji (a b)
  (cons a (list b))
  )                 

;(setq duzina 3)
;(setq lista '(((0 0) o)  ((0 1) o) ((0 2) o) ((1 0) x) ((1 1) -) ((1 2) -) ((1 3) -) ((2 0) -) ((2 1) -) ((2 2) -) ((2 3) -) ((2 4) o) ((3 1) x) ((3 2) -) ((3 3) -) ((3 4) -) ((4 2) -) ((4 3) -) ((4 4) o)         ))
;(print (generisiCinjenice lista))

(defparameter *T1-RULES*
  '(	
     
      (if (and (x ?a ?b) (!daLiJeTeme ?a ?b)) then (temeX))
      (if (and (o ?a ?b) (!daLiJeTeme ?a ?b)) then (temeO))
   

    (if (and (x ?a ?b)  (!eq ?a 0) (!ne ?b 0) (!ne ?b (1- duzina)) ) (strana1X) ) 
    (if (and (o ?a ?b)  (!eq ?a 0) (!ne ?b 0) (!ne ?b (1- duzina)) ) (strana1O) ) 
    (if (and (x ?a ?b)  (!manje ?a  (1- duzina) ) (!ne ?a 0) (!eq ?b (=plus ?a (1- duzina))) ) (strana2X) )
    (if (and (o ?a ?b)  (!manje ?a  (1- duzina) ) (!ne ?a 0) (!eq ?b (=plus ?a (1- duzina))) ) (strana2O) )
    (if (and (x ?a ?b)  (!vece ?a  (1- duzina) ) (!ne ?a (- (* 2 duzina) 2)) (!eq ?b (- (* 2 duzina) 2)) ) (strana3X) ) 
    (if (and (o ?a ?b)  (!vece ?a  (1- duzina) ) (!ne ?a (- (* 2 duzina) 2)) (!eq ?b (- (* 2 duzina) 2)) ) (strana3O) )      
    (if (and (x ?a ?b)  (!eq ?a (- (* 2 duzina) 2)) (!ne ?b (- (* 2 duzina) 2)) (!ne ?b (1- duzina)) ) (strana4X) )
    (if (and (o ?a ?b)  (!eq ?a (- (* 2 duzina) 2)) (!ne ?b (- (* 2 duzina) 2)) (!ne ?b (1- duzina)) ) (strana4O) )
    (if (and (x ?a ?b)  (!vece ?a  (1- duzina) ) (!ne ?a (- (* 2 duzina) 2)) (!eq ?b (=minus ?a (1- duzina)) )) (strana5X) ) 
    (if (and (o ?a ?b)  (!vece ?a  (1- duzina) ) (!ne ?a (- (* 2 duzina) 2)) (!eq ?b (=minus ?a (1- duzina)) )) (strana5O) )
    (if (and (x ?a ?b)  (!manje ?a  (1- duzina) ) (!ne ?a 0) (!eq ?b 0) ) (strana6X) ) 
    (if (and (o ?a ?b)  (!manje ?a  (1- duzina) ) (!ne ?a 0) (!eq ?b 0) ) (strana6O) ) 

     (if (and (x ?a ?b)  (!eq ?a 1)  ) (uzStranicu_goreX) ) 
      (if (and (x ?a ?b)  (!eq ?a (- (* 2 duzina) 3))  ) (uzStranicu_doleX) )
      (if (and (x ?a ?b)  (!manje ?a  (1+ duzina) ) (!eq ?b (=plus ?a (- duzina 2) )) ) (uzStranicu_desno_goreX) ) 
      (if (and (x ?a ?b)  (!manje ?a  (1+ duzina) )  (!eq ?b 1) ) (uzStranicu_levo_goreX) ) 
      (if (and (x ?a ?b)  (!vece ?a  (- duzina 3) ) (!eq ?b (- (* 2 duzina) 3)) ) (uzStranicu_desno_doleX) ) 
      (if (and (x ?a ?b)  (!vece ?a  (- duzina 3) ) (!eq ?b (=minus ?a (- duzina 2) )) ) (uzStranicu_levo_doleX) ) 

      (if (and (o ?a ?b)  (!eq ?a 1)  ) (uzStranicu_goreO) ) 
      (if (and (o ?a ?b)  (!eq ?a (- (* 2 duzina) 3))  ) (uzStranicu_doleO) )
      (if (and (o ?a ?b)  (!manje ?a  (1+ duzina) ) (!eq ?b (=plus ?a (- duzina 2) )) ) (uzStranicu_desno_goreO) ) 
      (if (and (o ?a ?b)  (!manje ?a  (1+ duzina) )  (!eq ?b 1) ) (uzStranicu_levo_goreO) ) 
      (if (and (o ?a ?b)  (!vece ?a  (- duzina 3) ) (!eq ?b (- (* 2 duzina) 3)) ) (uzStranicu_desno_doleO) ) 
      (if (and (o ?a ?b)  (!vece ?a  (- duzina 3) ) (!eq ?b (=minus ?a (- duzina 2) )) ) (uzStranicu_levo_doleO) )  

      (if (and (x ?a ?b) (!proveraKrajCinjenica ?a ?b 'x))  then (krajX)) 
      (if (and (o ?a ?b) (!proveraKrajCinjenica ?a ?b 'o)) then (krajO))  
      ;(if (and (x ?a ?b) (!eq (!spoji ?a ?b) (!protivnikUPobedu (=trenutnoStanje) 'o)) then (blokirajO)))
      ;(if (and (o ?a ?b) (!eq (!spoji ?a ?b) (!protivnikUPobedu (=trenutnoStanje) 'x)) then (blokirajX)))
    
      (if (and (x ?a ?b) (!proveraKrajCinjenica ?a ?b 'o))  then (blokX)) 
      (if (and (o ?a ?b) (!proveraKrajCinjenica ?a ?b 'x)) then (blokO)) 

      (if (and (x ?a ?b)  (!imaSusede (!spoji ?a ?b) 'x) ) (imaSusedeX) )
      (if (and (o ?a ?b)  (!imaSusede (!spoji ?a ?b) 'o) ) (imaSusedeO) )
      (if (and (x ?a ?b)  (!imaSusede (!spoji ?a ?b) 'o) ) (imaProtivnikeX) )
      (if (and (o ?a ?b)  (!imaSusede (!spoji ?a ?b) 'x) ) (imaProtivnikeO) )
      
      (if (x ?a ?b) then (igracX))
      (if (o ?a ?b) then (igracO))
   )
)

(defun heuristika (stanje igrac)
   (setq *T1-FACTS* (generisiCinjenice stanje))
 
   (prepare-knowledge *T1-RULES* *T1-FACTS* 3)
   (cond 
      (igrac 
       (+
       
         (+ (* (count-results '(igracX)) 1) (* (count-results '(blokX)) 50)(* (count-results '(krajX)) 100)
      
         (*(count-results '(imaSusedeX)) 1)
         (*(count-results '(imaProtivnikeO)) 1)
        (*(count-results '(temeX)) 7)
         (*(count-results '(strana6X) ) 2 ) (*(count-results '(strana5X) ) 2 ) (*(count-results '(strana3X) ) 2 ) (*(count-results '(strana2X) ) 2 ) (*(count-results '(strana4X) ) 2 ) (*(count-results '(strana1X) ) 2 )
         (*(count-results '(uzStranicu_levo_goreX) ) 3 )(*(count-results '(uzStranicu_goreX) ) 3 ) (*(count-results '(uzStranicu_doleX) ) 3 )(*(count-results '(uzStranicu_levo_doleX) ) 3 )(*(count-results '(uzStranicu_desno_doleX) ) 3 )(*(count-results '(uzStranicu_desno_goreX) ) 3 )
        
         )
        
          (+ (* (count-results '(igracO)) -1)  (* (count-results '(krajO)) -100) (* (count-results '(blokO)) -50)
       
          (*(count-results '(imaSusedeO)) -1)
          (*(count-results '(imaProtivnikeX)) -1)
          (*(count-results '(temeO)) -7)
         (*(count-results '(strana6O) ) -2 ) (*(count-results '(strana5O) ) -2 ) (*(count-results '(strana3O) ) -2 ) (*(count-results '(strana2O) ) -2 ) (*(count-results '(strana4O) ) -2 ) (*(count-results '(strana1O) ) -2 )
          (*(count-results '(uzStranicu_levo_goreO) ) -3 )(*(count-results '(uzStranicu_goreO) ) -3 ) (*(count-results '(uzStranicu_doleO) ) -3 )(*(count-results '(uzStranicu_levo_doleO) ) -3 )(*(count-results '(uzStranicu_desno_doleO) ) -3 )(*(count-results '(uzStranicu_desno_goreO) ) -3 )
        
         )
         )
       )
      (t 
   
          (+
           (+ (* (count-results '(igracO)) 1) (* (count-results '(blokO)) 50)(* (count-results '(krajO)) 100)
            (*(count-results '(imaSusedeO)) 1)
           
             (*(count-results '(imaProtivnikeX)) 1)
             (*(count-results '(temeO)) 7)
            (*(count-results '(strana6O) ) 2 ) (*(count-results '(strana5O) ) 2 ) (*(count-results '(strana3O) ) 2 ) (*(count-results '(strana2O) ) 2 ) (*(count-results '(strana4O) ) 2 ) (*(count-results '(strana1O) ) 2 )
            (*(count-results '(uzStranicu_levo_goreO) ) 3 )(*(count-results '(uzStranicu_goreO) ) 3 ) (*(count-results '(uzStranicu_doleO) ) 3 )(*(count-results '(uzStranicu_levo_doleO) ) 3 )(*(count-results '(uzStranicu_desno_doleO) ) 3 )(*(count-results '(uzStranicu_desno_goreO) ) 3 )
          
           )
           
           (+ (* (count-results '(igracX)) -1) (* (count-results '(krajX)) -100)(* (count-results '(blokX)) -50)
           (*(count-results '(imaSusedeX)) -1)
        
            (*(count-results '(imaProtivnikeO)) -1)
            (*(count-results '(temeX)) -7)
          (*(count-results '(strana6X) ) -2 ) (*(count-results '(strana5X) ) -2 ) (*(count-results '(strana3X) ) -2 ) (*(count-results '(strana2X) ) -2 ) (*(count-results '(strana4X) ) -2 ) (*(count-results '(strana1X) ) -2 )
           (*(count-results '(uzStranicu_levo_goreX) ) -3 )(*(count-results '(uzStranicu_goreX) ) -3 ) (*(count-results '(uzStranicu_doleX) ) -3 )(*(count-results '(uzStranicu_levo_doleX) ) -3 )(*(count-results '(uzStranicu_desno_doleX) ) -3 )(*(count-results '(uzStranicu_desno_goreX) ) -3 )
        
         )
          )
      )
   
   )
)
 


;(print (heuristika lista t))
;(print (heuristika lista '()))

(havannah)
