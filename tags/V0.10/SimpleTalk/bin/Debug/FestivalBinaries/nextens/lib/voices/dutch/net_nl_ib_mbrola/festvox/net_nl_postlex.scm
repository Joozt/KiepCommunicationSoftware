;;; $Id: net_nl_postlex.scm,v 1.9 2003/04/23 13:30:30 joopk Exp $
;;;
;;; by Joop Kerkhoff & Erwin Marsi
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2003
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights Reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;;; postlexical rules

;; TODO
;; - rule for /t j/ ==> /c/

(define (nl::postlex_rules utt)
  (set! trace nil)

  ;; first assimilation rules (example "kies zelf")
  (mapcar
   (lambda (s)
     (if (string-equal (item.feat s "ph_vc") "-")
         (begin
           (if trace (format t "Starting assimilation... on segment: %s \n" 
			       (item.feat s "name")))
           (net_nl_fricative_rule s)
           (net_nl_coda_devoicing s)
           (net_nl_regressive_assim s))))
   (cdr (utt.relation.items utt 'Segment))
   )
   
  ;; then deletion rules (example "kies zelf")
  (mapcar
   (lambda (s)
     (if (string-equal (item.feat s "ph_vc") "-")
         (begin
           (if trace (format t "Starting assimilation... on segment: %s \n" 
			       (item.feat s "name")))
           (net_nl_n_deletion s)
           (net_nl_cc_deletion s))))
   (cdr (utt.relation.items utt 'Segment))
   )

    ;; remove empty SylPart elements 
  (net_nl_remove_empty_SylPart utt)

   ;; rewrite CGN to IB/NL3 diphones notation
  (net_nl_ib_rewrite_diphones utt)
  
  ;; remove empty SylPart elements 
  (net_nl_remove_empty_SylPart utt)
  
  ;; insert Glottal stop
  (net_nl_insert_glottal_stop utt)

 
utt); end net_nl_postlex_rules


;;; fricative rule
(define (net_nl_fricative_rule s)
  (let ((seg (item.name s)))
    (if trace (format t "Fricative rule: %s \n" seg ))
    (if (and
	 ;; seg is voiced fricative
	 (string-equal (item.feat s "ph_ctype") "f")
	 (string-equal (item.feat s "ph_cvox") "+")
	 ;; preceded by an unvoiced fricative or plosive
	 (or (string-equal (item.feat s "p.ph_ctype") "f")
	     (string-equal (item.feat s "p.ph_ctype") "p"))
	 (string-equal (item.feat s "p.ph_cvox") "-")
	 )
	(cond
	 ((string-equal seg "v") (item.set_name s "f"))
	 ((string-equal seg "z") (item.set_name s "s"))
	 ((string-equal seg "G") (item.set_name s "x"))
	 ))
    ))


;;; auslaut verhaertung; final devoicing
;;; word final; compound final
(define (net_nl_coda_devoicing s)
  (let ((seg (item.name s)))
    (if trace (format t "Coda_devoicing: %s \n" seg))
    (if (string-equal (item.feat s "R:ProsTree.parent.name") "Coda")
	(cond
	 ((string-equal seg "b") (item.set_name s "p"))
	 ((string-equal seg "d") (item.set_name s "t"))
	 ((string-equal seg "g") (item.set_name s "k"))
	 ((string-equal seg "G") (item.set_name s "x"))
	 ((string-equal seg "v") (item.set_name s "f"))
	 ((string-equal seg "z") (item.set_name s "s"))
	 ((string-equal seg "Z") (item.set_name s "S"))
	 ))
    ))


;; regressive assimilation (only the conversion to unvoiced segments)
(define (net_nl_regressive_assim s)
  (let ((seg (item.name s)))
    (if trace (format t "Regressive assimilation: %s \n" seg))
    (if (and
	 ;; seg is voiced fricative
	 (string-equal (item.feat s "ph_ctype") "f")
	 (string-equal (item.feat s "ph_cvox") "+")
	 ;; preceded by an plosive or 
	 ;; followed by a unvoiced fricative
	 (or (string-equal (item.feat s "n.ph_ctype") "f")
	     (string-equal (item.feat s "p.ph_ctype") "p"))
	 (string-equal (item.feat s "n.ph_cvox") "-")
	 )
	(cond
	 ((string-equal seg "b") (item.set_name s "p"))
	 ((string-equal seg "d") (item.set_name s "t"))
	 ((string-equal seg "g") (item.set_name s "k"))
	 ((string-equal seg "G") (item.set_name s "x"))
	 ((string-equal seg "v") (item.set_name s "f"))
	 ((string-equal seg "z") (item.set_name s "s"))
	 ))
    ))


;;; n-deletion
(define (net_nl_n_deletion s)
  (let ((seg (item.name s)))
    (if (and (string-equal seg "n")
	     ;; preceded by schwa
	     (string-equal (item.feat s "p.name") "@")
	     ;; schwa is not first seg in prosodic word: no deletion in word #@n#
	     (not (string-equal (item.feat (item.prev s) "R:ProsTree.parent.parent.id")
			   (item.feat (item.prev s) "R:ProsTree.parent.parent.parent.daughter1.id")))
	     ;; seg is part of the coda
	     (string-equal (item.feat s "R:ProsTree.parent.name") "Coda")
	     ;; seg is final seg in prosodic word
	     (string-equal (item.feat s "R:ProsTree.parent.parent.id")
			   (item.feat s "R:ProsTree.parent.parent.parent.daughtern.id"))
	     ;; seg is final seg in syllable part (coda)
	     (string-equal (item.feat s "R:ProsTree.id")
			   (item.feat s "R:ProsTree.parent.daughtern.id"))
        )
        (begin
	  ;; remove complete coda
	 (if trace (format t "deletion of segment: %s \n" (item.feat s "name")))
         (item.delete s)
        )
     )
  )
)

; check if s is not already deleted
(define (net_nl_cc_deletion s)
  (let ((seg (item.name s)))
    ;; two identical consonants in a row
    (if (and (string-equal (item.feat s "ph_vc") "-")
             (string-equal (item.feat s "name") (item.feat s "n.name")))
	(begin
	  (if trace (format t "CC-deletion segment: %s \n" (item.feat s "name")))
	  (item.delete s)
	  )
	)
    )
  )

;; remove a syllable part (onset, nuclues, or coda)
;; that is empty (i.e. has no segments as daughters)
(define (net_nl_remove_empty_SylPart utt)

  (mapcar
   (lambda (sylpart)

     (if (not (item.relation.daughter1 sylpart 'ProsTree))
      (begin
	(if trace (format t 
			  "Deleting empty %s\n" 
			  (item.name sylpart)))
	(item.delete sylpart))))
   (utt.relation.items utt 'SylPart))
)


(define (net_nl_insert_glottal_stop utt)
  (mapcar
   (lambda (sylpart)

;(format t   "Glottal stop %s\n"  (item.name sylpart))
       (if (and (item.prev sylpart)
                (string-equal (item.name sylpart) "Nucleus")
                (string-equal (item.name (item.prev sylpart)) "Nucleus"))
            (begin
               (set! ThisSegm (item.relation.daughter1 sylpart 'ProsTree))
               (set! PrevSegm (item.relation.daughtern (item.prev sylpart) 'ProsTree))
               (if (and (member (item.name ThisSegm) '("ai" "oi" "ui" "Ai" "Oi" "E:" "9:" "9y" "O:"))
                        (member (item.name PrevSegm) '("@" "ai" "oi" "ui" "Ai" "Oi" "E:" "9:" "9y" "O:")))
                   (item.relation.insert ThisSegm 
			      'Segment
			      (list '?)
			      'before)))))
   (utt.relation.items utt 'SylPart))
)

;;; TODO: 
;;; - As soon as the postlexical rules are completely implemented,
;;;   many of these rewritings can disappear. 
;;;   Use the script check-diphones.py to find out which remain
;;;   In addition, some of these may only be the results of errors
;;;   in the lexicon.

(define (net_nl_ib_rewrite_diphones utt)
  (let (prev this next)
	

    ;; rewriting vowels
    (mapcar
     (lambda (sylpart)
       (if (string-equal (item.name sylpart) "Nucleus")
         (begin
           (if trace (format t 
			"Starting rewrite-rules on segment: %s \n" 
			(item.feat (item.relation.daughter1 sylpart 'ProsTree) "name")))

           ;; within a syllable: /$ j/ ==> /$i/
           ;; check if there is a next sylpart
          (if (item.next sylpart)
            (begin
              (set! thisseg  (item.relation.daughter1 sylpart 'ProsTree))
              (set! segname  (item.feat thisseg "name"))
              (set! nextsylp (item.next sylpart))
              (set! nextseg  (item.relation.daughter1 nextsylp 'ProsTree))
              (set! prosw (item.parent_to (item.relation sylpart 'ProsTree) 'ProsWord1))
              (set! thisword  (item.parent (item.relation prosw 'Word-Pros)))

;;(format t "woord: %s break: %s next: %s\n" (item.name thisword) (item.feat thisword 'pbreak) (item.name nextseg))

              (if (and (string-equal (item.feat nextseg "name") "j")
	               (string-equal (item.feat nextsylp "name") "Coda"))
	        (begin
	           (cond
	             ;; /a j/ ==> /ai/
	             ;; not all oi-vowel dipones exist
	            ((string-equal segname "a")
	              (begin
                        (if (not (string-equal (item.feat thisword 'pbreak) "0"))
                          (begin 
                            (item.delete nextseg)
	                    (item.set_name thisseg "ai"))
 	                    (if (item.next (item.next sylpart))
	                      (begin
	                        (set! nnextsylp (item.next (item.next sylpart)))
		                (if (not (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+"))
		                  (begin
                                    (item.delete nextseg)
	                            (item.set_name thisseg "ai")))
	                        (if (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+")
		                  (begin
                                    (item.set_name nextseg "?")
	                            (item.set_name thisseg "ai"))))
	                 (begin
                           (item.delete nextseg)
	                   (item.set_name thisseg "ai"))))))
	                 
	             ;; /o j/ ==> /oi/ 
                     ;; not all oi-vowel dipones exist
	            ((string-equal segname "o")
	              (begin
                        (if (not (string-equal (item.feat thisword 'pbreak) "0"))
                          (begin 
                            (item.delete nextseg)
	                    (item.set_name thisseg "oi"))
 	                    (if (item.next (item.next sylpart))
	                      (begin
	                        (set! nnextsylp (item.next (item.next sylpart)))
		                (if (not (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+"))
		                  (begin
                                    (item.delete nextseg)
	                            (item.set_name thisseg "oi")))
	                        (if (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+")
		                  (begin
                                    (item.set_name nextseg "?")
	                            (item.set_name thisseg "oi"))))
	                 (begin
                           (item.delete nextseg)
	                   (item.set_name thisseg "oi"))))))
	             ;; within a syllable: /u j/ ==> /ui/
	             ;; not all oi-vowel dipones exist
	             ((string-equal segname "u")
	               (begin
                         (if (not (string-equal (item.feat thisword 'pbreak) "0"))
                           (begin 
                             (item.delete nextseg)
	                     (item.set_name thisseg "ui"))
 	                 (if (item.next (item.next sylpart))
	                   (begin
	                     (set! nnextsylp (item.next (item.next sylpart)))
		             (if (not (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+"))
		               (begin
                                 (item.delete nextseg)
	                         (item.set_name thisseg "ui"))))
	               (begin
                         (item.delete nextseg)
	                  (item.set_name thisseg "ui"))))))

	             ;; /O j/ ==> /Oi/
	             ;; not all oi-vowel dipones exist
	             ((string-equal segname "O")
	               (begin
                        (if (not (string-equal (item.feat thisword 'pbreak) "0"))
                          (begin 
                            (item.delete nextseg)
	                    (item.set_name thisseg "Oi"))
 	                    (if (item.next (item.next sylpart))
	                      (begin
	                        (set! nnextsylp (item.next (item.next sylpart)))
		                (if (not (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+"))
		                  (begin
                                    (item.delete nextseg)
	                            (item.set_name thisseg "Oi")))
	                        (if (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+")
		                  (begin
                                    (item.set_name nextseg "?")
	                            (item.set_name thisseg "Oi"))))
	                 (begin
                           (item.delete nextseg)
	                   (item.set_name thisseg "Oi"))))))
	                  
	             ;; /A j/ ==> /Ai/
	             ;; not all oi-vowel dipones exist
	             ((string-equal segname "A")
	               (begin
                        (if (not (string-equal (item.feat thisword 'pbreak) "0"))
                          (begin 
                            (item.delete nextseg)
	                    (item.set_name thisseg "Ai"))
 	                    (if (item.next (item.next sylpart))
	                      (begin
	                        (set! nnextsylp (item.next (item.next sylpart)))
		                (if (not (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+"))
		                  (begin
                                    (item.delete nextseg)
	                            (item.set_name thisseg "Ai")))
	                        (if (string-equal (item.feat (item.relation.daughter1 nnextsylp 'ProsTree) "ph_vc") "+")
		                  (begin
                                    (item.set_name nextseg "?")
	                            (item.set_name thisseg "Ai"))))
	                 (begin
                           (item.delete nextseg)
	                   (item.set_name thisseg "Ai"))))))

	     )
	    ;; this operation may result in an empty coda,
	    ;; which will then be removed in the next pass
	    ))))

           )
         )
      )
    (utt.relation.items utt 'SylPart)
    )

;
;rewriting CGN->IB/NL3 diphones and missing diphones
;
    (mapcar
     (lambda (s)
	   (begin
	     (set! this (item.name s))
	     (set! next (item.feat s "n.name"))
     
	     (cond
;
; vowels: conversion to IB/NL3 notation
;
	      ((string-equal this "E+") (item.set_name s "Ei"))
	      ((string-equal this "Y+") (item.set_name s "9y"))
	      ((string-equal this "A+") (item.set_name s "Au"))
	      ((string-equal this "Y:") (item.set_name s "9:"))

;
; consonants: rewrite missing diphones
;
	      ;; ---------- dark l ----------
	      ;; within coda: l => L 
	      ((and (string-equal this "l")
		    (string-equal (item.feat s "R:ProsTree.parent.name") "Coda"))
	       (item.set_name s "L"))
	      ;; ---------- G ----------
	      ;; next
	      ;; G-r => x-r (e.g. begrip)	 ;; G-d => x-d (e.g. leegde)
	      ;; G-l => x-l (e.g. begluren)	 ;; G-n => x-n (e.g. diagnose)
	      ;; G-b => x-b (e.g. boegbeeld)	 ;; G-m => x-m (e.g. dogma)
	      ;; G-w => x-w (e.g. oogwenk)	 ;; G-z => x-z (e.g. oorlogsbuit)
	      ;; G-j => x-j (e.g. brugjaar)	 ;; G-h => x-h (e.g. luchtwaardigheid)
	      ;; G-s => x-s (e.g. gedragsanalyse);; G-t => x-t (e.g. rechtsdwang)
	      ;; prev
	      ;; N-G => N-x (e.g. mongool)	;; r-G => r-x (e.g. morgen)
	      ;; l-G => l-x (e.g. algebra)	;; m-G => m-x (e.g. omgaan)
	      ;; n-G => n-x (e.g. loongrens)	;; w-G => w-x (e.g. bouwgrond)
	      ;; s-G => s-x (e.g. lesgeven)	;; S-G => S-x (e.g. puchglas)
	      ;; g-G => g-x (e.g. druggebruiker)
	      ((and (string-equal this "G")
		    (set! prev (item.feat s "p.name"))
		    (or (member_string next '("r" "d" "l" "n" "b" "m" "w" "z" "j" "h" "s" "t"))
			(member_string prev '("N" "r" "l" "L" "m" "n" "w" "s" "S" "g"))))
	       (item.set_name s "x"))
	      ;; ---------- d ----------
	      ;; d-b => t-b (e.g. aardbei)	;; d-z => t-z (e.g. fietsbel)
	      ;; d-m => t-m (e.g. aardmassa)	;; d-l => t-l (e.g. bladluis)
	      ;; d-n => t-n (e.g. brandnetel)	;; d-Z => t-Z (e.g. budget)
	      ;; d-_ => t-_ (e.g. eindtijd)	;; d-h => t-h (e.g. adhesie)
	      ;; d-s => t-s (e.g. beleidsproces);; d-p => t-p (e.g. overheadprojector)
	      ;; d-S => t-S (e.g. overheadsheet);; d-f => t-f (e.g. roadfilm)
	      ;; d-k => t-k (e.g. overheadkosten)
	      ((and (string-equal this "d")
		    (member_string next '("b" "z" "m" "l" "n" "Z" "_" "h" "s" "p" "S" "f" "k")))
	       (item.set_name s "t"))  
	      ;; ---------- b ----------
	      ;; b-d => p-d (e.g. abdij)	;; b-z => p-z (e.g gipsbeen )
	      ;; b-w => p-w (e.g clubwedstrijd)	;; b-n => p-n (e.g. abnormaal)
	      ;; b-m => p-m (e.g. schrabmes)	
	      ((and (string-equal this "b")
		    (member_string next '("d" "z" "w" "n" "m")))
	       (item.set_name s "p"))  
	      ;; ---------- z ----------
	      ;; z-b => s-b (e.g. asbak)	;; z-d => s-d (e.g. busdienst)
	      ;; z-g => s-g (e.g. diskdrive)    ;; z-l => s-l (e.g. quizleider)
	      ;; z-m => s-m (e.g. quizmaster)	;; z-p => s-p (e.g. sales-promotion)
	      ;; z-r => s-r (e.g. gezondheidsreden)
	      ((and (string-equal this "z")
		    (member_string next '("b" "d" "g" "l" "m" "p" "r")))
	       (item.set_name s "s"))   
	      ;; ---------- v ----------
	      ;; v-d => f-d (e.g. hoofdrol)	  ;; v-b => f-b (e.g. afbreken)
	      ;; v-z => f-z (e.g. bedrijfsbeleid) ;; v-w => f-w (e.g. ofwel)
	      ;; v-k => f-k (e.g. molotovcocktail);; v-g => f-g (e.g. lovegame)
	      ;; v-s => f-s (e.g. geloofsdwang)   ;; v-x => f-x (e.g. sovchose)
	      ;; v-G => f-G (e.g. sauvegarde)	  ;; v-n => f-n (e.g. have-nots)
	      ((and (string-equal this "v")
		    (member_string next '("d" "b" "z" "w" "k" "g" "s" "x" "G" "n")))
	       (item.set_name s "f"))  
	      ;; ---------- Z ----------
	      ;; Z-w => S-w (e.g. bourgeois)      ;; Z-j => S-j (e.g. lits-jumeaux)
	      ((and (string-equal this "Z")
		    (member_string next '("w" )))
	       (item.set_name s "S")) 
	      )) 
      )
    (utt.relation.items utt 'Segment)
    )

))

(define (net_nl_create_diphtongs sylpart)
    ;; within a syllable: /$ j/ ==> /$i/
    ;; check if there is a next sylpart
    (if (item.next sylpart)
      (begin
       (set! thisseg  (item.relation.daughter1 sylpart 'ProsTree))
       (set! segname  (item.feat thisseg "name"))
       (set! nextsylp (item.next sylpart))
       (set! nextseg  (item.relation.daughter1 nextsylp 'ProsTree))

 ;      (set! nnextsylp (item.next (item.next sylpart)))
(if ((item.next (item.next sylpart)))
(format t "next element %s" (item.name (item.relation.daughter1 (item.next (item.next sylpart)) 'ProsTree))))
       (if (and (string-equal (item.feat nextseg "name") "j")
	        (string-equal (item.feat nextsylp "name") "Coda"))
	  (begin
	    (cond
	     ;; /a j/ ==> /ai/
	     ((string-equal segname "a")
	      (begin
	        (item.delete nextseg)
	        (item.set_name thisseg "ai")))
	     ;; /o j/ ==> /oi/
             ;; a lot of diphones oi-diphthongs don't exist
	     ((string-equal segname "o")
	      (begin
                (item.delete nextseg)
	        (item.set_name thisseg "oi")))
	     ;; within a syllable: /u j/ ==> /ui/
	     ((string-equal segname "u")
	      (begin
	        (item.delete nextseg)
	        (item.set_name thisseg "ui")))
	     )
	    ;; this operation may result in an empty coda,
	    ;; which will then be removed in the final pass
	    ))))
)


(provide 'net_nl_postlex)


