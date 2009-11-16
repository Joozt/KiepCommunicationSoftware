;;; $Id: net_nl_dur_kun.scm,v 1.7 2003/04/01 14:38:04 emarsi Exp $
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


;;;  KUN Segment Duration 


(set! net_nl_ib::phone_data
'(
  ;;;Segments and their initial duration in seconds

  ;; Vowels
   (@ 0.060) ;; schwa, gemak
   (A 0.070) ;; pad
   (E 0.070) ;; pet
   (I 0.070) ;; pit
   (O 0.070) ;; pot
   (Y 0.070) ;; put
   (i 0.070) ;; vier
   (u 0.070) ;; voer
   (y 0.070) ;; vuur
   (a 0.070) ;; laan
   (e 0.070) ;; veer
   (o 0.070) ;; rood
   (2 0.070) ;; deur
  ;; diphthongs
   (Ei  0.075) ;; reis
   (9y  0.075) ;; huis
   (Au  0.075) ;; koud
  ;; foreign vowels
   (E:  0.075) ;; beige
   (Y:  0.075) ;; freule
   (9:  0.075) ;; freule
   (O:  0.075) ;; roze
  ;; consonants
   (p   0.060) ;; pas
   (t   0.060) ;; tas
   (k   0.060) ;; kas
   (b   0.050) ;; bas
   (d   0.050) ;; das
   (g   0.050) ;; goal
   (f   0.075) ;; fiets
   (s   0.075) ;; sap
   (x   0.075) ;; toch
   (h   0.060) ;; hand
   (v   0.060) ;; vaas
   (z   0.060) ;; zeep
   (S   0.075) ;; sjiek
   (Z   0.075) ;; gage
   (G   0.060) ;; regen
   (m   0.060) ;; man
   (n   0.060) ;; nam
   (N   0.060) ;; lang
   (J   0.060) ;; oranje
   (r   0.060) ;; rand
   (l   0.060) ;; lief
   (j   0.060) ;; jas
   (w   0.060) ;; wat
;;;   (dZ  0.070) ;; jazz
;; Not in CGN, present in NL3 database
   (ai  0.080) ;; draai
   (oi  0.080) ;; mooi
   (ui  0.080) ;; roei
   (Ai  0.080) ;; ai
   (Oi  0.080) ;; hoi
   (L   0.080) ;; bal
  ;; others
   (?   0.015) ;; glottal stop
   (_   0.050) ;; pause

))


(define (KUN_Duration utt)

; insert break pause symbols
;(Break_Pauses utt)

 (let ((segments (utt.relation.items utt 'Segment))
        (trace 0)
	(endtime 0)
	 seg duration ThisSyll LastFoot LastSyll ProsW ProsW1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Settings Speaking rate, Pause durations ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    (set! hev_pause (Parameter.get 'HeavyPause))	; pause heavy phrase
;    (set! med_pause (Parameter.get 'MedPause))		; pause medium phrase
;    (set! light_pause (Parameter.get 'LightPause))	; pause light phrase
    (set! hev_pause 0.15)		; pause heavy phrase
    (set! med_pause 0.1)		; pause medium phrase
    (set! light_pause 0.015)		; pause light phrase
    (set! sp_rate (Parameter.get 'SpeakRate))	; Get speaking rate
    (set! mult_voc (- 1 (* sp_rate 0.05)))	; change voc-duration (sp_rate * 5 percent)
    (set! mult_cons (- 1 (* sp_rate 0.03)))	; change cons-duration (sp_rate * 3 percent)
 
    
    (while segments

	(set! seg (car segments))
	(set! duration (car (cdr (assoc_string 
				     (item.name seg)  
				     net_nl_ib::phone_data ))))

;
; pause durations
;
	(if (string-equal (item.feat seg "ph_vc") "0")		; pause segments
	 (begin
             (cond
               ((string-equal (item.feat seg 'break) "heavy")  (set! duration hev_pause))
               ((string-equal (item.feat seg 'break) "medium") (set! duration med_pause))
               ((string-equal (item.feat seg 'break) "light")  (set! duration light_pause))
	     )
	     ;
	     ;  Speaking rate pauses
	     (set! duration (* duration mult_voc))	; speaking rate shortening is the same as vowels
	 ))

;
; all phonemes
;
	(if (not (string-equal (item.feat seg "ph_vc") "0"))		; for all segments
	 (begin
   	   (set! ThisSyll (item.parent					; syllable
		           (item.parent
		       	    (item.relation seg 'ProsTree))))

   	   (set! ProsW1    (item.parent (item.parent (item.parent ThisSyll))))
	   (set! LastFoot  (item.daughtern (item.daughtern ProsW1)))
	   (set! LastSyll  (item.daughtern LastFoot))
	   (set! FirstFoot (item.daughter1 (item.daughter1 ProsW1)))
	   (set! FirstSyll (item.daughter1 FirstFoot))
    	   (set! UttWord   (item.parent (item.relation ProsW1 'Word-Pros))) ;sentence word


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function words shortening ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; shorten all segments in function words:
;          monosyllabic words (except nouns and adjectives) without sentence accent
;

	(if (and (not (string-equal (item.feat UttWord 'acc) "+"))			; if not accented word
	         (not (member_string (item.feat UttWord "R:Token.parent.pos") '(N Adj))))
	  (if (equal? FirstSyll LastSyll 'id)
	       (begin
	           (set! duration (* duration 0.90))
;	     (format t "Shorten function word %s \tduration %l\n" (item.name UttWord) duration)
	           (if (> trace 2) (format t "Shorten function word \tduration %l\n" duration))))
	)

;
; shorten weak monosyllabic compound parts
; ProsWord2 is weak AND monosyllabic AND ProsWord1 consists of more than 1 ProsWord2 parts
;
	(if (and (string-equal (item.feat (item.parent (item.parent ThisSyll)) 'metrical) 'weak)
	         (equal? (item.daughter1 (item.parent ThisSyll)) (item.daughtern (item.parent ThisSyll)))
	         (not (equal?  (item.daughter1 ProsW1) (item.daughtern ProsW1)))
	    )
	 (begin
	     (set! duration (* duration 0.90))
	     (if (> trace 2) (format t "Shorten monosyllabic weak compound part \tduration %l\n" duration)))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;
; Pre-final lengthening ;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
; lengthen all segements in nucleus and coda in last syllable/foot in last word in phrase
;

      	   (set! PhraseBreak (item.feat UttWord 'pbreak))		; word level
;      	   (format t " word %s ->> break %s\n" (item.name UttWord) PhraseBreak)	   

           (if (member_string PhraseBreak '(B heavy medium light))
       	      (begin

		;; lengthen last foot before phrase end
                (if (equal? (item.parent ThisSyll) LastFoot)
                   (begin
			(set! duration (* duration 1.05))
		 	(if (> trace 1) (format t "Final foot lengthening\tduration %l\n" duration))
		)); end lastfoot

		;; lengthen nucleus and coda in last syllable before phrase end
                (if (and
                	(not (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
                	(equal? ThisSyll LastSyll))
                   (begin
			(set! duration (* duration 1.2))
			(if (> trace 1) (format t "Final syllable lengthening\tduration %l\n" duration))
		)); end lastsyll
	   )); end if break
	)); end if all segments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Segmental duration rules ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if no glide inserted
; lengthen second vowel in cases 'duo, duel, ...'
; lengthen also 'mao,..'
;
       (if (and
                (string-equal (item.feat seg "ph_vc") "+")
                (string-equal (item.feat seg "p.ph_vc") "+")
 		(string-equal (item.feat seg "R:ProsTree.parent.parent.parent.parent.id")	; same ProsWord2
                              (item.feat seg "p.R:ProsTree.parent.parent.parent.parent.id"))
           ); end and
		(begin
		    (set! duration (* duration 1.25))
		    (if (> trace 1) (format t "Lenghten two vowels in diff. syllables\tduration %l\n" duration))
                ); end begin
       ); end if

;
; shorten consonants in coda position
;
       (if (and
                (string-equal (item.feat seg "ph_vc") "-")
 		(string-equal (item.feat seg "R:ProsTree.parent.name") "Coda")
           ); end and
		(begin
		    (set! duration (* duration 0.95))
		    (if (> trace 1) (format t "Shorten cons in coda\tduration %l\n" duration))
                ); end begin
       ); end if


;
; shorten consonants in clusters
; evaluate: first cons not equal last cons in coda tree
;
       (if (and
                (string-equal (item.feat seg "ph_vc") "-")
 		(not (string-equal (item.feat seg "R:ProsTree.parent.daughter1.id")
                                   (item.feat seg "R:ProsTree.parent.daughtern.id")))
           ); end and
		(begin
		    (set! duration (* duration 0.75))
		    (if (> trace 1) (format t "Shorten cons in a cluster\tduration %l\n" duration))
                ); end begin
       ); end if

;
; shorten consonants additional in clusters in coda position
; evaluate: first cons not equal last cons
;
;       (if (and
;                (string-equal (item.feat seg "ph_vc") "-")
; 		(string-equal (item.feat seg "R:ProsTree.parent.name") "Coda")
; 		(not (string-equal (item.feat seg "R:ProsTree.parent.daughter1.id")
;                                   (item.feat seg "R:ProsTree.parent.daughtern.id")))
;           ); end and
;		(begin
;		    (set! duration (* duration 0.9))
;		    (if (> trace 1) (format t "Shorten cons in a cluster in coda\tduration %l\n" duration))
;                ); end begin
;       ); end if


;
; shorten voc followed by a consonant cluster (Hofhuis)
; compare long vowels in closed syllables (Gussenhoven, Rietveld)
;
       (if (and
                (string-equal (phone_feature (item.name seg) `vc) "+")
                (string-equal (item.feat seg "n.R:ProsTree.parent.name") "Coda")
 		(string-equal (item.feat seg "n.n.R:ProsTree.parent.name") "Coda")
           ); end and
		(begin
		    (set! duration (* duration 0.9))
		    (if (> trace 1) (format t "Shorten voc followed by a cons cluster\tduration %l\n" duration))
                ); end begin
       ); end if


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; foot durations VOWELS ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(if (string-equal (phone_feature (item.name seg) `vc) "+")
	   (begin

		;
		;  Speaking rate vowels
		(set! duration (* duration mult_voc))

; INTONATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; lengthen vowel in accented (stressed) syllable:  5%
; lengthen preaccented syllable			   3% Not yet implemented
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	(if (and (string-equal (item.feat UttWord 'acc) "+")			; if ToDIsymbol on word
	         (string-equal (item.feat ThisSyll 'stress) "1"))
	 (begin
	           (set! duration (* duration 1.05))
		   (if (> trace 2) (format t "Lenghten intonation \tduration %l\n" duration))
	))

;
; lengthen foothead strong foot long voc
;
       (if (and
                (string-equal (phone_feature (item.name seg) `vlng) "l")
		(string-equal
		 (item.feat
		  (item.parent
		   (item.parent
		    (item.parent
		     (item.relation seg 'ProsTree)))) 'metrical) "strong")
 		(string-equal
		 (item.feat
		  (item.parent
		   (item.parent
		    (item.relation seg 'ProsTree))) 'metrical) "strong")
           ); end and
		     (begin
		       (set! duration (* duration 1.3))
		       (if (> trace 2) (format t "Lenghten foothead strong foot long \tduration %l\n" duration))
                     ); end begin
       ); end if

;
; lengthen foothead strong foot short voc
;
       (if (and
          	(string-equal (phone_feature (item.name seg) `vlng) "s")
 		(string-equal
		 (item.feat
		  (item.parent 
		   (item.parent 
		    (item.parent 
		     (item.relation seg 'ProsTree)))) 'metrical) "strong")
  		(string-equal
		 (item.feat
		   (item.parent 
		    (item.parent 
		     (item.relation seg 'ProsTree))) 'metrical) "strong")
           ); end and
		     (begin
		       (set! duration (* duration 1.1))
		       (if (> trace 2) (format t "Lenghten foothead strong foot short voc\tduration %l\n" duration))
                     ); end begin
        ); end if

;
; lengthen foothead long voc
;
       (if (and
                (string-equal (phone_feature (item.name seg) `vlng) "l")
 		(string-equal
		 (item.feat
		  (item.parent 
		   (item.parent
		    (item.relation seg 'ProsTree))) 'metrical) "strong")
           ); end and
		     (begin
		       (set! duration (* duration 1.2))
		       (if (> trace 2) (format t "Lenghten foothead long voc\tduration %l\n" duration))
                     ); end begin
       ); end if

;
; lengthen foothead short voc
;
       (if (and
                (string-equal (phone_feature (item.name seg) `vlng) "s")
 		(string-equal
		 (item.feat
		  (item.parent
		   (item.parent 
		    (item.relation seg 'ProsTree))) 'metrical) "strong")
           ); end and
		     (begin
		       (set! duration (* duration 1.1))
		       (if (> trace 2) (format t "Lenghten foothead short voc\tduration %l\n" duration))
                     ); end begin
       ); end if


;
; lengthen all voc in last syllable in a foot
;
;       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
;                         (item.feat seg "R:ProsTree.parent.parent.parent.daughtern.id"))
;		(begin
;		    (set! duration (* duration 1.1))
;		    (if (> trace 2) (format t "Lenghten last syllable in foot\tduration %l\n" duration))
;               ); end begin
;       ); end if


;
; lengthen all vowels in the first syllable of a word
;
       (if  (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
                          (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.daughter1.id"))
		(begin
		    (set! duration (* duration 1.1))
		    (if (> trace 2) (format t "Lenghten first syllable in word\tduration %l\n" duration))
                ); end begin
       ); end if


;
; lengthen all vowels in the last syllable of a word
;
       (if  (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
                          (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.daughtern.id"))
		(begin
		    (set! duration (* duration 1.05))
		    (if (> trace 2) (format t "Lenghten last syllable in word\tduration %l\n" duration))
                ); end begin
       ); end if

;
; lengthen all vowels in the first foot of a word
;
       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.id")
                         (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.id"))
		(begin
		    (set! duration (* duration 1.1))
		    (if (> trace 2) (format t "Lenghten first foot in word\tduration %l\n" duration))
                ); end begin
       ); end if


;
; lengthen all vowels in the last foot of a word
;
       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.id")
                         (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.id"))
		(begin
		    (set! duration (* duration 1.10))
		    (if (> trace 2) (format t "Lenghten last foot in word\tduration %l\n" duration))
                ); end begin
       ); end if


; shorten 2% voc in a foot, three or more syllables in a foot
; check syll1/=sylln AND syll1/=syll2 AND syll2/=sylln
       (if (and
                (not (string-equal (item.feat ThisSyll "R:ProsTree.parent.daughter1.id")
                                   (item.feat ThisSyll "R:ProsTree.parent.daughtern.id")))
                (not (string-equal (item.feat ThisSyll "R:ProsTree.parent.daughter1.id")
                                   (item.feat ThisSyll "R:ProsTree.parent.daughter2.id")))
                (not (string-equal (item.feat ThisSyll "R:ProsTree.parent.daughter2.id")
                                   (item.feat ThisSyll "R:ProsTree.parent.daughtern.id")))
            ); end and
		(begin
		    (set! duration (* duration 0.98))
		    (if (> trace 2) (format t "Shorten voc %s, more then 2 syllables in a foot\tduration %l\n" (item.feat seg "name") duration))
               ); end begin
       ); end if

;
; shorten 5% voc in first foot in a word (example minimalisatie)
; if more then one foot before stressed foot
; foot is weak; next foot is weak; check first foot of a word (ProsWord2)
       (if (and
                (string-equal (item.feat (item.parent ThisSyll) 'metrical) "weak")	; weak foot
                (equal? (item.parent ThisSyll) FirstFoot)				; first foot word
                (string-equal (item.feat (item.next FirstFoot) 'metrical) "weak")
            ); end and
		(begin
		    (set! duration (* duration 0.95))
                    (if (> trace 2) (format t "Shorten voc %s before pre-main stress foot\tduration %l\n" (item.feat seg "name") duration))
               ); end begin
       ); end if

;
; lengthen [y,u,i] before /r/ in the same foot
;
       (if (and
                (string-matches (item.feat seg "name") "[yui]")
                (string-equal (item.feat seg "n.name") "r")
 		(string-equal (item.feat seg "R:ProsTree.parent.parent.parent.id")
                              (item.feat seg "n.R:ProsTree.parent.parent.parent.id"))
           ); end and
		(begin
		    (set! duration (* duration 1.25))
 		    (if (> trace 2) (format t "Lenghten /r/ in same foot\tduration %l\n" duration))
               ); end begin
       ); end if

;
; lenghten long voc in open syllable (opposite of shorten closed syllables)
; example: openen

      (if (and
                (string-equal (phone_feature (item.name seg) `vlng) "l")
 		(string-equal (item.feat seg "R:ProsTree.parent.id")
                              (item.feat seg "R:ProsTree.parent.parent.daughtern.id"))
           ); end and
		(begin
		    (set! duration (* duration 1.1))
		    (if (> trace 2)(format t "Lenghten open syllable\tduration %l\n" duration))
                ); end begin
       ); end if


; shorten long voc in closed syllable  (see FONPARS durations)
;
;       (if (and
;                (string-equal (phone_feature (item.name seg) `vlng) "l")
;                (string-equal (phone_feature (item.feat seg "n.name") `vc) "-")
; 		(string-equal (item.feat seg "R:ProsTree.parent.parent.id")
;                              (item.feat seg "n.R:ProsTree.parent.parent.id"))
;           ); end and
;		(begin
;		    (set! duration (* duration 0.9))
;		    (if (> trace 2)(format t "Shorten closed syllable\tduration %l\n" duration))
;                ); end begin
;       ); end if

		 
         ); end if begin
    ); end if vowels




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; foot durations consonants ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (if (string-equal (phone_feature (item.name seg) `vc) "-")
	   (begin


		;
		;  Speaking rate consonants duration
		;
	         (set! duration (* duration mult_cons))

;
; lengthen consonants foothead strong foot
;
       (if (and
		(string-equal
		 (item.feat
		  (item.parent
		   (item.parent
		    (item.parent
		     (item.relation seg 'ProsTree)))) 'metrical) "strong")
 		(string-equal
		 (item.feat
		  (item.parent
		   (item.parent
		    (item.relation seg 'ProsTree))) 'metrical) "strong")
           ); end and
		     (begin
		       (set! duration (* duration 1.1))
		       (if (> trace 3) (format t "Lenghten cons in foothead strong foot\tduration %l\n" duration))
                     ); end begin
       ); end if

;
; lengthen consonants in first syllable of a foot
;
       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
                         (item.feat seg "R:ProsTree.parent.parent.parent.daughter1.id"))
		(begin
		    (set! duration (* duration 1.05))
		    (if (> trace 3) (format t "Lenghten cons in first syllable in foot\tduration %l\n" duration))
                ); end begin
       ); end if


;
; lengthen consonants in the first syllable of a word
;
       (if  (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
                          (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.daughter1.id"))
		(begin
		    (set! duration (* duration 1.1))
		    (if (> trace 3) (format t "Lenghten cons first syllable in word\tduration %l\n" duration))
                ); end begin
       ); end if


;
; lengthen consonants in the last syllable of a word
;
       (if  (string-equal (item.feat seg "R:ProsTree.parent.parent.id")
                          (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.daughtern.id"))
		(begin
		    (set! duration (* duration 1.05))
		    (if (> trace 3) (format t "Lenghten cons in last syllable in word\tduration %l\n" duration))
                ); end begin
       ); end if

;
; lengthen consonants in the first foot of a word
;
       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.id")
                         (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.id"))
		(begin
		    (set! duration (* duration 1.1))
		    (if (> trace 3) (format t "Lenghten cons in first foot in word\tduration %l\n" duration))
                ); end begin
       ); end if

;
; lengthen consonants in the last foot of a word
;
       (if (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.id")
                         (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.id"))
		(begin
		    (set! duration (* duration 1.05))
		    (if (> trace 3) (format t "Lenghten cons in last foot in word\tduration %l\n" duration))
                ); end begin
       ); end if


   ); end begin if
  ); end if consonants


; Set variables necessary for synthesis
	   (set! endtime (+ endtime duration))
	   (item.set_feat seg 'dur duration)
	   (item.set_feat seg 'end endtime)
	   (if (> trace 0)(format t "Segment: %l \tduration %l\n" (item.feat seg 'name) duration))
	   (set! segments (cdr segments))

    ); end while
    (set! uttend (+ (item.feat (utt.relation.last utt 'Segment) 'end) 0.05))
  ); end let ...
 
utt)


(provide 'net_nl_ib_kun_dur)
