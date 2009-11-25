;;; $Id: net_nl_phones.scm,v 1.4 2003/04/14 09:08:15 joopk Exp $
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


;;; Definition of phone set and features
;;; The phone set is that used for braod phonetic transcription 
;;; in the CGN project.  

(defPhoneSet
  net_nl
  ;;; Phone features
  (;; vowel or consonant
   (vc + - 0)  
   ;; vowel length: short or long
   (vlng s l 0)
   ;; vowel type: short long dipthong schwa
   (vtype s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: plosive fricative affricative nasal liquid
   (ctype p f a n l r 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                        dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  ;;; Phone set
  (;; vowels
   (@   +   s   a   2   2   -   0   0   0) ;; de
   (A   +   s   s   3   3   -   0   0   0) ;; pad
   (E   +   s   s   2   1   -   0   0   0) ;; pet
   (I   +   s   s   1   1   -   0   0   0) ;; pit
   (O   +   s   s   3   3   +   0   0   0) ;; pot
   (Y   +   s   s   2   3   +   0   0   0) ;; put
   (i   +   s   s   1   1   -   0   0   0) ;; vier
   (u   +   s   s   1   3   +   0   0   0) ;; voer
   (y   +   s   s   1   3   +   0   0   0) ;; vuur
   (a   +   l   l   3   1   -   0   0   0) ;; laan
   (e   +   l   l   2   1   -   0   0   0) ;; veer
   (o   +   l   l   2   3   +   0   0   0) ;; rood
   (2   +   l   l   2   1   -   0   0   0) ;; deur
   ;; diphtongs
   (E+  +   l   d   3   2   -   0   0   0) ;; CGN reis
   (Y+  +   l   d   3   2   -   0   0   0) ;; CGN huis
   (A+  +   l   d   3   2   -   0   0   0) ;; CGN koud
   (Ei  +   l   d   3   2   -   0   0   0) ;; IB/NL3 reis
   (9y  +   l   d   3   2   -   0   0   0) ;; IB/NL3 huis
   (Au  +   l   d   3   2   -   0   0   0) ;; IB/NL3 koud
   ;; foreign vowels
   (E:  +   l   d   2   1   -   0   0   0) ;; beige
   (Y:  +   l   d   2   1   -   0   0   0) ;; CGN freule
   (9:  +   l   d   2   1   -   0   0   0) ;; IB/NL3 freule
   (O:  +   l   d   2   1   -   0   0   0) ;; roze
   ;; nasal vowels
   (E~  +   l   d   3   2   -   0   0   0) ;; vaccin
   (A~  +   l   d   3   2   -   0   0   0) ;; croissant
   (O~  +   l   d   3   2   -   0   0   0) ;; conge
   (Y~  +   l   d   3   2   -   0   0   0) ;; parfum
   ;; consonants
   (p   -   0   0   0   0   0   p   l   -) ;; pas
   (t   -   0   0   0   0   0   p   a   -) ;; tas
   (k   -   0   0   0   0   0   p   v   -) ;; kas
   (b   -   0   0   0   0   0   p   l   +) ;; bas
   (d   -   0   0   0   0   0   p   a   +) ;; das
   (g   -   0   0   0   0   0   p   v   +) ;; goal
   (f   -   0   0   0   0   0   f   b   -) ;; fiets
   (v   -   0   0   0   0   0   f   b   +) ;; vaas
   (s   -   0   0   0   0   0   f   a   -) ;; sap
   (z   -   0   0   0   0   0   f   a   +) ;; zeep
   (S   -   0   0   0   0   0   f   p   -) ;; sjiek
   (Z   -   0   0   0   0   0   f   p   +) ;; gage
   (x   -   0   0   0   0   0   f   g   -) ;; toch
   (G   -   0   0   0   0   0   f   g   +) ;; regen
   (h   -   0   0   0   0   0   f   g   -) ;; hand
   (m   -   0   0   0   0   0   n   l   +) ;; man
   (n   -   0   0   0   0   0   n   a   +) ;; nam
   (N   -   0   0   0   0   0   n   v   +) ;; lang
   (J   -   0   0   0   0   0   n   v   +) ;; oranje
   (r   -   0   0   0   0   0   r   a   +) ;; rand
   (l   -   0   0   0   0   0   l   a   +) ;; lief
   (j   -   0   0   0   0   0   l   p   +) ;; jas
   (w   -   0   0   0   0   0   l   l   +) ;; wat
;; Not in CGN, present in IB/NL3 database
   (ai  +   l   d   2   1   -   0   0   0) ;; haai
   (oi  +   l   d   2   1   -   0   0   0) ;; mooi
   (ui  +   l   d   2   1   -   0   0   0) ;; boei
   (Ai  +   l   d   2   1   -   0   0   0) ;; ai
   (Oi  +   l   d   2   1   -   0   0   0) ;; hoi
   (L   -   0   0   0   0   0   l   a   +) ;; bal
   ;; other
   (?   0   0   0   0   0   -   0   0   -) ;; glottal stop in NL3 database
   (_   0   0   0   0   0   -   0   0   -) ;; silence
   )
)

(PhoneSet.silences '(_))


(provide 'net_nl_phones)
