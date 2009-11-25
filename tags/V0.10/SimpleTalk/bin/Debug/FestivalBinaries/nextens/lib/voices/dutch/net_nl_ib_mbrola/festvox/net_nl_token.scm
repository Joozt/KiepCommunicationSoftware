;;; $Id: net_nl_token.scm,v 1.4 2003/04/01 14:38:04 emarsi Exp $
;;;
;;; by Erwin marsi & Joop Kerkhoff
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


;;; Token rules to map (tokens to words)




;;; Part of speech done by crude lookup using gpos 
(set! net_nl_guess_pos 
'((fn
   de
   naar
   heeft
    ;; function words 
  )
  ;; Or split them into sub classes (but give them meaningful names)
  ; (fn0 .. .. .. ..)
  ; (fn1 .. .. .. ..)
  ; (fn2 .. .. .. ..)
))

(define (net_nl_token_to_words token name)
  "(net_nl_token_to_words TOKEN NAME)
Returns a list of words for the NAME from TOKEN.  This primarily
allows the treatment of numbers, money etc."
  (cond
   ((string-matches name "[1-9][0-9]+")
    (net_nl_number token name))

   ;; Add many other rules, for numbers, abbrev, alphanumerics, homographs
   ((string-equal name "i.v.m") 
    (list "in" "verband" "met"))
   ((string-equal name "zwart-wit") 
    (list "zwart" "wit"))
   (t
    (list name))))


(define (net_nl_number token name)
  "(net_nl_number token name)
Return list of words that pronounce this number in nl."
  ;; You have to write this

)

(provide 'net_nl_token)
