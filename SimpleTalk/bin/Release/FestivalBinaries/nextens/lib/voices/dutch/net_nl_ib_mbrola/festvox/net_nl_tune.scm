;;; $Id: net_nl_tune.scm,v 1.4 2003/04/01 14:38:04 emarsi Exp $
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
;;; tune choice


;;; define a tune choice module

(define (Tune utt)
"(Syntax utt)\n                                
Perform tune choice for accents and boundaries."
  (let ((rval (apply_method 'Tune_Method utt)))
    (cond
     ;; new style
     (rval rval)
     ;; do nothing
     (t utt)))) 



(define (nl::basic_tune_choice utt)
"
\(nl::basic_tune_choice UTT\)

Choose an intonational tune by associating a particular ToDI pitch
accent with every accented word, and particular ToDI boundary tones
with every medium/heavy break. To this end, a new relation called
Intonation is created, which contains the intonational events, as well
as a new Word-Int relation, which contains the assocations between
words and their intonational events.
"
  ;; make a new relation to store the tune as a sequence of
  ;; intonational events (i.e. ToDI pitch accents and boundary tones)
  (utt.relation.create utt 'Intonation)
  ;; make a new relation to to store the association between words and
  ;; intonational events
  (utt.relation.create utt 'Word-Int)
  ;; to force an initial boundary tone at start of utterance we insert
  ;; the tone here, but postpone its association with the 1st word,
  ;; because the word is not append to Word-Int yet
  (utt.relation.append utt 'Intonation '("%L"))
  (mapcar
    (lambda (word)
      ;; take the lazy way: just add every word to Word-Int,
      ;; whether it has intonation or not
      (utt.relation.append utt 'Word-Int word)
      (if (member (item.feat word "p.pbreak") '("medium" "heavy"))
	  ;; if the word is *preceded* by break
	  ;; that is, the previous word has a medium of heavy pbreak
	 (begin
	   ;; associate a final boundary with the preceding word
	   (item.relation.append_daughter
	    (item.prev word)
	    'Word-Int
	    (utt.relation.append utt 'Intonation '("%")))
	   ;; and an initial boundary tone with the current word	   
	   (item.relation.append_daughter
	    word
	    'Word-Int
	    (utt.relation.append utt 'Intonation '("%L")))))
      (if (string-equal (item.feat word 'acc) "+")
          (begin
               ;; if word is the last accent in non final phrase
               (if (nl::LastAccentNonFinalPhrase word utt)
                   (nl::SelectTuneNonFinalPhrase word utt)
	           ;; if the word is accented
	           ;; associate a default pitch accent, default H*L	  
                   (item.relation.append_daughter
	                                         word
	                                         'Word-Int
                                                 (utt.relation.append utt 'Intonation '("H*L")))))))
    (utt.relation.items utt 'Word))
  ;; to force initial boundary tone at start of utterance, we
  ;; associate the first word with the tone inserted earlier
  (item.prepend_daughter
   (utt.relation.first utt 'Word-Int)
   (utt.relation.first utt 'Intonation))
  ;; force final boundary tone at end of utterance
  ;; FIX-ME: question intonation? 
  (item.relation.append_daughter
   (utt.relation.last utt 'Word)
   'Word-Int
   (utt.relation.append utt 'Intonation '("L%")))
utt)


; select tune for last accent in a non final phrase
(define (nl::SelectTuneNonFinalPhrase word utt)
     ;;; Tune choice, last accent on last accent before a non final boundary
     ;;; accent on last word H*
     ;;; accent on VERB before REL H*LH
     ;;; default L*H
       (set! pos (item.feat word "R:Token.parent.pos"))
       (set! WordAfterBound (item.name (item.next BoundWord)))
       (if (equal? word BoundWord)							; accent on last word, non final phrase
          (item.relation.append_daughter
	                    word
	                    'Word-Int
                            (utt.relation.append utt 'Intonation '("H*")))
          (if (and (string-equal pos "V") (member WordAfterBound '("die" "dat")))	;accent on VERB before REL
             (item.relation.append_daughter
	                       word
	                       'Word-Int
                               (utt.relation.append utt 'Intonation '("H*LH")))
             (item.relation.append_daughter
	                       word
	                       'Word-Int
                               (utt.relation.append utt 'Intonation '("L*H")))))	; default last accent non final phrase
)


;; check if accented word is last accent in non final phrase
(define (nl::LastAccentNonFinalPhrase word utt)

    (set! FinalAccentNFP nil)			; last accent in a Not Final Phrase
    (set! NextSearch t)
    (set! BoundWord word)
    
    ;;; first check if this accent is last accent in non final phrase
    (if (and (item.next word)			; accent on last word, non final phrase
             (member (item.feat word "pbreak") '("medium" "heavy")))
        (begin 
          (set! FinalAccentNFP t)			; found last accent
          (set! NextSearch nil)                 	; stop searching
          (set! BoundWord word)))               	; this word has boundary
             
          (while (and (string-equal NextSearch t)	; search next accent or boundary
                         (item.next BoundWord))
             (begin
                 (set! BoundWord (item.next BoundWord))
                 (if (string-equal (item.feat BoundWord 'acc) "+")
                    (set! NextSearch nil))		; not final accent, stop search

                 (if (and (string-equal NextSearch t)
                          (member (item.feat BoundWord "pbreak") '("medium" "heavy"))) 	; check non final phrase 
                    (begin 
                        (set! NextSearch nil)					; stop search
                         (if (not (string-equal (item.feat BoundWord 'id)	; non final phrase?
                                                (item.feat (utt.relation.last utt 'Word) 'id)))
                             (set! FinalAccentNFP t))))))
; if NotFinalPhrase then BoundWord is the word on boundary position
FinalAccentNFP
)


(provide 'net_nl_tune)
