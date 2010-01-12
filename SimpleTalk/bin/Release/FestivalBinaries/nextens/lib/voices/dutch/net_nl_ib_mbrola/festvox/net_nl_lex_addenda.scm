;;; $Id: net_nl_lex_addenda.scm,v 1.2 2003/04/01 14:38:04 emarsi Exp $
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


;;; additions to the lexicon


(define (nl::addenda)
"
\(nl::addenda\)

Basic lexicon should \(must ?\) basic letters and punctuation.
"
;;; Pronunciation of letters in the alphabet
(lex.add.entry '("a" nil ((((a t a) 0)))))
;(lex.add.entry '("b" nn (((b e) 0))))
;(lex.add.entry '("c" nn (((th e) 0))))
;(lex.add.entry '("d" nn (((d e) 0))))
;(lex.add.entry '("e" nn (((e) 0))))
; ...
; Symbols ...
;(lex.add.entry 
; '("*" n (((a s) 0) ((t e) 0) ((r i1 s) 1)  ((k o) 0))))
;(lex.add.entry 
; '("%" n (((p o r) 0) ((th i e1 n) 1) ((t o) 0))))

;; Basic punctuation must be in with nil pronunciation
(lex.add.entry '("." punc nil))
;(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
(lex.add.entry '("'" punc nil))
(lex.add.entry '(":" punc nil))
(lex.add.entry '(";" punc nil))
(lex.add.entry '("," punc nil))
;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
(lex.add.entry '("-" punc nil))
(lex.add.entry '("\"" punc nil))
(lex.add.entry '("`" punc nil))
(lex.add.entry '("?" punc nil))
(lex.add.entry '("!" punc nil))

(lex.add.entry '("kamerhamer" nil ( ( ((k a:) 3) ((m @ r) 0) ) ( ((h a:) 2) ((m @ r) 0) ) )))
(lex.add.entry '("boomleeuwerik" nil  ((((b o: m) 1)) (((l e:) 2) ((w @) 0) ((r I k) 0)))))
)


(provide 'net_nl_lex_addenda)