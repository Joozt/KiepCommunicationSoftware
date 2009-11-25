;;; $Id: net_nl_synthesis.scm,v 1.10 2003/05/06 08:51:05 emarsi Exp $
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


;;; our additions to the definitions of utterance types in lib/synthesis.scm


(define (net_nl_ib_utterance_types)
"
Define the utterance types supported by net_nl_ib.
"
  (set! UttTypes nil)

  (defUttType Text
    (Initialize utt)
    (Text utt)
    (Token utt)
    (POS utt)
    (Syntax utt)
    (Phrasify utt)
    (Intonation utt)
    (Tune utt)
    (Word utt)
    (Pauses utt)
    (PostLex utt)
    (Duration utt)
    (Int_Targets utt)
    (Wave_Synth utt)
    )

  (defUttType Tokens
    (Token utt)
    (POS utt)
    (Syntax utt)
    (Phrasify utt)
    (Intonation utt)
    (Tune utt)
    (Word utt)
    (Pauses utt)
    (PostLex utt)
    (Duration utt)
    (Int_Targets utt)
    (Wave_Synth utt)
    )

  ;; This mode is used to synthesize from a list of words with
  ;; additional features. It is assumed that accents and breaks are
  ;; supplied by the caller.
  (defUttType Words
    (Initialize utt)
    (Word utt)
    (Pauses utt)
    (Tune utt)
    (PostLex utt)
    (Duration utt)
    (Int_Targets utt)
    (Wave_Synth utt)
    )

  ;; This mode is used by the Nintens GUI when the user wants to
  ;; return to the default intonation.
  (defUttType NewInton
    (Phrasify utt)
    (Intonation utt)
    (Tune utt)
    (Word utt)
    (Pauses utt)
    (PostLex utt)
    (Duration utt)
    (Int_Targets utt)
    (Wave_Synth utt)
    )

  ;; This mode is used by the Nintens GUI when only the ToDI
  ;; intonation symbols have been changed. 
  (defUttType CurrentInton
    (Word utt)
    (Pauses utt)
    (PostLex utt)
    (Duration utt)
    (Int_Targets utt)
    (Wave_Synth utt)
    )
)


(define (ResynthNewInton utt)
" 
\(ResynthNewInton UTT\)

Resynthesize, producing a new ToDI intonation.
This fynction is used by Nintens.
"
  (utt.set_feat utt 'type 'NewInton)
  (utt.synth utt))



(define (ResynthCurrentInton utt)
" 
\(ResynthCurrentInton UTT\)

Resynthesize and play UTT from the current ToDI intonation.
This fynction is used by Nintens,
"
  (utt.set_feat utt 'type 'CurrentInton)
  (utt.play (utt.synth utt)))




;; FIX-ME: intonational features are not handled correctly yet!

(define (SayWords words)
"
\(SayWords WORDS\)

WORDS, a list of words with optional feature specifications, 
is rendered as speech.
"
  (utt.play (utt.synth (eval (list 'Utterance 'Words words)))))


(define (SynthWords words)
"
\(SynthWords WORDS\)

WORDS, a list of words with optional feature specifications, 
is synthesized without playing the result.
"
  (utt.synth (eval (list 'Utterance 'Words words))))



(provide 'net_nl_synthesis)