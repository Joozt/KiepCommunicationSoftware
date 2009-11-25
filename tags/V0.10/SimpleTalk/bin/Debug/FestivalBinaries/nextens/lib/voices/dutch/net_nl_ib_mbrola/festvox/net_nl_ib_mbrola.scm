;;; $Id: net_nl_ib_mbrola.scm,v 1.20 2003/04/01 14:38:04 emarsi Exp $
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


;;; A Dutch female voice (Ineke Burema) 
;;; using the nl3 Mbrola diphone database

;;; Try to find out where we are
(if (assoc 'net_nl_ib_mbrola voice-locations)
    (defvar net_nl_ib_mbrola_dir 
      (cdr (assoc 'net_nl_ib_mbrola voice-locations)))
    ;;; Not installed in Festival yet so assume running in place
    (defvar net_nl_ib_mbrola_dir (pwd)))

(if (not (probe_file (path-append net_nl_ib_mbrola_dir "festvox/")))
    (begin
     (format stderr "net_nl_ib: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append net_nl_ib_mbrola_dir "festvox/"))
     (format stderr "   Either the voice isn't linked into Festival\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))


;;;  Add the directory containing general voice stuff to load-path
(set! load-path (cons (path-append net_nl_ib_mbrola_dir "festvox/") load-path))


;;; if on Cygwin, set TMPDIR dir to windows tmp dir,
;;; so (make_tmp_filename) works correctly
(if (string-matches *ostype* ".*CYGWIN.*")
    (setenv "TMPDIR" (or (getenv "TEMP") (getenv "TMP"))))


;;; support for the Nintens GUI
(require 'net_nl_nintens)


(define (voice_net_nl_ib_mbrola)
"
\(voice_net_nl_ib_mbrola\)

Set speaker to IB in Dutch.
"
  (format t "Initializing voice net_nl_ib_mbrola\nPlease wait...\n")

  (voice_reset)
  (net_nl_ib_voice_backup)

  ;; define some new utterance types
  (require 'net_nl_synthesis)
  (net_nl_ib_utterance_types)

  (Param.set 'Language 'net_nl)

  ;; ------------------------------------------------------------
  ;; Token module: 
  ;; tokenisation
  ;; ------------------------------------------------------------

  (require 'net_nl_token)
  (Param.set 'Token_Method 'Token_Any)
  (set! token_to_words net_nl_token_to_words)

  ;; ------------------------------------------------------------
  ;; POS module:
  ;; part-of-speech tagging
  ;; ------------------------------------------------------------

  ;; initialize according to the settingsfile
  (MBT.init (path-append net_nl_ib_mbrola_dir "mbt-ibs/wotan.all.tag.settingsfile"))
  ;; by default, MBT searches for its required files in the same
  ;; directory as where the settingsfile is found 

  ;; use the Memory-based tagger for Dutch
  (Param.set 'POS_Method 'MBT)
  ; (MBT.verbose t)

  ;; ------------------------------------------------------------
  ;; Syntax module:
  ;; syntactic parsing
  ;; ------------------------------------------------------------
  
  (require 'net_nl_syntax)

  ;; -- Option 1 -- No syntax
  (Param.set 'Syntax_Method nil)

  ;; -- Option 2 -- Amazon syntactic parser
  ; (require 'net_nl_syntax_amazon)
  ;(Param.set 'Syntax_Method 'nl::amazon) 
  ;(set! nl::amazon_trace t)

  ;; ------------------------------------------------------------
  ;; Phrasify module:
  ;; phrase break prediction
  ;; ------------------------------------------------------------

  (require 'net_nl_break)
  
  ;; -- Option 1 -- Phrase break prediction by punctuation
  ;(set! pos_supported nil) ;; well not real pos anyhow
  ;(set! phrase_cart_tree nl::phrase_cart_tree)
  ;(Param.set 'Phrase_Method 'cart_tree)

  ;; -- option 2 -- Prosit accent assignment using a Timbl classifier
  (require 'net_nl_break_prosit)
  (Param.set 'Phrasify_Method 'nl::prosit-break)

  ;; -- Option 3 -- Phrase break prediction by Herwijnen algorithm
  ;; Note: this requires syntactic analysis!
  ; (require 'net_nl_break_herwijnen)
  ; (Param.set 'Phrasify_Method 'nl::HerwijnenPhrasing)

  ;; ------------------------------------------------------------
  ;; Intonation module:
  ;; pitch accent placement and tune choice
  ;; ------------------------------------------------------------

  (require 'net_nl_accent)

  ;; -- option 1 -- Basic accent assignment on the basis C/F word
  ;; uses POS tag to determine if a word is a function or a content
  ;; word, and assigns accents to all content words
  ; (Param.set 'Int_Method 'nl::basic_accent_placement)

  ;; -- option 2 -- Prosit accent assignment using a Timbl classifier
  (require 'net_nl_accent_prosit)
  (Param.set 'Int_Method 'nl::prosit_accent_placement)

  ;; ------------------------------------------------------------
  ;; Tune module:
  ;; tune choice
  ;; ------------------------------------------------------------
  
  (require 'net_nl_tune)

  ;; -- option1 -- Basic tune choice
  (Param.set 'Tune_Method 'nl::basic_tune_choice)

  ;; -----------------------------------------------------------
  ;; Word module:
  ;; lexicon loopkup, grapheme-to-phoneme conversion, and
  ;; building prosodic structures for words 
  ;;------------------------------------------------------------

  ;; Phone set
  (require 'net_nl_phones)
  (Param.set 'PhoneSet 'net_nl)
  (PhoneSet.select 'net_nl)

  (require 'net_nl_lex)
  (require 'net_nl_lex_addenda)
  
  (Param.set 'Word_Method 'nl::word)

  ;; -- Option 1 -- TreeTalk memory-based grapheme-to-phoneme conversion
  ; (require 'net_nl_treetalk)
  ; (nl::setup_kunlex_treetalk)
  ; (lex.select "kunlex_treetalk")
  ; (set! nl::trace_treetalk t)
  ; FIX-ME: this mindlessly ignores any user preferences declared earlier
  
  ;; -- Option 2 -- Fonpars CS rewrite rules
  (nl::setup_kunlex_fonpars)
  (lex.select "kunlex_fonpars")

  ;; ------------------------------------------------------------
  ;; Pauses module:
  ;; pause insertion
  ;; ------------------------------------------------------------

  (require 'net_nl_pauses)
  (Param.set 'Pause_Method 'nl::pauses)

  ;; ------------------------------------------------------------
  ;; Postlex module:
  ;; postlexical rules
  ;; ------------------------------------------------------------

  (require 'net_nl_postlex)
  (set! postlex_rules_hooks (list nl::postlex_rules))

  ;; ------------------------------------------------------------
  ;; Duration module:
  ;; segment and pause durations
  ;; ------------------------------------------------------------
  
  ;; -- Option 1 -- Fixed durations
  ;; all segments are 100 milliseconds
  ; (Param.set 'Duration_Method 'Default)

  ;; -- Option 2 -- KUN rule-based duration prediction
  (require 'net_nl_dur_kun)
  (Param.set 'Duration_Method 'KUN_Duration)

  ;; -- Option 3 -- Lucent duration prediction (Esther Klabbers)
  ; (require 'net_nl_ib_dur_lucent)
  ; (Param.set 'Duration_Method 'Lucent_Duration)

  ;; ------------------------------------------------------------
  ;; Int_Target module:
  ;; fundamental frequency control
  ;; ------------------------------------------------------------

  ;; -- Option 1 -- Fixed F0
  ;(set! duffint_params '((start 180) (end 150)))
  ;(Parameter.set 'Int_Method 'DuffInt)
  ;(Parameter.set 'Int_Target_Method Int_Targets_Default)

  ;; -- Option 2 -- ToDI intonation according to the G&R model
  (require 'net_nl_int_todi)
  (Param.set 'Int_Target_Method 'ToDI-intonation)

  ;; FIX-ME: reestablish the ToDI manual option
  ;(Param.set 'Int_Method 'ToDI-manual)

  ;; ------------------------------------------------------------
  ;; Wave_Synth module: 
  ;; wave form synthesis
  ;; ------------------------------------------------------------
  
  (require 'mbrola)
  ;; use MBROLA diphone syntheiszer with nl3 diphone database
  (Param.set 'Synth_Method MBROLA_Synth)

  (cond
   ;; if on Cygwin, use Mbrola for Cywgin
   ((string-matches *ostype* ".*CYGWIN.*")
    (set! mbrola_progname (path-append (cadr etc-path) "mbrola.exe")))
   ;; if on Linux, use Mbrola for Linux
   ((string-matches *ostype* ".*Linux.*")
    (set! mbrola_progname (path-append (cadr etc-path) "mbrola")))
    ;; otherwise, assume it is in the path 
   (t
    (set! mbrola_progname "mbrola")))

  ;; Assume that a recent version of mbrola is used, 
  ;; which requires the -I flag
  ;; Because we need an extra parameter in the new version of mbrola
  ;; we add that parameter to the database "name"
  ;; FIX-ME: remove -I call
  (set! mbrola_database 
	(format nil
		"%s%s "
		;; MBROLA initialization file
		;; may be used to set synthesizer defaults
	 net_nl_ib_mbrola_dir "nl3/nl3" 
	 ))

  ;; set callback to restore some original values changed by this voice
  (set! current_voice_reset net_nl_ib_voice_reset)

  (set! current-voice 'net_nl_ib_mbrola)
)




(define (net_nl_ib_voice_backup)
"
\(net_nl_ib_voice_backup\)

Save some values of global variables for use in voice reset.
"
  (set! UttTypes_backup UttTypes)
) 


(define (net_nl_ib_voice_reset)
"
\(net_nl_ib_voice_reset\)

Reset global variables back to previous voice.
"
  (set! UttTypes UttTypes_backup)  
)



(proclaim_voice
 'net_nl_ib_mbrola
 '((language Dutch)
   (gender female)
   (dialect standard)
   (description
    "A Dutch female voice using Mbrola with the nl3 database,
developed within the Nextens project" )
   (builtwith festvox-1.2)))




(provide 'net_nl_ib_mbrola)
