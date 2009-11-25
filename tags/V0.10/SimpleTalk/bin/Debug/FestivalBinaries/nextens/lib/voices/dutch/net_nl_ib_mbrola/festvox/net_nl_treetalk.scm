;;; $Id: net_nl_treetalk.scm,v 1.3 2003/04/01 14:38:04 emarsi Exp $
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


;;; TreeTalk


(define (nl::tree_talk word)
"
\(nl::tree_talk WORD\)

Performs grapheme-to-phoneme conversion and syllabicifaction for WORD
using the TreeTalk method. Syllabification is taken to include
prosodic word phrasing and stress assignment. Returns a lemma in the
same format as the default function lex.lookup.
"
  (if nl::trace_treetalk
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "nl::tree_talk for word \"%s\"\n" word)
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")))
  (list word nil 
	(nl::syllabification 
	 (nl::graph_to_phon_conversion word))))


(define (nl::graph_to_phon_conversion word)
"
\(nl::graph_to_phon_conversion WORD\)

Perform grapheme-to-phoneme conversion for WORD. Returns a list of
phonemes.
"
;; depends on global vars nl::phon_padding, nl::phon_timbl, and
;; nl::phon_win_size
  (if nl::trace_treetalk
      (format t "*** Starting grapheme-to-phoneme conversion ***\n\n"))
  (let (;; letters is the list of all *lowercased* letters in the word
	(letters (symbolexplode (downcase word)))
	(start 0)
	(features "")
	len
	phon
	phonemes)
    (set! len (length letters)) ; size of the word
    ;; add padding before and after the word, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! letters (append nl::phon_padding 
			  letters 
			  nl::phon_padding))
    ;; move a window over the list of letters
    (while (< start len)
	   ;; Features is the string obtained by concatenating all
	   ;; letters/paddings symbols in the window.  A dummy class
	   ;; ('?') is appended, because this expected by Timbl
	   (set! features (string-append (nl::slice letters 
						start (+ start nl::phon_win_size))
					 " ?"))
	   ;; call Timbl with these features, which return the phoneme
	   ;; corresponding to the letter in the center of the window
	   (if nl::trace_treetalk
	       (format t "Calling Timbl with features %s " features))
	   (set! phon (intern (Timbl.classify nl::phon_timbl features)))
	   (if nl::trace_treetalk
	       (format t "returns class %s\n" phon))
	   (set! phonemes (append phonemes (list phon)))
	   (set! start (+ start 1)))
    ;; Finally, expand compressed phonemes to the original pair of
    ;; phonemes (this means an extra loop over all phonemes; can be
    ;; avoided by integrating expand_phonemes in the current function)
    (nl::expand_phonemes phonemes)))


(define (nl::expand_phonemes phonemes)
"
\(nl::expand_phonemes PHONEMES\)

Expands compressed phonemes in the list PHONEMES to the original pair
of phonemes. The mapping is defined is nl::expansion_map.
"
  (let (result)
    (mapcar
     (lambda (phon)
       ;; unless phoneme is empty 
       (if (not (string-equal phon nl::empty_sym))
	   ;; if phoneme has a key in nl::expansion_map, then append
	   ;; its corresponding value (i.e. a pair of phonemes) to the
	   ;; result list, else append the phoneme itself to the
	   ;; result list
	   (set! result (append result
				(or (cdr (assoc_string phon nl::expansion_map))
				    (list phon))))))
     phonemes)
    (if  nl::trace_treetalk
	 (format t "\nExpanding phonemes in %l\ngives %l\n\n" phonemes result)) 
    result))


(define (nl::syllabification phonemes)
"
\(nl::syllabification PHONEMES\)

Perform syllabification, which is taken to include prosodic word
phrasing and stress assignment, on the list of PHONEMES. Returns a
list of one or more prosodic words, which in turn consists of one or
more syllables.
"
;; Example: 
;; (t a f @ l b o m)
;; returns 
;; ((((t a) 1) ((f @ l) 0)) (((b o m) 2)))
;; (Note: this function may be stated more compact by factoring out
;; some repeated code, but it would make things even less clear ...)
  (if nl::trace_treetalk
      (format t "*** Starting syllabification ***\n\n"))
  (let ((start 0)
	padded_phons
	features
	result
	bnd
	syl
	syl_phons
	pwrd)
    (set! len (length phonemes))
    ;; add padding before and after the phonemes, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! padded_phons (append nl::syl_padding phonemes nl::syl_padding))
    ;; move a window over the list of phonemes
    (while phonemes
	   (if nl::trace_treetalk
	       (begin
		 (format t "Stress of current syllable: %l\n" syl)
		 (format t "Phonemes of current syllable: %l\n" syl_phons)
		 (format t "Syllables of prosodic word: %l\n" pwrd)
		 (format t "Current output: %l\n\n" result)))
	   ;; Features is the string obtained by concatenating all
	   ;; phonemes/padding symbols in the window.  A dummy class
	   ;; ('?') is appended, because this expected by Timbl
	   (set! features (string-append (nl::slice padded_phons start 
						(+ start nl::syl_win_size))
					 " ?"))
	   ;; Call Timbl with these features, which returns the type
	   ;; of boundary (see below) corresponding to the letter in
	   ;; the center of the window
	   (if nl::trace_treetalk
	       (format t "Calling Timbl with features %s " features))
	   (set! bnd (Timbl.classify nl::syl_timbl features))
	   (if nl::trace_treetalk
	       (format t "returns class %s,\n" bnd))
	   ;; Unless there is no boundary at all (i.e. '+'),
	   ;; take action according to the type of boundary
	   (if (not (string-equal bnd '+))
	       (begin
		 ;; Construct a syllable by consing syl_phons (the
		 ;; list of collected phonemes) and syl (the last
		 ;; encountered stress level: 0, 1, or 2)
		 (set! syl (cons syl_phons syl)) 
		 (cond
		  ;; 0 --> syllable boundary (without stress)
		  ((string-equal bnd 0)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl without stress
		   (set! syl '(0))
		   (if nl::trace_treetalk
		       (format t "which is an unstressed syllable boundary.\n")))
		  
		  ;; 1 --> syllable boundary with primary stress
		  ((string-equal bnd 1)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl with primary stress
		   (set! syl '(1))
		   (if nl::trace_treetalk
		       (format t "which is a primary stressed syllable boundary.\n")))

		  ;; 2 --> syllable boundary with secundary stress
		  ((string-equal bnd 2)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl with secundary stress
		   (set! syl '(2))
		   (if nl::trace_treetalk
		       (format t "which is a secundary stressed syllable boundary.\n")))

		  ;; 3 --> both prosodic word and syllable boundary (without stress)
		  ((string-equal bnd 3)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syl without stress
		   (set! syl '(0))
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both an unstressed syllable boundary\n")
			 (format t "and a prosidic word boundary.\n"))))

		  ;; 4 --> both prosodic word and syllable boundary 
		  ;;       with primary stress
		  ((string-equal bnd 4)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syl with primary stress
		   (set! syl '(1))
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both a primary stressed syllable boundary\n")
			 (format t "and a prosidic word boundary.\n"))))

		  ;; 5 --> both prosodic word and syllable boundary 
		  ;;       with secundary stress
		  ((string-equal bnd 5)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syllable with secundary stress
		   (set! syl '(2))
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both a secundary stressed syllable boundary\n")
			 (format t "and a prosidic word boundary.\n")))) 
		  )
		 ;; in any case, empty the list of collected phonemes
		 (set! syl_phons nil))
	       (if nl::trace_treetalk
		       (format t "which is not a boundary.\n"))
	       )
	   ;; add current phoneme to phonemes of current syllable 
	   (set! syl_phons (append syl_phons (list (car phonemes))))
	   (set! phonemes (cdr phonemes))					      
	   (set! start (+ start 1)))
    ;; Flush remaining stuff: 
    ;; first, construct the final syllable by consing syl_phons (the list of
    ;; collected phonemes) and syl (the last encountered stress level)
    (set! syl (cons syl_phons syl)) 
    ;; next, add final syl to the last prosodic word
    (set! pwrd (append pwrd (list syl)))
    ;; finally, add last prosodic word to the current result
    (set! result (append result (list pwrd)))
    (if nl::trace_treetalk
	(begin
	  (format t "Stress of current syllable: %l\n" syl)
	  (format t "Phonemes of current syllable: %l\n" syl_phons)
	  (format t "Syllables of prosodic word: %l\n\n" pwrd)
	  (format t "Final output: %l\n\n" result)))
    ;; FIX-ME:
    ;; apply well-formed filter to prevent things like
    ;; two primary streses in a single word 
    result))
	   

;;; ------------------------------------------------------------
;;; Support functions
;;; ------------------------------------------------------------

(define (nl::construct_padding_list win_size pad_sym)
"
\(nl::construct_padding_list WIN_SIZE PAD_SYM\) 

Returns a list of padding symbols PAD_SYM for use with a window with
of size WIN_SIZE.
"
  (let ((half_win_size (/ win_size 2))
	l)
    (while (> half_win_size 1)
	   (set! l (cons pad_sym l))
	   (set! half_win_size (- half_win_size 1))) 
    l))  
  

(define (nl::slice list start end)
"
\(nl::slice LIST START END\)

Return the string obtained by concatenating all elements of LIST from
position START to END.
"
  (let ((l (nth_cdr start list))
	(str ""))
    (while (and l (< start end))
	   (set! str (string-append str (car l) " "))
	   (set! l (cdr l))
	   (set! start (+ start 1)))
    str))


;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------

;;; (Note: using global vars to speed up processing, hopefully...)

(defvar nl::trace_treetalk nil
"
If true, trace grapheme-to_phoneme conversion and syllabification
in TreeTalk. See nl::treetalk.
")

(defvar nl::expansion_map
      '((1 i j)       ;; malaria /malarija/
	(3 e j)       ;; ateist /atejIst/
	(4 p j)       ;; computer /kOmpjut@r/
	(5 A j)       ;; flamboyant /flAmbwAjAnt/
	(6 d j)       ;; managen /mEn@dj@n/
	(7 y j)       ;; fonduen fOndyj@n
	(8 k j)       ;; barbecuen /bArb@kjuw@n/
	(9 y w)       ;; actuele /Aktywel@/
	(# o w)       ;; asteroide /Ast@rowid@/
	({ u w)       ;; barbecue /bArb@kjuw/
	(} k s)       ;; axon /AksOn/
	([ t s)       ;; martiale /mArts3al@/
	(] t S))     ;; helices /helitSEs/
"
Assoc list that defines the mapping from compressed phonemes to the
original pair of phonemes. See nl::expand_phonemes.
")


(defvar nl::empty_sym '-
"
The symbol for empty phones (epsilons). See nl::expand_phonemes.
")


(defvar nl::pad_sym '_
"
The symbol used for padding a window when windowing letters/phonemes
at the begin and end of a word. See nl::phon_padding and
nl::syl_padding.
")


(defvar nl::phon_win_size 9
"
The size of window used for windowing letters during
graheme-to-phoneme conversion. See nl::graph_to_phon_conversion
")

(defvar nl::phon_padding 
  (nl::construct_padding_list nl::phon_win_size
			      nl::pad_sym)
" 
The list of padding symbols to be appended before and after the word
during graheme-to-phoneme conversion. See nl::graph_to_phon_conversion
")


(defvar nl::syl_win_size 9
" 
The size of window used for windowing letters during
syllabification. See nl::syllabification
")

(defvar nl::syl_padding
  (nl::construct_padding_list nl::syl_win_size
			      nl::pad_sym)
"
The list of padding symbols to be appended before and after the word
during syllabification. See nl::syllabification
")


;;; ------------------------------------------------------------
;;; Start Timbl instances for TreeTalk
;;; ------------------------------------------------------------

(define (nl::init_tree_talk)
"
\(nl::init_tree_talk\)

Initialize TreeTalk by reading the Timbl instance bases
and setting Timbl options
\(see nl::phon_timbl and nl::syl_timbl\)
" 
  ;; destruct any previous instance bases 
  (nl::destruct_tree_talk)
  ;; start a new Timbl instance for grapheme-to-phoneme conversion,
  ;; and read the igtree
  (set! nl::phon_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::phon_timbl 
			   (string-append net_nl_ib_mbrola_dir "treetalk-ibs/phon.igtree")) 

  ;; start a new Timbl instance for syllabification, and read the
  ;; igtree
  (set! nl::syl_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::syl_timbl 
			   (string-append net_nl_ib_mbrola_dir "treetalk-ibs/syl.igtree")))


(define (nl::destruct_tree_talk)
"
\(nl::init_tree_talk\)

Destruct the Timbl instance bases used by TreeTalk 
\(see nl::phon_timbl and nl::syl_timbl\)
"
  (if (symbol-bound? 'nl::phon_timbl)
      (Timbl.destroy nl::phon_timbl))
  (if (symbol-bound? 'nl::syl_timbl)
      (Timbl.destroy nl::syl_timbl)))


(provide 'net_nl_treetalk)


