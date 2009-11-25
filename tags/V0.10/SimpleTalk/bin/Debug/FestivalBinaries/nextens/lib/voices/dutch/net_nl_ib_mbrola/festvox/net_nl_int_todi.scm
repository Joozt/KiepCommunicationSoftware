;;; $Id: net_nl_int_todi.scm,v 1.17 2003/04/01 14:38:04 emarsi Exp $
;;;
;;; by Joop Kerkhoff & Erwin marsi
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



;;;-----------------------------------------------------------------------------
;;; F0 target calculation
;;;-----------------------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Scaling of F0 targets
;;; ------------------------------------------------------------

;;; Phonetic implementation of scaling of F0 targets
;;; according to the model descibed in:
;;;
;;; Rob van den Berg, Carlos Gussenhoven and Toni Rietveld,
;;; "Downstep in Dutch: implications for a model".
;;; In: G.J. Docherty and D.R. Ladd (eds.),
;;; "Papers in Laboratory Phonology II", 
;;; Cambridge: Cambridge University Press, 1992, pp. 335-359.  


;;; Parameters of the target scaling model

;;; Reference frequency
(Parameter.set 'Fr 105)
;;; Range
(Parameter.set 'N 1.65)
;;; Register width
(Parameter.set 'W 1.50)
;;; Accentual downstep factor
(Parameter.set 'da 0.95)
;;; Phrasal downstep factor
(Parameter.set 'dp 0.95)
;;; Timing star position in vowel
(Parameter.set 'StarPos 0.1)
;;; Speaking Rate
(Parameter.set 'SpeakRate 2)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NO TODI RULES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (No_ToDI utt)
 
 (format t "No ToDI rules!\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;TODI TARGET CALCULATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ToDI-intonation utt)

 (set! ToDItrace 0)		;set ToDItrace 1 for trace output
 (if (> ToDItrace 0) (format t "### Starting F0 calculation ###\n"))

; define and get settings
   (set! StarPos (Parameter.get 'StarPos))	; get position Star in vowel
   (set! ToTime 0.10)				; ToTime 100 ms
   (set! FromTime 0.10)				; FromTime 100 ms

  (Make-TargetSymbols utt)		; convert ToDISymbols in targets (symbols)
  (Timing-AD-boundaries utt)		; calculate the begin and end times of AD's
  (Star-Targets utt)			; calculate F0 and Timing Stars
  (No-Star-Targets utt)			; calculate F0 and Timing No Stars
  (Target_Modifications utt)		; small modifications of previous calculations
  (Locate-Targets-Phoneme utt)		; locate targets on phonemes
); end define ToDI-intonation


;===========================
;TIMING AD BOUNDARIES
;===========================
(define (Timing-AD-boundaries utt)

    (if (> ToDItrace 0) (format t "\nTiming AD Boundaries\n" ))

    (mapcar
     (lambda (Target)

       (set! TargetSymbol (item.feat Target "name"))
;       (set! ToDIsymbol (item.feat (item.parent Target) "name"))
;       (set! NextTarget (item.next_leaf Target))
;       (set! NextSymbol (item.feat NextTarget "name"))

   
       (if (member_string TargetSymbol '(LB1 HB1 XB1)) 			; %L, %H or %HL, calculate AD begin
          (begin
            (set! star (item.parent Target))
            (set! syl  (item.parent (item.relation star 'Syl-Int)))
            (set! word (item.parent (item.parent (item.parent (item.relation syl 'ProsTree)))))
            (set! segm (item.daughter1_to (item.relation word 'ProsTree) 'Segment))
            (set! begin_dur ( - (item.feat segm "end") (item.feat segm "dur")))
;   (format t "Ad-begin: %s Syl: %s %s\n" TargetSymbol (item.feat segm "name") begin_dur)
       )); end if

;save TADB for each target
   (item.set_feat Target 'TADB begin_dur)

       (if (member_string TargetSymbol '(LE2 HE2 XE1))			; L%, H% or %, calculate AD end
          (begin
            (set! star (item.parent Target))
            (set! syl  (item.parent (item.relation star 'Syl-Int)))
            (set! word (item.parent (item.parent (item.parent (item.relation syl 'ProsTree)))))
            (set! segm (item.daughtern_to (item.relation word 'ProsTree) 'Segment))
             (while (string-equal (item.feat segm "ph_cvox") "-")	; search last voiced segment
               (set! segm (item.prev segm)))
            (set! end_dur  (item.feat segm "end") )
            (item.set_feat Target 'TADE end_dur)
;   (format t "Ad-end: %s Syl: %s %s\n" TargetSymbol (item.feat segm "name") end_dur)
       )); end if
      )
   (utt.relation.leafs utt 'ToneTargets)
  )
  
; save TADE for each target
    (mapcar
     (lambda (Target)
          
;  (set! TADB1 (item.feat Target "TADE"))
;  (format t "Adbegin2: %s\n" TADB1)
;  (if (> TADB1 0) (format t "Adbegin2: %s\n" TADB1))

       (if (item.feat Target "TADE")
         (set! TADE (item.feat Target "TADE"))
         (item.set_feat Target 'TADE TADE))
     )
   (reverse (utt.relation.leafs utt 'ToneTargets))
  )
)


(define (Make-TargetSymbols utt)

    (utt.relation.create utt 'ToneTargets)				; save TargetSymbols in ToneTargets
    (if (> ToDItrace 0) (format t "\nTarget Symbols\n" ))

    (mapcar
     (lambda (ToDISymbol)

      (set! TargetSymbol (item.feat ToDISymbol "name"))
      (if (> ToDItrace 0) (format t " %s" TargetSymbol))

      (cond
       ((string-equal TargetSymbol "%L")
              (Add_TargetSymbols utt ToDISymbol "LB1" "LB2" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "%H")
              (Add_TargetSymbols utt ToDISymbol "HB1" "HB2" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "%HL")
              (Add_TargetSymbols utt ToDISymbol "XB1" "XB2" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "H*L") 
              (Add_TargetSymbols utt ToDISymbol "H" "HH" "l" "-" "-" "-"))
       ((string-equal TargetSymbol "!H*L") 
              (Add_TargetSymbols utt ToDISymbol "!H" "HH" "l" "-" "-" "-"))
       ((string-equal TargetSymbol "H*+L")
              (Add_TargetSymbols utt ToDISymbol "H" "HH" "+l" "-" "-" "-"))
       ((string-equal TargetSymbol "!H*+L") 
              (Add_TargetSymbols utt ToDISymbol "!H" "HH" "+l" "-" "-" "-"))
       ((string-equal TargetSymbol "H*") 
              (Add_TargetSymbols utt ToDISymbol "H" "HH" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "!H*") 
              (Add_TargetSymbols utt ToDISymbol "!H" "HH" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "L*")
              (Add_TargetSymbols utt ToDISymbol "L" "x" "x" "-" "-" "-"))
       ((string-equal TargetSymbol "L*H")
              (Add_TargetSymbols utt ToDISymbol "L" "x" "x" "h" "-" "-"))
       ((string-equal TargetSymbol "L*HL")         		; delayed target
                (Add_TargetSymbols utt ToDISymbol "L" "d" "d" "H" "HH" "l"))
       ((string-equal TargetSymbol "L*!HL")         		; delayed target, downstepped
                (Add_TargetSymbols utt ToDISymbol "L" "d" "d" "!H" "HH" "l"))
       ((string-equal TargetSymbol "H*LH")         		; pre-nuclear H*L
              (Add_TargetSymbols utt ToDISymbol "H" "HH" "+l" "h" "-" "-"))
       ((string-equal TargetSymbol "!H*LH")         		; pre-nuclear H*L
              (Add_TargetSymbols utt ToDISymbol "!H" "HH" "+l" "h" "-" "-"))
       ((string-equal TargetSymbol "L%")
              (Add_TargetSymbols utt ToDISymbol "LE1" "LE2" "-" "-" "-" "-"))
       ((string-equal TargetSymbol "H%")
              (Add_TargetSymbols utt ToDISymbol "HE1" "HE2" "HE3" "-" "-" "-"))
       ((string-equal TargetSymbol "%")
              (Add_TargetSymbols utt ToDISymbol "XE1" "-" "-" "-" "-" "-"))
       ((member_string TargetSymbol '("," "."))
              (Add_TargetSymbols utt ToDISymbol "P" "-" "-" "-" "-" "-"))
        ))
     (utt.relation.items utt 'Intonation)
    )

)


;===========================
;STAR TARGETS and BOUNDARIES
;===========================
(define (Star-Targets utt)

   (if (> ToDItrace 0) (format t "\nCalculate Star Targets (Boundaries and Stars)\n" ))

;
; calculate F0High and F0Low using Ladd
;
   (set! PhrasDown 1)						; counter phrasal downstep
   (set! AccentDown 1)						; counter accental downstep
   (set! F0High (target-scaling  1 AccentDown PhrasDown 1 1))  	;arguments (target-scaling T m n Sa Sp)
   (set! F0Low  (target-scaling -1 AccentDown PhrasDown 1 1))
   (set! Range (- F0High F0Low))

   (if (> ToDItrace 0)
     (begin
         (format t "SETTINGS: StarTime ToTime FromTime: %s %s %s\n" StarPos ToTime FromTime)
         (format t "          F0High F0Low: %s %s\n"F0High F0Low)))

    (mapcar
     (lambda (Target)

       (set! TargetSymbol (item.feat Target "name"))
;       (set! ToDIsymbol (item.feat (item.parent Target) "name"))
   
;===================
; LEFT AD boundary
;===================
;
; left boundary: TADB
;
   (if (member_string TargetSymbol '(LB1 HB1 XB1))			; %lL, %H, or %HL
     (begin
       (set! TADB (item.feat Target "TADB"))
       (if (member_string TargetSymbol '(LB1))
        (item.set_feat Target 'Fzero (nint (+ F0Low (* 0.35 Range)))));Fzero = FLow + 35% (FHigh-FLow)
       (if (member_string TargetSymbol '(HB1))
          (item.set_feat Target 'Fzero F0High))				;Fzero = FHigh
       (if (member_string TargetSymbol '(XB1))				;first target %HL and %H are the same
            (item.set_feat Target 'Fzero F0High))
      (item.set_feat Target 'Time TADB)
   )); end if

;===================
;   STAR TARGETS
;===================
   (if (member_string TargetSymbol '(H !H L P))				; H*, !H*, L* or Punctuation
     (begin

        ;
        ; calculation duration StarTime Target
        ;
       (set! star (item.parent Target))
       (set! syl (item.parent (item.relation star 'Syl-Int)))
       (set! target (item.daughter1 (item.relation syl 'ProsTree)))
       ; search vowel in syllable (in nucleus!)
       (if (string-equal (item.feat target "name") "Onset") (set! target (item.next target)))
       (set! tsegm (item.relation (item.daughter1 target) 'Segment))
       (set! psegm (item.prev (item.relation tsegm 'Segment)))
       (set! begin_dur (item.feat psegm "end"))
       (set! this_seg_dur (item.feat tsegm "dur"))
       (set! StarTime (+ begin_dur (* this_seg_dur StarPos)))	;star position

;   (format t "Target: %s pseg %s tsegm %s \n" TargetSymbol (item.feat psegm "name") (item.feat tsegm "name"))

       (if (member_string TargetSymbol `(H !H))            		; H* = H HH and !H = !H HH
         (begin
           (if (string-equal TargetSymbol "!H")  			;arguments (target-scaling T m n Sa Sp)
              (begin
                 (set! AccentDown (+ AccentDown 1))
                 (set! F0High (target-scaling 1 AccentDown PhrasDown 1 1))
                 (set! Range (- F0High F0Low))
	      ))
           (set! NextTarget (item.next_leaf Target))
           (item.set_feat Target 'Fzero F0High)
           (item.set_feat Target 'Time StarTime)
           (item.set_feat NextTarget 'Fzero F0High)
           (item.set_feat NextTarget 'Time (+ StarTime 0.02))))
      (if (string-equal TargetSymbol "L")              			; L* = L x x/ L d d
         (begin
           (set! NextTarget (item.next_leaf Target))
           (set! NextNextTarget (item.next_leaf NextTarget))
           (item.set_feat Target 'Fzero F0Low)
           (item.set_feat Target 'Time StarTime)
           (item.set_feat NextTarget 'Fzero F0Low)
           (item.set_feat NextTarget 'Time (+ StarTime 0.02))		; length 20 ms
           (item.set_feat NextNextTarget 'Fzero (+ F0Low 5))		; size rise 5 Hz
           (item.set_feat NextNextTarget 'Time (+ StarTime 0.035))))	; length rise 15 ms
      (if (string-equal TargetSymbol "P")            			; Punctuation
         (begin
           (item.set_feat Target 'Fzero (nint (/ (+ F0High F0Low) 2)))
           (item.set_feat Target 'Time StarTime)))

   )) ;end if

;===================
; RIGHT AD boundary
;===================
;
; right boundary: TADE
;
   (if (member_string TargetSymbol '(LE2 HE2 XE1))			; L%, H% or %
     (begin
       (set! TADE (item.feat Target "TADE"))				; calculated AD end
       (if (string-equal TargetSymbol "LE2") 				; second target L%
         (item.set_feat Target 'Fzero (nint (- F0Low (* 0.2 Range))))); Fzero = FLow - 20%(FHigh-FLow)
       (if (string-equal TargetSymbol "XE1")				; target %
         (begin
           (if (member_string PrevSymbol `(H !H HH))
              (item.set_feat Target 'Fzero F0High)			; keep high
              (item.set_feat Target 'Fzero (nint (- F0High (* 0.2 Range))))) ; lower F0
       ))
       (if (string-equal TargetSymbol "HE2") 				; second target H%
         (begin
           (item.set_feat Target 'Time (- TADE 0.02))			; set target HE2
           (item.set_feat Target 'Fzero (nint (+ F0Low (* 0.75 Range))))
           (set! Target (item.next_leaf Target))			; set target HE3
           (item.set_feat Target 'Fzero (nint (+ F0Low (* 0.75 Range))))))
           (item.set_feat Target 'Time TADE)
   )); end if
   
     (set! PrevSymbol TargetSymbol)					;save this target  
      )
   (utt.relation.leafs utt 'ToneTargets)
  )
)


;
;======================
; NO STAR TARGETS
;======================
;
(define (No-Star-Targets utt)

   (if (> ToDItrace 0) (format t "\nCalculate No Star Targets\n" ))

;
; Reset F0High/F0Low
;
    (set! PhrasDown 1)						; counter phrasal downstep
    (set! AccentDown 1)						; counter accental downstep
    (set! F0High (target-scaling  1 AccentDown PhrasDown 1 1))  	; arguments (target-scaling T m n Sa Sp)
    (set! F0Low  (target-scaling -1 AccentDown PhrasDown 1 1))
    (set! Range (- F0High F0Low))

    (mapcar
     (lambda (Target)

      (set! TargetSymbol (item.feat Target "name"))
      (set! ToDIsymbol (item.feat (item.parent Target) "name"))

      (if (string-equal TargetSymbol "!H")  		; change F0High after a downstep
         (begin
           (set! AccentDown (+ AccentDown 1))
           (set! F0High (target-scaling 1 AccentDown PhrasDown 1 1))))

      ;Special Case "Delayed Fall" (timing of LddHl or Ldd!Hl)
      (if (and (string-equal TargetSymbol "d")
               (string-equal (item.feat PrevTarget "name") "d"))
         (begin
         
;   (format t "Delayed Fall 2n %s %s\n" TargetSymbol (item.feat (item.next_leaf Target) "name") )
           (set! Time_d2 (item.feat Target "Time"))			; get time target "d2"
           (set! Target_H (item.next_leaf Target))			; target H or !H
           (set! Target_HH (item.next_leaf Target_H))			; target HH
           (set! NextTarget (item.next_leaf Target_HH))			; first target after HH
           (set! NextTime (item.feat NextTarget "Time"))

           (while (equal? NextTime 0)					; search next target<>0
             (begin
               (set! NextTarget (item.next_leaf NextTarget))
               (set! NextTime (item.feat NextTarget "Time"))))
           (set! TargetTime (OLAPT Time_d2 FromTime 0 NextTime))
           
;    (format t "Delayed Fall d2 %s TT %s NT %s\n" Time_d2 TargetTime NextTime )
           (item.set_feat Target_H 'Time TargetTime)			; save target H
           (set! Target Target_HH)					; set target HH
           (set! TargetTime (OLAPT TargetTime 0.020 0 NextTime))
           (item.set_feat Target 'Time TargetTime)			; save target HH
      ))
      
      (if (member_string TargetSymbol '(LB2 HB2 XB2 l +l h LE1 HE1))
       (begin

;   (format t "Target: %s pseg %s tsegm %s \n" TargetSymbol (item.feat psegm "name") (item.feat tsegm "name"))

       (set! PrevTime (item.feat PrevTarget "Time"))
       (set! NextTarget (item.next_leaf Target))
       (set! NextTime (item.feat NextTarget "Time"))
       (while (equal? NextTime 0)					;search next target<>0
          (begin
             (set! NextTarget (item.next_leaf NextTarget))
             (set! NextTime (item.feat NextTarget "Time"))))

       (if (string-equal TargetSymbol "LB2")				;second target %L
         (begin
           (set! PrevFzero (item.feat PrevTarget "Fzero"))
           (set! NextFzero (item.feat NextTarget "Fzero"))
           (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))
           (set! TargetFzero (* PrevFzero 0.9))				; slope 10%
           (set! TargetFzero (OLAPF PrevTime PrevFzero TargetTime TargetFzero NextTime NextFzero))))
       (if (string-equal TargetSymbol "HB2")				;second target %L
         (begin
           (set! PrevFzero (item.feat PrevTarget "Fzero"))
           (set! NextFzero (item.feat NextTarget "Fzero"))
           (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))
           (set! TargetFzero (* PrevFzero 0.9))				; slope 10%
           (set! TargetFzero (OLAPF PrevTime PrevFzero TargetTime TargetFzero NextTime NextFzero))))
       (if (string-equal TargetSymbol "XB2")				;second target %L
         (begin
           (set! PrevFzero (item.feat PrevTarget "Fzero"))
           (set! NextFzero (item.feat NextTarget "Fzero"))
           (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))
           (set! TargetFzero (nint (+ F0Low (* 0.35 Range))))		; Flow + 35% van de range
           (set! TargetFzero (OLAPF PrevTime PrevFzero TargetTime TargetFzero NextTime NextFzero))))
       (if (member_string TargetSymbol '(l +l))				;default target l/+l
         (begin
           (set! TargetTime (OLAPT PrevTime FromTime 0 NextTime))
           (set! TargetFzero F0Low)))
       (if (string-equal TargetSymbol "l")				;overrule previous default targets l
         (begin
           (cond
            ((member_string  (item.feat NextTarget "name") '(H !H L))	;l H, l !H and l L
             (begin
               (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))))   ;if H*L (!)H*L
            ((string-equal (item.feat NextTarget "name") "L")		;l L
             (begin
               (set! TargetTime (OLAPT PrevTime FromTime 0 NextTime))
               (set! TargetFzero (+ F0Low 15))))
            ((member_string  (item.feat NextTarget "name") '(HE1 HE2 HE3 LE1 LE2)) 	;l H% and l L%
             (begin
              (set! TargetTime (OLAPT PrevTime 0.1 0 NextTime))			; timing l in almost the middle
;     (format t "l voor final H of L: TT %s PT %s NT %s\n" TargetTime PrevTime NextTime )
               (set! TargetFzero F0Low)))						; if not enough time
            ((string-equal (item.feat NextTarget "name") "XE1")				;l %
             (begin
               (set! TargetTime NextTime)
               (set! TargetFzero (item.feat NextTarget "Fzero")))))
       ))
       (if (string-equal TargetSymbol "h")
         (begin
           (set! TargetTime (OLAPT PrevTime FromTime 0 NextTime))	;default Timing target h
           (set! TargetFzero F0High)					;default Fzero target h
           ;
	   ;overrule previous default target h
	   ;
           (if (string-equal (item.feat PrevTarget "name") "+l")	;H*LH, prehead
              (begin
                 (set! TargetFzero (item.feat NextTarget "Fzero"))
                 (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))))	;set h ToTime next target
           (if (string-equal (item.feat PrevTarget "name") "x")		;L*H, geen delayed fall
              (set! TargetFzero (nint (- F0High (* Range 0.2)))))	;F0 = Fhigh - 0.2(Fhigh-Flow)
       ))
       (if (string-equal TargetSymbol "LE1")				;target L%
         (begin  
           (set! PrevFzero (item.feat PrevTarget "Fzero"))		;first target LE1
           (set! NextFzero (item.feat NextTarget "Fzero"))
           (if (string-equal (item.feat PrevTarget "name") "HH")
              (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))
              (set! TargetTime (OLAPT PrevTime 0 0.4 NextTime))
           )           
           (set! TargetFzero  PrevFzero )))
       (if (string-equal TargetSymbol "HE1")				;target H%
         (begin
           (set! PrevFzero (item.feat PrevTarget "Fzero"))
           (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))           
           (item.set_feat Target 'Fzero PrevFzero)			; first target HE1 in H%
           (item.set_feat Target 'Time TargetTime)
           (set! Target (item.next_leaf Target))			; second target HE2 in H%
           (item.set_feat Target 'Fzero (nint (+ PrevFzero (* 0.75 Range))))
           (set! Target (item.next_leaf Target))			; third target HE3 in H%
           (set! TargetFzero (nint (+ PrevFzero (* 0.75 Range))))
           (set! TargetTime (item.feat Target "Time"))))
 
        (item.set_feat Target 'Fzero TargetFzero)
        (item.set_feat Target 'Time TargetTime)
      )) ;end if

     (set! PrevTarget Target)						;keep this target          
     )
   (utt.relation.leafs utt 'ToneTargets)
  )
)


;
; Modifications of the calulated Targets
;
(define (Target_Modifications utt)

   (if (> ToDItrace 0) (format t "\nTarget_Modifications\n" ))
  
   (mapcar
    (lambda (Target)

       (set! TargetSymbol (item.feat Target "name"))
       (set! NextTarget (item.next_leaf Target))

;
; Fzero Modification: l + 15% in case (H* l H*) or (H* l L*)
; 
       (if (and (member_string TargetSymbol '(l +l))				; modification l value
                (string-equal (item.feat PrevTarget "name") "HH")
                (member_string  (item.feat NextTarget "name") '(H !H L h)))	;also H*LH
         (begin
           (set! TargetFzero (item.feat Target "Fzero"))
           (set! TargetFzero (nint (* TargetFzero 1.15)))
           (item.set_feat Target 'Fzero TargetFzero)
        ))
        
;
; Fzero Modification: h - 10% in case (L* h H*) or (L* h L*)
;
       (if (and (string-equal TargetSymbol "h")				      ; modification h value
                (member_string  (item.feat PrevTarget "name") '(d x +l))
                (member_string  (item.feat NextTarget "name") '(H !H L)))
         (begin
           (set! TargetFzero (item.feat Target "Fzero"))
           (set! TargetFzero (nint (* TargetFzero 0.9)))
           (item.set_feat Target 'Fzero TargetFzero)
        ))           

;
; Time Modification: move HH to next H in case (H* H*)
;

       (if (and (string-equal TargetSymbol "HH")			; change HH Timing
                (member_string (item.feat NextTarget "name") '(H !H)))
         (begin
           (set! NextTime (item.feat NextTarget "Time"))
           (set! PrevTime (item.feat PrevTarget "Time"))
           (set! TargetTime (OLAPT PrevTime 0 ToTime NextTime))
; (format t "\nFound : %s %l %l %l\n"  TargetSymbol PrevTime TargetTime NextTime) 

           (item.set_feat Target 'Time TargetTime)
        ))
       
      (set! PrevTarget Target)						;keep this target          
    )
   (utt.relation.leafs utt 'ToneTargets)
  ); end mapcar
); end define


(define (Locate-Targets-Phoneme utt)

   (if (> ToDItrace 0) (format t "\nLocate Targets on Segments\n" ))
  
  (utt.relation.create utt 'Target)
  
  (set! segm (item.next (utt.relation.first utt 'Segment)))   		; first segment AD
  (set! EndTime (item.feat segm "end"))
 
   (mapcar
    (lambda (TargetSymbol)

      (set! TargetTime (item.feat TargetSymbol "Time"))
      
; skip same time targets     
(if (item.next_leaf TargetSymbol)
(begin
      (set! NextTargetTime (item.feat (item.next_leaf TargetSymbol) "Time"))
      (if (equal? NextTargetTime TargetTime)
        (begin
           (set! TargetSymbol (item.next_leaf TargetSymbol))
           (set! TargetTime (item.feat TargetSymbol "Time"))
       ))
))

      (set! TargetFzero (item.feat TargetSymbol "Fzero"))
      (set! TargetName (item.feat TargetSymbol "name"))
      
      (while (> TargetTime EndTime)					; searching phoneme
        (begin
          (set! segm (item.next segm))
          (set! EndTime (item.feat segm "end"))))
        
      (set! TFtargets (utt.relation.append utt
                                           'Target
                                           (list TargetName)))
      (item.set_feat TFtargets 'f0 TargetFzero)
      (item.set_feat TFtargets 'pos TargetTime)
      (if (not (item.relation segm 'Target))
             (utt.relation.append utt 'Target segm))
      (item.relation.append_daughter segm 'Target TFtargets)

   (if (> ToDItrace 0) (format t "TargetSymbol %s %d %f on %s\n" TargetName TargetFzero TargetTime (item.feat segm "name")))

    )
   (utt.relation.leafs utt 'ToneTargets)
  ); end mapcar
); end define


(define (Add_TargetSymbols utt Symbol C1 C2 C3 C4 C5 C6)
 
   (utt.relation.append utt 'ToneTargets Symbol)
   (if (not (string-equal C1 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C1)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
   (if (not (string-equal C2 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C2)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
   (if (not (string-equal C3 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C3)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
   (if (not (string-equal C4 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C4)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
   (if (not (string-equal C5 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C5)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
   (if (not (string-equal C6 "-"))
    (begin 
      (set! ToneSym (utt.relation.append utt 'ToneTargets (list C6)))
      (item.relation.append_daughter Symbol 'ToneTargets ToneSym)))
)







;; FIX-ME: this doesn't work any more!

(define (ToDI-manual utt)
  (utt.relation.create utt 'Intonation)
  (utt.relation.create utt 'Syl-Int)
  (let (FirstWord LastWord)

    ;; Avoid illegal ToDIstreams:
    ;; check for initial and final boundaries,
    ;; and insert defaults when missing
    (set! FirstWord (utt.relation.first utt 'Word))
    (set! LastWord (utt.relation.last utt 'Word))
    (if (equal? (item.feat FirstWord 'ibnd) 0)
	(item.set_feat FirstWord 'ibnd "%L"))
    (if (equal? (item.feat LastWord 'fbnd) 0)
	(item.set_feat LastWord 'fbnd "L%"))
    
    ;; copy ToDI symbols from the words to the Intonation relation,
    ;; and associate these intonational events with the appropriate
    ;; syllables over the relation 'Syl-Int
    (mapcar
     (lambda (wrd)
       ;; initial boundary tone
       (if (member_string (item.feat wrd 'ibnd) ToDIInitBnd)
	   (SetWordInitBnd utt wrd (item.feat wrd 'ibnd)))
       ;; pitch accent
       (if (member_string (item.feat wrd 'acc) ToDIAcc)
	   (SetWordAcc utt wrd (item.feat wrd 'acc)))
       ;; final boundary tone
       (if (member_string (item.feat wrd 'fbnd) ToDIFinalBnd)
	   (SetWordFinalBnd utt wrd (item.feat wrd 'fbnd)))
	;; FIX-ME: check for illegal ToDI symbols (?)
       )
     (utt.relation.items utt 'Word))))





;;;-----------------------------------------------------------------------------
;;; Overlap functions
;;;-----------------------------------------------------------------------------

;
; OLAPT overlap time function 
;
(define (OLAPT Tleft Dleft Dright Tright)
(let (Ttarget)
"Return a time value between Tleft and Tright depending on
 the values Dleft (e.g. FromTime) en Dright (e.g. ToTime)"

   (set! Ttarget 0.0)
   (if (and (equal? Dleft 0.0)(> Dright 0.0)) (set! Ttarget (- Tright Dright)))
   (if (and (> Dleft 0.0)(equal? Dright 0.0)) (set! Ttarget (+ Tleft Dleft)))
 
   (if (equal? Ttarget 0.0)
    (begin
     (if (equal? Dleft Dright)
       (set! Ttarget (/ (+ Tleft Tright) 2))
       (set! Ttarget (+ (/ (* (- Tright Tleft) Dleft) (+ Dleft Dright)) Tleft)))))
 
; check limits
      (if (< Ttarget Tleft) (set! Ttarget Tleft))
      (if (> Ttarget Tright) (set! Ttarget Tright))

;(format t "\ninput OLAPT: %f %f %f %f\n" Tleft Dleft Dright Tright)
;(format t " OLAPT end: %f " Ttarget)

   Ttarget
));end define OLAPT


(define (SLOPE time1 freq1 time2 freq2)
(let (NFzero)
; usage SLOPE with time1 < time2 !!!
; change freq1 to get the correct slope value
                                   
; (format t " INPUT SLOPE: t1:%.1f f1:%.1f t2:%.1f f2:%.1f\n" time1 freq1 time2 freq2)

 (set! maxsteil (/ (- (target-scaling 1 1 1 1 1) (target-scaling -1 1 1 1 1)) 0.100))
		 				; aanname TOTIME = 100 ms
						; is steilheid groter => corrigeer dan
; calculate deltaT
    (set! deltaT (- time2 time1))
    (if (equal? time2 time1) (set! deltaT 0.1))				; not possible delaT = 0

;(format t "SLOPE deltaT: %f\n" deltaT)

  (set! deltaF (- freq2 freq1))
  (set! steilheid (/ deltaF deltaT))
  (set! sign 1.0)
  (if (< steilheid 0) (begin (set! sign  -1.0) (set! steilheid (* sign  steilheid))))

  
 (if (> steilheid maxsteil) 
    (set! NFzero (- freq2 (* sign (* maxsteil deltaT))))
    (set! NFzero freq1))           					 ;default first F0-value=freq1

; (format t "SLOPE: steilheid %.1f maxsteil %.1f deltaT %.1f deltaF %d -> %d\n" steilheid maxsteil deltaT deltaF NFzero)

  (nint NFzero)								; return NFzero
));end define SLOPE


(define (OLAPF time1 freq1 time2 freq2 time3 freq3)
(let (NFzero)

; Indien 1 frequentie gelijk nul en twee frequenties ongelijk aan nul:
;   pas de laagste F0 waarde aan
; indien drie frequenties ongelijk nul: pas de middelste F0=freq2 aan
; Indien een time/freq nul dan time3/freq3
; time1 < time2, corrigeer altijd laagste F0 waarde


; (format t "   INPUT OLAP-F0: t1:%.1f f1:%.1f t2:%.1f f2:%.1f t3:%.1f f3:%.1f\n" time1 freq1 time2 freq2 time3 freq3)


  (if (or (equal? freq1 0) (equal? freq2 0) (equal? freq3 0))   		; bepaal hoogste en laagste F0;
   (begin
     (if (equal? freq3 0) 
       (if (> freq2 freq1) (begin (set! Fhigh freq2)
                                  (set! Flow freq1))
                           (begin (set! Fhigh freq1)
                                  (set! Flow freq2))))
     (if (equal? freq2 0)
       (if (> freq3 freq1) (begin (set! Fhigh freq3)
                                  (set! Flow freq1))
                           (begin (set! Fhigh freq1)
                                  (set! Flow freq3))))
     (if (equal? freq1 0)
       (if (> freq3 freq2) (begin (set! Fhigh freq3)
                                  (set! Flow freq2))
                           (begin (set! Fhigh freq2)
                                  (set! Flow freq3))))


      (set! NFzero (SLOPE time1 Flow time2 Fhigh))		; Change the lowest value F0
;  (format t "OLAP-F0 minimal one freq is zero: %d\n" NFzero)
    ))

  (if (and (not (equal? freq1 0))
           (not (equal? freq2 0))
           (not (equal? freq3 0)))		; change the middle=freq2 value
    (begin 
      (set! Ftmp (SLOPE time1 freq2 time2 freq1))		; always the first argument is changed
      (set! NFzero (SLOPE time2 Ftmp time3 freq3))		; so de middle F0=freq2 on the first place
    ))

  (if (equal? NFzero 0)						; default lowest LADD-value, NEVER return zero
    (set! NFzero (target-scaling -1 1 1 1 1)))

;(format t "OLAP-F0 end: %d\n" NFzero)
(nint NFzero)							; return new Fzero

));end define OLAPF


;;; Interface functions

(define (H-target-scaling n m Sa Sp)
"
\(H-target-scaling n m Sa Sp\)

Return F0 value in Hz for the H* target of the n-th accent in the m-th
phrase.  The value of Sa determines if accentual downstep is applied
\(1\) or not \(0\).  The value of Sp determines if phrasal downstep is
applied \(1\) or not \(0\).
"
  (target-scaling 1 n m Sa Sp))


(define (L-target-scaling n m Sa Sp)
"
\(L-target-scaling n m Sa Sp\)

Return F0 value in Hz for the L* target of the n-th accent in the m-th
phrase The value of Sa determines if accentual downstep is applied
\(1\) or not \(0\).  The value of Sp determines if phrasal downstep is
applied \(1\) or not \(0\).
"
  (target-scaling -1 n m Sa Sp))


;;; main scaling function

(define (target-scaling T n m Sa Sp)
"
\(target-scaling T m n Sa Sp\)

Return F0 value in Hz for target T of the n-th accent in the m-th
phrase, where the value of T indicates an H* \(1\) or a L* \(-1\)
target.  The value of Sa determines if accentual downstep is applied
\(1\) or not \(0\).  The value of Sp determines if phrasal downstep is
applied \(1\) or not \(0\).
"
  (set! Fzero (* (Parameter.get 'Fr)
                  (pow (Parameter.get 'N)
	              (* (phrasal-scaling m Sp)
	                  (accentual-scaling T n Sa)))))
  (if (< Fzero 30) (set! Fzero 30))
(nint Fzero))


(define (phrasal-scaling m Sp)
  (pow (Parameter.get 'dp)
       (* Sp 
	  (- m 1))))


(define (accentual-scaling T n Sa)
  (* (pow (Parameter.get 'W) T)
     (pow (Parameter.get 'da)
	  (* 0.5
	     Sa 
	     (+ 1 T)
	     (- n 1)))))


;;; info functions for debugging

(define (print-scaling-parameters)
"
\(print-scaling-parameters\)

Print the parameters of the F0 target scaling model.
"
  (format t "Fr (reference frequency)          :  %.2f\n"  (Parameter.get 'Fr))
  (format t "N  (range)                        :  %.2f\n" (Parameter.get 'N))
  (format t "W  (register width)               :  %.2f\n" (Parameter.get 'W))
  (format t "da (accentual downstep factor)    :  %.2f\n" (Parameter.get 'da))
  (format t "dp (phrasal downstep factor)      :  %.2f\n" (Parameter.get 'dp))
  (format t "StarPos (StarTime relative vowel) :  %.2f\n" (Parameter.get 'StarPos))
  (format t "Speaking rate                     :  %.2f\n" (Parameter.get 'SpeakRate))
  )


(define (print-scaling-tables)
"
\(print-scaling-tables\)

Print tables with F0 targets for H* and L* tones
of the n-th accent in the m-th phrase.
" 
  (format t "Scaling table for H*:\n\n")
  (print-scaling-table 1)
  (format t "\n")
  (format t "Scaling table for L*:\n\n")
  (print-scaling-table -1))


(define (print-scaling-table T)
  (let ((accent_nr 1)
	(phrase_nr 1))
    (format t "    ")
    (while (< accent_nr 16)
	   (format t "%6s" (format nil "A%d:" accent_nr))
	   (set! accent_nr (+ accent_nr 1)))
    (format t "\n")
    (while (< phrase_nr 10)
	   (set! accent_nr 1)
	   (format t "P%d: " phrase_nr)
	   (while (< accent_nr 16)
		  (format t "%6.0f" 
			  (target-scaling T phrase_nr accent_nr 1 1))
		  (set! accent_nr (+ accent_nr 1)))
	   (format t "\n")
	   (set! phrase_nr (+ phrase_nr 1)))))



(provide 'net_nl_int_todi)

	   