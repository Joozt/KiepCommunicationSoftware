



(define (bakeliet1)
  (set! u1 (SayWords '(
                       (Alex ((ibnd %L)(acc H*L))) 
                       de
                       (Kock ((acc H*L)))
                       heeft
                       (bijzondere ((acc !H*)))
                       (plannen ((acc !H*L)(fbnd L%))))))
)                       
(define (bakeliet2)
  (set! u2 (SayWords '((voor ((ibnd %L)))
                       de
                       (voormalige ((acc H*L)))
                       (pastorie ((acc !H*L)))
                       in
                       (Hank ((acc !H*L)(fbnd L%))))))
)                       
(define (bakeliet3)

  (set! u3 (SayWords '((hij ((ibnd %L)))
                       hoopt
                       daar
                       in
                       (mei ((acc L*H)(fbnd H%))))))

)                       
(define (bakeliet4)

  (set! u4 (SayWords '((een ((ibnd %L)))
                       (bakeliet ((acc H*)))
                       en
                       (plastic ((acc H*L)))
                       museum
                       te
                       (openen ((fbnd L%))))))

)                       
(define (bakeliet5)

  (set! u5 (SayWords '((de ((ibnd %L)))
                       Kock
                       heeft
                       al
                       een
                       (bijzondere ((acc H*L)))
                       (collectie ((acc !H*L)))
                       (verzameld ((fbnd L%))))))

)                       
(define (bakeliet6)

  (set! u6 (SayWords '((van ((ibnd %L)))
                       onder
                       meer
                       (oude ((acc H*L)))
                       (radioos ((acc L*H)(fbnd H%))))))
)                       
(define (bakeliet7)


  (set! u7 (SayWords '((serviezen ((ibnd %L)(acc L*H)(fbnd H%))))))
)                       
(define (bakeliet8)


  (set! u8 (SayWords '((asbakken ((ibnd %L)(acc H*L)))
                       (stoelen ((acc L*H)(fbnd H%))))))

)                       
(define (bakeliet9)

  (set! u9 (SayWords '((typemachines ((ibnd %L)(acc H*L)))
                       tot
                       (stekkers ((acc !H*L)(fbnd L%))))))
)


(define (TestToDI1)
  (set! utt1 (SayWords '((deze ((ibnd %L))) (kleine ((acc H*L))) (tak ((acc !H*L))) breekt (af ((fbnd L%))) ) ))
;  (utt.praat utt1)
)

(define (TestToDI2)
  (set! utt2 (SayWords '((de ((ibnd %L))) kinderen (fietsen ((fbnd L%)(acc H*L))) ) ))
;  (utt.praat utt2)
)

(define (TestToDI3)
  (set! utt3 (SayWords '((de ((ibnd %L))) (kinderen ((acc H*L))) gaan met de (bus ((acc H*L)(fbnd L%))) ) ))
;  (utt.praat utt3)
)  
(define (TestToDI4)
  (set! utt4 (SayWords '((de ((ibnd %L)))   (kat ((acc L*H))) (rent ((fbnd H%))) ) ))
;  (utt.praat utt4)
)
(define (TestToDI5)
  (set! utt5 (SayWords '((  begin ((ibnd %L)(acc H*L))) te (lopen ((acc H*L)(fbnd H%)) )) ))
;  (utt.praat utt5)

)

(define (TestToDI6)
  (set! utt6 (SayWords '(( kat ((ibnd %H)(acc H*L)(fbnd L%)))) ))
;  (utt.praat utt5)
)

(define (TestToDI7)
  (set! utt7 (SayWords '((de ((ibnd %L))) (grote ((acc H*))) (jongen ((acc !H*L)(fbnd L%))) ) ))
;  (utt.praat utt1)
)
