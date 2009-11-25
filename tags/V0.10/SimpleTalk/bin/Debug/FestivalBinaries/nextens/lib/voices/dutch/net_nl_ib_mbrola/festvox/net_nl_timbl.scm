;;; $Id: net_nl_timbl.scm,v 1.3 2003/04/01 14:38:04 emarsi Exp $
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


;;; general functions for using Timbl



(define (nl::feature-vector item feature-def)
"
\(nl::Feature-vector ITEM FEATURE-DEF\)

A general function for constructing instances for Timbl. Return a
feature vector for ITEM by appending all value for the features
defined in FEATURE-DEF, into a string. FEATURE-DEF must be a list of
pairs, where the first member specifies the feature, and the second
member specifies the value to use in case the feature's value is
undefined \(that is, when item.feat returns a zero\). The vector is
terminated by a question mark, which \(arbitrarily\) represents its
class.
"
  (let ((vector "")
	val)
    (mapcar
     (lambda (pair)
       ;; feature path is specified in the first member of the pair
       (set! val (item.feat item (car pair)))
       (set! vector
	     (string-append 
	      vector
	      ;; if the feature's value is a zero, then append the
	      ;; value specified in the second member of the pair,
	      ;; otherwise append the value itself
	      (if (equal? val 0)
		  (cadr pair)
		  val)
	      " ")))
     ;; feature-def is list of the form 
     ;; ( (path1 val1) ... (pathn valn)) 
     ;; where path defines a feature and string defines a value to use
     ;; if the feature turns out to be undefined
     feature-def)
    ;; finnaly, append a question mark, represensting the unknown
    ;; class of this instance
    (string-append vector "?")))


(provide 'net_nl_timbl)

  
