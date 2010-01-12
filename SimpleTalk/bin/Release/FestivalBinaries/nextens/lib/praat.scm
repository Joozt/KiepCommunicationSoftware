;;; $Id: praat.scm,v 1.1 2003/03/12 13:25:12 emarsi Exp $

;;; interface between Festival and Praat


;;; cygwin requires a special version of make_tmp_filename
(require 'cygwin)


(define (utt.praat utt)
"
\(utt.praat UTT\)

Draw UTT's signal, segment labels, and pitch contour in Praat.
"
  (let ((filename (make_tmp_filename))
	basename
	wav_fn
	text_fn
	pitch_fn)
    (set! basename (path-basename filename))
    (set! wav_fn (string-append filename ".Wav"))
    (set! text_fn (string-append filename ".TextGrid"))
    (set! pitch_fn (string-append filename ".PitchTier"))
    (utt.save.wave utt wav_fn 'wav) 
    (utt.save.praat.texttier utt text_fn)
    (utt.save.praat.pitchtier utt pitch_fn)
    ;; needs to be done asynchronously, 
    ;; otherwise files are deleted before Praat has read them
    (Sendpraat "praat" 10
     (string-append
      (format nil "Read from file... %s\n" wav_fn)
      (format nil "Read from file... %s\n" pitch_fn)  
      (format nil "Read from file... %s\n" text_fn)
      (format nil "select Sound %s\n" basename)
      "To Manipulation... 0.01 75 300\n"  
      (format nil "select PitchTier %s\n" basename)
      (format nil "plus Manipulation %s\n" basename)
      "Replace pitch tier\n"  
      (format nil "select Manipulation %s\n" basename)
      "Edit\n"   
      (format nil "select Sound %s\n" basename)
      (format nil "plus TextGrid %s\n" basename)
      "Edit\n"))
    (delete-file wav_fn)
    (delete-file text_fn)
    (delete-file pitch_fn)
    ))
  

(define (utt.save.praat.pitchtier utt filename)
  (let ((fd (fopen filename "w"))
	(i 1))
    (format fd "File type = \"ooTextFile\"\n")
    (format fd "Object class = \"PitchTier\"\n\n")
    (format fd "xmin = 0\n")
    (format fd "xmax = %f\n" 
	    (item.feat (utt.relation.last utt 'Segment) 'end))
    (format fd "points: size = %d\n"
	    (length (utt.relation.leafs utt 'Target)))
    (mapcar
     (lambda (target)
       (format fd "points [%d]:\n" i)
       (format fd "    time = %f\n" 
	       (item.feat target 'pos))
       (format fd "    value = %f\n" 
	       (item.feat target 'f0))
       (set! i (+ i 1)))
     (utt.relation.leafs utt 'Target))
  (fclose fd)))


(define (utt.save.praat.texttier utt filename)
  (let ((fd (fopen filename "w"))
	(x 0)
	(xmax 
	 (item.feat (utt.relation.last utt 'Segment) 'end))
	(i 1))
    (format fd "File type = \"ooTextFile\"\n")
    (format fd "Object class = \"TextGrid\"\n\n")
    (format fd "xmin = 0\n")
    (format fd "xmax = %f\n" xmax)
    (format fd "tiers? <exists>\n")
    (format fd "size = 1\n")
    (format fd "item []:\n")
    (format fd "    item [1]\n")
    (format fd "        class = \"IntervalTier\"\n")
    (format fd "        name = \"Segments\"\n")
    (format fd "        xmin = 0\n")
    (format fd "        xmax = %f\n" xmax)
    (format fd "        intervals: size = %d\n"
	    (length (utt.relation.items utt 'Segment)))
    (mapcar 
     (lambda (item)
       (format fd "        intervals [%d]:\n" i)
       (format fd "            xmin = %f\n" x)
       (set! x (item.feat item 'end))
       (format fd "            xmax = %f\n" x)
       (format fd "            text = \"%s\"\n" 
	       (item.feat item 'name))
       (set! i (+ i 1)))
     (utt.relation.items utt 'Segment))
    (fclose fd)))


;; the old function

(define (sendpraat_indirect messages)
"
\(sendpraat_indirect MESSAGES\)

Send MESSAGES to Praat using the separately running Sendpraat program,
where MESSAGES is a list of Praat command strings.
Use this as an alternative if sending messages directly by
means of the function Sendpraat fails.  
"
  (let ((command "sendpraat 10 praat "))
    (mapcar
     (lambda (message)
       (set! command
	     (string-append command
			    (format nil " \"%s\"" message))))
     messages)
    (set! command (string-append command "\n"))
    ;(format t command)
    (system command)
    ))


(provide 'praat)