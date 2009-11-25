;;; support for drawing Festival relations with dot


;;; TODO:
;;; - split code in net_nl specific part, and generic code
;;; - find a better solution instead of sleep


(defvar dot_exec "/usr/local/bin/dot"
"path to dot graph drawing program")

(defvar png_viewer "mozilla"
"path to a program capable of displaying png graphics, for instance
Mozilla")

(defvar sleep_time 2
"no. of seconds to wait before the png file is deleted")


(define (ShowSynTreeGraph utt)
  (let ((inp_fn (make_tmp_filename))
	(out_fn (string-append (make_tmp_filename) ".png")))
    (WriteSynTree2Graph utt inp_fn)
    (system
     (format nil "%s -Tpng %s -o %s"
	     dot_exec
	     inp_fn
	     out_fn))
    (system 
     (format nil "%s %s && sleep %d"
	     png_viewer
	     out_fn
	     sleep_time))
    (delete-file inp_fn)
    (delete-file out_fn)))


(define (ShowProsTreeGraph utt)
  (let ((inp_fn (make_tmp_filename))
	(out_fn (string-append (make_tmp_filename) ".png")))
    (WriteProsTree2Graph utt inp_fn)
    (system
     (format nil "%s -Tpng %s -o %s"
	     dot_exec
	     inp_fn
	     out_fn))
    (system 
     (format nil "%s %s && sleep %d"
	     png_viewer
	     out_fn
	     sleep_time))
    (delete-file inp_fn)
    (delete-file out_fn)))
     
  

(define (WriteSynTree2Graph utt fn)
  (let (fd)
    (set! fd (fopen fn "w"))
    (GraphHeader fd)
    (format fd "{rank = same;\n")
    (mapcar 
     (lambda (term)
       (if (item.relation term 'Token) 
	   (format fd "\"%s\"\n; " (item.feat term 'id))))
     (utt.relation.items utt 'SynTree))
    (format fd "};\n")
    (SynTree2Graph utt 'SynTree fd)
    (GraphTrailer fd)
    (fclose fd)))


(define (WriteProsTree2Graph utt fn)
  (let (fd)
    (set! fd (fopen fn "w"))
    (GraphHeader fd)
    (format fd "{rank = same;\n")
    (mapcar 
     (lambda (term)
       (if (item.relation term 'Segment) 
	   (format fd "\"%s\"\n; " (item.feat term 'id))))
     (utt.relation.items utt 'ProsTree))
    (format fd "};\n")
    (ProsTree2Graph utt 'ProsTree fd)
    (GraphTrailer fd)
    (fclose fd)))

 
(define (WriteRelation2Graph utt relation fn)
  (set! fd (fopen fn "w"))
  (GraphHeader fd)
  (Relation2Graph utt relation fd)
  (GraphTrailer fd)
  (fclose fd))


(define (Relation2Graph utt relation stream)
  (mapcar
   (lambda (node)
     (format stream "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     (item.name node))
     (mapcar
      (lambda (dtr)
	(format stream "%s -> %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id))
	)
      (item.daughters node)))
   (utt.relation.items utt relation)))


(define (SynTree2Graph utt relation stream)
  (mapcar
   (lambda (node)
     (format stream "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     ;; escape double quotes in output to dot
	     (cond
	      ((string-equal (item.name node) "\"-")
	       "\\\"-")
	      ((string-equal (item.name node) "-\"")
	       "-\\\"")
	      (t 
	       (item.name node))))
     (mapcar
      (lambda (dtr)
	(format stream "%s -> %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id))
	)
      (item.daughters node)))
   (utt.relation.items utt relation)))


(define (ProsTree2Graph utt relation stream)
  (mapcar
   (lambda (node)
     (format stream "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     (item.name node))
     (mapcar
      (lambda (dtr)
	(format stream "%s -> %s [fontname=Helvetica,fontsize=11%s];\n"
		(item.feat node 'id)
		(item.feat dtr 'id)
		(if (member_string (item.name dtr) '("Prosword1" "ProsWord2" "Foot" 
						     "Appendix" "Syllable"))
		    (if (string-equal (item.feat dtr 'metrical) "strong")
			",label=s"
			",label=w")
		    "")))
      (item.daughters node)))
   (utt.relation.items utt relation)))


(define (GraphHeader stream)
  (format stream "digraph G {\n")
  (format stream "ordering = \"out\";\n")
  (format stream "nodesep = .1;\n")
  (format stream "ranksep = .1;\n")
  (format stream "node [shape=plaintext,fontname=Helvetica,fontsize=11,height=0.1];\n")
  (format stream "edge [dir=none];\n"))


(define (GraphTrailer stream)
  (format stream "}\n"))
  
