;;; $Id: cygwin.scm,v 1.2 2002/10/18 13:13:30 emarsi Exp $

;;; some support functions that are only required on cygwin


;;;------------------------------------------------------------
;;; Temp filename creation
;;;------------------------------------------------------------

(define (get_win_tmp_dir_on_cygwin)
"
\(get_win_tmp_dir_on_cygwin\)

Return the Windows directory for temporary storage of files, by
reading the environment variable TEMP or TMP in the Cygwin environment
\(e.g. /cygwin/c/temp\) and mapping it to the corresponding Windows
path \(that is, c:\\temp\).
"
  (let ((path (or 
	       (getenv "TEMP")
	       (getenv "TMP")))
	drive)
    (set! path (string-after path "/cygdrive/"))
    (set! drive (string-before path "/"))
    (set! path (string-after path "/"))
    (string-append 
     drive
     ":\\"
     (forward-to-backward-slash path))))


(define (forward-to-backward-slash path)
"
\(forward-to-backward-slash PATH\)

Unix to Windows path conversion, which translates all forward to
backward slashes in PATH.
"
(while (string-matches path ".*/.*")
       (set! path (string-append
		   (string-before path "/")
		   "\\"
		   (string-after path "/"))))
path)


(define (escape-backward-slash path)
"
\(escape-backward-slash PATH\)

"
(let ((s ""))
  (mapcar 
   (lambda (c)
     (set! s (string-append s 
			    (if (string-equal c "\\")
				"\\\\"
				(format nil "%s" c)))))
   (symbolexplode path))
  s))




(define (CygwinMakeTmpFilename)
" 
\(CygwinMakeTmpFilename\)

Cygwin-specific version of make_tmp_filename.
"
  (string-append tmp_dir_on_cygwin 
		 "\\"
		 (path-basename (original_make_tmp_filename))))


(define (RestoreMakeTmpFilename)
" 
\(RestoreMakeTmpFilename\)

Restore the orginal version of make_tmp_filename instead of the
Cywin-specific version.
"
  (set! make_tmp_filename original_make_tmp_filename))



;;;------------------------------------------------------------
;;; All Cygwin-specific actions
;;;------------------------------------------------------------

(if (string-matches *ostype* ".*CYGWIN.*")
    (begin
      ;; If we are on cygwin, the filename returned by the function
      ;; make_tmp_filename is a unix path, which cannot be accessed by
      ;; a program running under Windows (Amazon, Praat, etc.) We
      ;; have to do some ugly stuff...  We read the value of the
      ;; environment var "TEMP", which is something like
      ;; "/cygwin/c/TEMP", and translate it to the corresponding
      ;; windows path, i.e. "c:\\TEMP"
      (set! tmp_dir_on_cygwin (get_win_tmp_dir_on_cygwin))
      ;; Next, we backup the original make_tmp_filename function.
      ;; Check that we do this only the first time.
      (if (not (symbol-bound? 'original_make_tmp_filename))
	  (set! original_make_tmp_filename make_tmp_filename))
      ;; and replace it by a cygwin specific version
      (set! make_tmp_filename CygwinMakeTmpFilename)
      ;; In the unlikely event that you switch platform without
      ;; restarting Festival, call RestoreMakeTmpFilename ;-)
  ))


(provide 'cygwin)