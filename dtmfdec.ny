;nyquist plug-in
;version 3
;type analyze
;name "DTMF Decoder"
;action "Analyzing..."
;info "by Robert J. Haenggi\nReleased under GPL v2.\n"
;;
;; dtmfdec.ny
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;; Plug-in version 1.0, May 2014
;; Requires Audacity 2.0  or later, developed under Audacity 2.0.6 Alpha
;;
;control debugout "Additional information for the Debug Screen" choice "None,Words only,Words/Numbers" 0 
;control tolerance "Tolerance in Percent" real "" 3.0 0.5 10.0
;control blk "Blocksize in Samples" int "" 1075 200 3000
;control hop "Hopsize  in Samples" int "" 200 1 3000
;;
;; DTMF decoding
;;
;; Low frequency part (association list)
(setf f1-alist '((0 . (#\space ))
  (697 . (#\1 #\2 #\3 
     #\A #\a #\b #\c #\d #\e #\f))
  (770 . (#\4 #\5 #\6
     #\B #\g #\h #\i #\j #\k #\l #\m #\n #\o))
  (852 . (#\7 #\8 #\9
     #\C #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y   #\z))
  (941 . (#\* #\0 #\# #\D))))  
;
;; High frequency part
(setf f2-alist '((0 . (#\space ))
  (1209 . (#\1 #\4 #\7
     #\* #\g #\h #\i #\p #\q #\r #\s)) 
  (1336 . (#\2 #\5 #\8 #\0
     #\a #\b #\c #\j #\k #\l #\t #\u #\v))
  (1477 . (#\3 #\6 #\9
     #\# #\d #\e #\f #\m #\n #\o #\w #\x #\y #\z))
  (1633 . (#\A #\B #\C #\D))))
;;
(setf choices '())
;;
;; Characters that we do not pass to the debug screen
;; Only letters and 1, 0, *, # 
(setf exceptions 
  '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 ))    
;;
;; Let's not be too picky about the frequency
;; 1.9 % tolerance is standard
;; but the frequency estimator might need more
(defun approx (freq comp)
  (<= (abs (- comp freq)) (* 0.01 tolerance comp)))
;;
;; Returns the list of characters for given two freqs 
(defun pick-combination (f1 f2)
  (reverse (intersection 
     (cdr (assoc f1 f1-alist :test 'approx))
     (cdr (assoc f2 f2-alist :test 'approx)))))
;;
;; Filter the estimated frequency
;; Remove outlayers (middle of 3)
(defun median (sig)
  (multichan-expand #'(lambda (v0)
     (let* ((sr (snd-srate v0))
            (rsr (/ sr)) 
            (dur (1- (snd-length v0  ny:all)))
            (first (sref v0 0))
            (last (snd-sref v0 (* rsr dur)))
            (v+1 (seq (snd-from-array 0 sr (vector first))
                      (extract-abs 0 (* dur rsr) v0))) 
            (v-1 (seq (extract-abs rsr (* rsr (1+ dur)) v0)
                      (snd-from-array 0 sr (vector last)))))
      (sim v-1 v0 v+1 (mult -1 (s-max  v+1 (s-max v-1 v0))) 
           (mult -1 (s-min v+1 (s-min v-1 v0)))))) 
  sig))
;;
;; Count zero crossings for a given block size
;; that's the crudest of all pitch estimators... 
(defun zcr (sig block step)
  (let* (
         (clipped (clip (mult 10000 sig) 1))
         (delta (s-abs (mult (/ 0.5  *sr*) (slope clipped)))))
   (snd-avg delta block step op-average)))
;;
;; Find all possible number/letter combinations
(defun combinations (fn choices &aux (count 0))
  (labels ((recursion (x y)
             (if y (mapc (lambda (i) (recursion (cons i x) (cdr y))) (car y))
                 (progn (if (> count 9999) (return-from combinations  (format t "Only ~a combinations explored" count))) 
                        (incf count) 
                        (funcall fn x)))))
    (recursion nil (reverse choices))) count)
;;
;; The debug screen shows possible words as source
;; A probability order would be nice
(defun print-to-debug (list)
  (setf conditions 
     (mapcar (lambda (x) (not (member x exceptions))) list))
  (when (or (= debugout 2) (eval (cons #'and conditions)))
  (dolist (current list)
        (unless (equal current #\space) (format t "~a" current)))
  (terpri)))
;;;;
;;;; main
(setq *sr* *sound-srate*)
(if (arrayp s) (setf s (sum (aref s 0) (aref s 1))))
;; control-signal, 1 for voiced, 0 for below -40 dB
(setf voiced (snd-oneshot (snd-avg s blk hop op-peak) 0.01 (/ blk *sr*)))
(setf f1 (median (mult (/ *sr* 2) voiced 
   (zcr (lowpass8 s 1051) blk hop))))
(setf f2 (median (mult (/ *sr* 2)voiced 
     (zcr (highpass8 s 1039) blk hop))))
;;
(do* ((low 0 (snd-fetch f1)) (high 0 (snd-fetch f2))
      decoded dec-1 dec-2 (result ""))
     ((or (null low) (null high)) 
      (prog2 (when  (> debugout 0) (format t "~a possible combinations (including numbers)~%" (apply '* (mapcar 'length choices)))  
                   (combinations #'print-to-debug  choices))
             (format nil "~a" result))) 
     (setf decoded (pick-combination low high))
     (when (and decoded (equal dec-1 decoded) 
                 (not (equal decoded dec-2)))
        (setf choices (append choices (list (append (cdr decoded) (list (car decoded))))))  
        (setf result (format nil "~a~a" result (car decoded))))
     (setf dec-2 dec-1)
     (setf dec-1 decoded))