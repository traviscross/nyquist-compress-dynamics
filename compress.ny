;nyquist plug-in
;version 1
;type process
;name "Compress dynamics..."
;action "Compressing..."
;info "Does dynamic (volume) compression with lookahead.\n'Compress ratio' is how much to louden the soft parts.\nYou can soften the soft parts with values < 0, and invert\nloudness with values > 1 (lower max amp when you do).\n'Attack speed' is how fast the volume drops to anticipate a loud section.\n'Release speed' is how fast it rises after a loud section.\nLower values give slower changes in volume.\nLower 'maximum amplitude' if you experience clipping.\nRaise 'floor' to make quiet parts stay quiet."
;control compress-ratio "Compress ratio" real "" .75 -1 2
;control left-falloff-db-s "Attack speed" real "dB/s" 3 .01 20
;control right-falloff-db-s "Release speed" real "dB/s" 3 .01 20
;control floor "Floor" real "linear" .02 0.0 1.0
;control scale-max "Maximum amplitude" real "linear" .95 .0 1.0

; umm, this isn't ready for prime-time
; control use-percep "Use perceptual model" int "yes/no" 0 0 1
(setf use-percep 0)

;;Version 1.1

;;Authored by Chris Capel (http://pdf23ds.net)
;;All rights reserved
;;Permission granted for personal use, without redistribution.

;;this algorithm works by finding all peaks and putting "tops" on them, which are
;;lines going each direction that have a slope determined by falloffs. These lines
;;are constructed so that they never cross a sample value. Together, these tops
;;form an envelope whose inverse can be applied with pwe and mult to bring all the
;;tops up to .95.

;;Compressing based on perceived loudness--perceptual model

;;would use rms instead of absolute peak, on the theory that it would more closely
;;follow perceived loudness, except that the theory is wrong. absolute peak actually
;;seems to *more* closely track perceived loudness, though it's not perfect. (I think
;;the main shortcoming is that it doesn't know about the response curve of the human
;;ear, where middle-range sounds seem louder than very high or low ones.) Perhaps to
;;fix it would require applying equalization that makes the computer "hear" what humans
;;do as far as frequency response, so that the peak values (or maybe RMS then) would
;;then nearly perfectly track perceptual loudness. Frequency response actually varies
;;considerably between different people, and volume levels and playback systems, and
;;is especially affected by age, but it might still be possible to improve on an
;;unequalized absolute peak, so that brass don't sound louder than strings at the same
;;amplitude.

;;http://personal.cityu.edu.hk/~bsapplec/frequenc.htm
;;20 hz -40 db
;;30 hz -30 db
;;50 hz -20
;;80 hz -10
;;120 hz -5
;;200 hz 0
;;300 hz +4
;;450 hz +5
;;600 hz +4
;;800 hz +2
;;1300 hz 0
;;2000 hz +3
;;3000 hz +7
;;4000 hz +9
;;6000 hz +1
;;8500 hz -7
;;12000 hz 0
;;14000 hz +4
;;16000 hz -3
;;20000 hz -30

;;this EQ setting can probably be improved on a lot

;;20 -40
;;40 -23
;;80 -10
;;160 -4
;;320 +3
;;640 +2
;;1280 0
;;2560 +4
;;5120 +9
;;10240 -7
;;20480 -20

(defun my-snd-fetch (snds)
  (if (arrayp snds)
      (let ((val1 (snd-fetch (aref snds 0)))
	    (val2 (snd-fetch (aref snds 1))))
	(when (not (null val1))
	  (linear-to-db (max (abs val1) (abs val2)))))
      (let ((val (snd-fetch snds)))
	(when (not (null val))
	  (linear-to-db (abs val))))))

(defun my-snd-srate (snds)
  (if (arrayp snds)
      (snd-srate (aref snds 0))
      (snd-srate snds)))

(defun get-my-sound (sound)
  (snd-avg (if (= use-percep 1) (get-percep-adjusted-sound sound) sound) 2000 1000 op-peak))

(defun get-percep-adjusted-sound (sound)
  (let ((bands '((20 -40)
		 (40 -23)
		 (80 -10)
		 (160 -4)
		 (320 +3)
		 (640 +2)
		 (1280 0)
		 (2560 +4)
		 (5120 +9)
		 (10240 -7)
		 (20480 -20))))
    (dolist (band bands)
      (setf sound (eq-band sound (car band) (cadr band) 1)))
    sound))

(setf max-gain (if (= floor 0) 0 (/ 1.0 floor)))
(defun limit-amp (samp)
  (if (and (> floor 0) (> samp max-gain))
      max-gain
      samp))

(defun adjust-samp (samp)
  ;;convert an envelope value into a gain value
  (expt (limit-amp (/ scale-max (db-to-linear samp))) compress-ratio))

(let* ((snds (if (arrayp s)
		 (let ((v (make-array 2)))
		   (dotimes (i 2)
		     (setf (aref v i) (get-my-sound (aref s i))))
		   v)
		 (get-my-sound s)))
       (right-falloff (/ right-falloff-db-s (my-snd-srate snds))) ;;(db/sec)/(sample/sec)->db/sample
       (left-falloff (/ left-falloff-db-s (my-snd-srate snds)))
       (tops nil)
       (pwe-args '(1.0 1.0 1.0)) ;;initialize it with the final time
       (max-sample nil))
  (push (list (linear-to-db 0.0) -1) tops) ;; (db index)
  ;;first find all the tops we need.
  ;;we pick up a bunch of extra ones going up the left side of waveform hills
  (do* ((cur-value (my-snd-fetch snds) (my-snd-fetch snds))
	(i 0 (1+ i))
	(top (car tops) (car tops)))
       ((null cur-value) (setf max-sample (float i)))
    ;;if the sample is above the line of the current top
    (when (> cur-value
	     (- (car top) (* right-falloff (- i (cadr top)))))
      (push (list cur-value i) tops)))
  ;;remove all the extra tops
  (do ((cur-tops tops (cdr cur-tops)))
      ((null cur-tops))
    (do ((top (car cur-tops))
	 (prev-top (cadr cur-tops) (cadr cur-tops)))
	;;exit if the previous top is not contained within the current one
        ((or (not prev-top)
	     (let ((prev (car prev-top))
		   (cur (- (car top) (* left-falloff
					(- (cadr top) (cadr prev-top))))))
	       (> prev cur))))
      ;;otherwise remove it and repeat
      (setf (cdr cur-tops) (cddr cur-tops))))
  ;;ok, we have all our tops now.
  ;;first set the ending scale
  (push (adjust-samp
	 (- (car (car tops))
	    (* right-falloff
	       (- max-sample (cadr (car tops))))))
	pwe-args)
  (push 1.0 pwe-args)
  ;;now all the in between ones
  (do* ((cur-tops tops (cdr cur-tops))
	(right-top (car cur-tops) (car cur-tops))
	(left-top (cadr cur-tops) (cadr cur-tops)))
       ((null right-top))
    ;;we construct pwe-args in reverse, so first value, then time
    (push (adjust-samp (car right-top))
	  pwe-args)
    ;;convert sample number to 0-1 value
    (push (/ (cadr right-top) max-sample) pwe-args)
    (if left-top
      ;;the intersection of two lines (the two tops), given the slope (a) and a point on both lines
      ;;calculate offset (b)
      ;;b_1,2 = y - ax
      ;;x_i = (b_1 - b_2) / (a_2 - a_1)
      ;;y_i = a_2 x_i + b_2
      ;;note that right-falloff is the slope for the left top, and vice versa
      (let* ((b-left (+ (car left-top) (* right-falloff (cadr left-top))))
	     (b-right (- (car right-top) (* left-falloff (cadr right-top))))
	     (x_i (/ (- b-left b-right) (+ right-falloff left-falloff)))
	     (y_i (+ (* left-falloff x_i) b-right)))
	(push (adjust-samp y_i) pwe-args)
	(push (/ x_i max-sample) pwe-args))
      ;;else
      (progn
	;;this is the starting scale
	(push (adjust-samp
	       (- (car right-top)
		  (* left-falloff (cadr right-top))))
	      pwe-args)
	(push 0.0 pwe-args))))
  (mult s (pwe-list pwe-args)))
