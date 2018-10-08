;;
;; MIX
;;

(require 'cl-lib)

;;; Code:

;;
;; Bit Operations
;;
;; MIX-bytes are six bits long with values from 0..64
;; MIX-words consist of 5 MIX-bytes and a sign (5x6+1 = 31bits)
;; We represent negative number as 2 complement with sign bit set

(assert (> most-positive-fixnum (lsh 2 31))
        "Number range not large enoght to hold MIX words")

(defun mix-word-sign (w) (cond ((>= w 0) 1) (t -1)))

;; 63 = 11111b masks lower 5 bits
(defun mix-word-b5 (w) (logand 63 w))
(defun mix-word-b4 (w) (mix-word-b5 (ash w -6)))
(defun mix-word-b3 (w) (mix-word-b5 (ash w -12)))
(defun mix-word-b2 (w) (mix-word-b5 (ash w -18)))
(defun mix-word-b1 (w) (mix-word-b5 (ash w -24)))
(defun mix-word-a  (w) (logand 1024 (ash w -18))) ;; b1-2
(defun mix-word-i  (w) (mix-word-b3 w))
(defun mix-word-f  (w) (mix-word-b2 w))
(defun mix-word-c  (w) (mix-word-b1 w))

(defun mix-wordf (w)
  "Retun string representation of mix-word"
  (format "%s%02d.%02d.%02d.%02d.%02d"
          (cond ((>= w 0) "+") (t "-"))
          (mix-word-b1 w)
          (mix-word-b2 w)
          (mix-word-b3 w)
          (mix-word-b4 w)
          (mix-word-b5 w)))

(mix-wordf (+ 1 (* 64 (+ 2 (* 64 3)))))

(defun test ()
  (assert (= (mix-word-sign -100) -1))
  (assert (= (mix-word-sign 0) 1))
  (assert (= (mix-word-sign 100) 1))
  (assert (= (mix-word-b5   0)  0))
  (assert (= (mix-word-b5   63) 63))
  (assert (= (mix-word-b5   64) 0))
  (assert (= (mix-word-b4   64) 1))
)
(test)

;; MACHINE STATE

(defvar mix-rA 0)
(defvar mix-rX 0)
(defvar mix-rI1 0)
(defvar mix-rI2 0)
(defvar mix-rI3 0)
(defvar mix-rI4 0)
(defvar mix-rI5 0)
(defvar mix-rI6 0)
(defvar mix-rJ 0)
(defvar mix-cmp 0)
(defvar mix-overflow 0)
(defvar mix-rIP 0) ;; instruction pointer, implicit in Knuth

(defvar mix-M (make-vector 4000 0))

(defun mix-ld (i) "store" (elt mix-M i))
(defun mix-st (i x) "load" (aset mix-M i x))

;; PRINTING

(defvar mix-buffer-name "*MIX*")

(defun mix-buffer ()
  (get-buffer-create mix-buffer-name))

(defun printf (fmt &rest rest)
  (insert (apply 'format (cons fmt rest))))


(defun render ()
  "Render MIX state"
  (with-current-buffer (mix-buffer)
    (erase-buffer)
    (printf "#\n# MIX\n#\n")
    (printf "rA:  %s\n" (mix-wordf mix-rA))
    (printf "rX:  %s\n" (mix-wordf mix-rX))
    (printf "rI1: %s\n" (mix-wordf mix-rI1))
    (printf "rI2: %s\n" (mix-wordf mix-rI2))
    (printf "rI3: %s\n" (mix-wordf mix-rI3))
    (printf "rI4: %s\n" (mix-wordf mix-rI4))
    (printf "rI5: %s\n" (mix-wordf mix-rI5))
    (printf "rI6: %s\n" (mix-wordf mix-rI6))
    (printf "rJ : %s\n" (mix-wordf mix-rJ))
    (printf "rIP: %s\n" (mix-wordf mix-rIP))
    (printf "cmp: %s. overflow:%s.\n"
            (cond ((= mix-cmp 0) "0") ((> mix-cmp 0) "+") ((< mix-cmp 0) "-"))
            (cond ((= mix-overflow 0) "0") (t "OVERFLOW")))
    (printf "-------------------------------------------------------------------\n")
    (loop for i from 0 to 10 do
          (printf "M[%2d]: %s M[%d]: %s M[%d]: %s M[%d]: %s\n"
                  (+ i  0) (mix-wordf (mix-ld i))
                  (+ i 10) (mix-wordf (mix-ld (+ i 10)))
                  (+ i 20) (mix-wordf (mix-ld (+ i 20)))
                  (+ i 30) (mix-wordf (mix-ld (+ i 30)))))))


;;
;; Instructions
;;

(defun ENTA (x) (setq mix-rA x))
(defun ENTX (x) (setq mix-rX x))

(defun LDA (i) (setq mix-rA (mix-ld i)))
(defun LDX (i) (setq mix-rX (mix-ld i)))

(defun STA (i) (mix-st i mix-rA))
(defun STX (i) (mix-st i mix-rX))

(defun ADD (x) (setq mix-rA (+ mix-rA x)))
(defun SUM (x) (setq mix-rA (- mix-rA x)))
(defun MUL (x) (setq mix-rA (* mix-rA x)))
(defun DIV (x) (setq mix-rA (/ mix-rA x)))

(defun mix-cmp (a b)
  (cond ((> a b) (setq mix-cmp +1))
        ((= a b) (setq mix-cmp  0))
        ((< a b) (setq mix-cmp -1))))

(defun CMPA (i) (mix-cmp mix-rA (mix-ld i)))
(defun CMPX (i) (mix-cmp mix-rX (mix-ld i)))

(defun JMP (i) (setq mix-rIP i))
(defun JL  (i) (cond ((= mix-cmp -1) (setq mix-rIP i))))
(defun JE  (i) (cond ((= mix-cmp 0)  (setq mix-rIP i))))
(defun JG  (i) (cond ((= mix-cmp +1) (setq mix-rIP i))))


(ENTA 4)
(STA 1)
(LDA 0)
(ADD 123)
(STA 5)
(ADD -1)
(CMPA 5)
(JL 5)
(JE 6)
(JG 7)
(render)
