;;
;; MIX
;;

(require 'cl)

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
(defun mix-word-a  (w) (+ (* 64 (mix-word-b1 w)) (mix-word-b2 w)))
(defun mix-word-i  (w) (mix-word-b3 w))
(defun mix-word-f  (w) (mix-word-b4 w))
(defun mix-word-c  (w) (mix-word-b5 w))

(defun mix-wordf-reg (w)
  (format "%s%02d.%02d.%02d.%02d.%02d"
          (cond ((>= w 0) "+") (t "-"))
          (mix-word-b1 w)
          (mix-word-b2 w)
          (mix-word-b3 w)
          (mix-word-b4 w)
          (mix-word-b5 w)))
(defun mix-wordf-dec (w) (format "%10d" w))

(defun mix-wordf (w) (mix-wordf-reg w))

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
(defvar mix-hlt 0) ;; is halted? not in Knuth
(defvar mix-M (make-vector 4000 0))

(defun mix-reset ()
  (setq mix-rA 0)
  (setq mix-rX 0)
  (setq mix-rI1 0)
  (setq mix-rI2 0)
  (setq mix-rI3 0)
  (setq mix-rI4 0)
  (setq mix-rI5 0)
  (setq mix-rI6 0)
  (setq mix-rJ 0)
  (setq mix-cmp 0)
  (setq mix-overflow 0)
  (setq mix-rIP 0)
  (setq mix-hlt 0)
  (setq mix-M (make-vector 4000 0)))

(defun mix-ld (i) "load" (elt mix-M i))
(defun mix-st (i x) "store" (aset mix-M i x))

;; PRINTING

(defvar mix-buffer-name "*MIX*")

(defun mix-buffer () (get-buffer-create mix-buffer-name))

(defun printf (fmt &rest rest)
  (insert (apply 'format (cons fmt rest))))

(defun memf (i)
  (if (= i mix-rIP)
      (format "> M[%2d]: %s" i (mix-wordf (mix-ld i)))
      (format "  M[%2d]: %s" i (mix-wordf (mix-ld i)))))

(defun render ()
  "Render MIX state"
  (with-current-buffer (mix-buffer)
    (read-only-mode 0)
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
            (cond ((= mix-cmp 0) "E") ((> mix-cmp 0) "G") ((< mix-cmp 0) "L"))
            (cond ((= mix-overflow 0) "0") (t "OVERFLOW")))
    (printf "-------------------------------------------------------------------\n")
    (loop for i from 0 to 10 do
          (printf "%s %s %s %s\n"
                  (memf (+ i 0))
                  (memf (+ i 10))
                  (memf (+ i 20))
                  (memf (+ i 30))
                  (memf (+ i 40))))))

;;
;; Instructions
;;

;; Enter data into a register
(defun ENTA (x) (setq mix-rA x))
(defun ENTX (x) (setq mix-rX x))

;; Load memory contents to registers
(defun LDA (i) (setq mix-rA (mix-ld i)))
(defun LDX (i) (setq mix-rX (mix-ld i)))

(defun STA (i) (mix-st i mix-rA))
(defun STX (i) (mix-st i mix-rX))

(defun ADD (x) (setq mix-rA (+ mix-rA x)))
(defun SUB (x) (setq mix-rA (- mix-rA x)))
(defun MUL (x) (setq mix-rA (* mix-rA x)))
(defun DIV (x) (setq mix-rA (/ mix-rA x)))

(defun mix-cmp (a b)
  ;; (message "CMP %d %d" a b)
  (cond ((> a b) (setq mix-cmp +1))
        ((= a b) (setq mix-cmp  0))
        ((< a b) (setq mix-cmp -1))))

(defun CMPA (i) (mix-cmp mix-rA (mix-ld i)))
(defun CMPX (i) (mix-cmp mix-rX (mix-ld i)))

(defun JMP (i) (setq mix-rIP i))
(defun NXT ()  (setq mix-rIP (+ 1 mix-rIP)))
(defun JL  (i) (if (= -1 mix-cmp) (setq mix-rIP i) (NXT)))
(defun JE  (i) (if (= 0  mix-cmp) (setq mix-rIP i) (NXT)))
(defun JG  (i) (if (= +1 mix-cmp) (setq mix-rIP i) (NXT)))

(defun HLT ()  (setq mix-hlt 1))

(mix-reset)

(defun PRT (i)
  "Print 0 terminated string to *MESSAGES*"
  (let ((buf ()))
    (while (/= 0 (mix-ld i))
      (push (mix-ld i) buf)
      (setq i (+ 1 i)))
    (message (apply 'string (seq-reverse buf)))))

(defun PRTN (i)
  "Print number at memory position i"
  (message (format "%d" (mix-ld i))))

(defun test-prt ()
  (mix-st 20 ?H)
  (mix-st 21 ?e)
  (mix-st 22 ?l)
  (mix-st 23 ?l)
  (mix-st 24 ?o)
  (mix-st 25 ?!)
  (PRT 20))

;;
;; Instruction decoding
;;
(defun mix-ins-ld () (mix-ld mix-rIP))
(defun mix-ins-op () (mix-word-c (mix-ins-ld)))
(defun mix-ins-M  () (mix-word-a (mix-ins-ld)))
(defun mix-ins-F  () (mix-word-f (mix-ins-ld)))
(defun mix-ins-nxt () (setq mix-rIP (+ 1 mix-rIP)))

(defun mix-step ()
  (let ((op (mix-ins-op))
        (M  (mix-ins-M))
        (F  (mix-ins-F)))
    ;; (message (format "EXECUTING INSTRUCTION AT M[%d] %d : %d,%d" mix-rIP op M F))
    (cond ((= op 8)  (LDA M) (mix-ins-nxt))
          ((= op 9)  (STA M) (mix-ins-nxt))
          ((= op 15) (LDX M) (mix-ins-nxt))
          ((= op 16) (STX M) (mix-ins-nxt))
          ((= op 5)  (HLT))
          ((= op 1)  (ADD M) (mix-ins-nxt))
          ((= op 2)  (SUB M) (mix-ins-nxt))
          ((= op 3)  (MUL M) (mix-ins-nxt))
          ((= op 4)  (DIV M) (mix-ins-nxt))
          ((= op 56) (CMPA M) (mix-ins-nxt))
          ((= op 39) (cond ((= F 1) (JMP M))
                           ((= F 4) (JL M))
                           ((= F 5) (JE M))
                           ((= F 6) (JG M))
                           (t       (error "Invalid field"))))
          ((= op 60) (PRT M) (mix-ins-nxt))
          ((= op 61) (PRTN M) (mix-ins-nxt))
          (t         (message (format "NOT FOUND") (HLT))))))

(defun mix-run ()
  (while (= mix-hlt 0)
    (mix-step)
    (render)
    ;; (read-event)
    (sit-for .1)
    ))

;;
;; Mix Assembly
;;
(setq *MIXOP* (list 
               :LDA 8 :LDX 15
               :STA 9 :STX 16 ;; ??
               :ADD 1 :SUB 2 :MUL 3 :DIV 4
               :CMPA 56 :JMP 39
               :PRT 60 :PRTN 61
               :HLT 5
               ))
(defun mix-asm-opcode (s) (or (getf *MIXOP* s) (error (format "No such operation: %s" s))))

(defun mix-asm-word (s b1 b2 b3 b4 b5)
  (* s (+ b5 (lsh (+ b4 (lsh (+ b3 (lsh (+ b2 (lsh (+ b1) 6)) 6)) 6)) 6))))
(defun mix-asm-set-rA (s b1 b2 b3 b4 b5) (setq mix-rA (mix-asm-word s b1 b2 b3 b4 b5)))
(defun mix-asm-set-M  (i s b1 b2 b3 b4 b5) (mix-st i (mix-asm-word s b1 b2 b3 b4 b5)))
(defun mix-asm-sto-ins (i op &optional A F I)
  (setq A (or A 0))
  (setq I (or I 0))
  (setq F (or F 0))
  (let ((C  (mix-asm-opcode op))
        (As (mix-word-sign A))
        (A1 (mix-word-b4 A))
        (A2 (mix-word-b5 A)))
    (mix-st i (mix-asm-word As A1 A2 I F C))))

(display-buffer (mix-buffer))

(mix-reset)
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
(JMP 30)
(mix-st 20 ?R)
(mix-st 21 ?E)
(mix-st 22 ?S)
(mix-st 23 ?:)
(mix-st 24 ? )
(mix-st 25 0)
(mix-st 0 3)
(mix-st 1 100)
(mix-asm-sto-ins 30 :LDA 0) ; rA <- M[0]
(mix-asm-sto-ins 31 :ADD 1) ; rA += 1
(mix-asm-sto-ins 32 :MUL 3) ; rA *= 3
(mix-asm-sto-ins 33 :CMPA 1) ; rA < M[1] ?
(mix-asm-sto-ins 34 :JMP  31 4) ; -> JMP 31
(mix-asm-sto-ins 35 :STA 2) ; store result in M[2]
(mix-asm-sto-ins 36 :PRT 20) ; print RES:
(mix-asm-sto-ins 37 :PRTN 2)
(mix-asm-sto-ins 38 :HLT)
(render)
(mix-run)
