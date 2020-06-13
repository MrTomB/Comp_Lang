#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.2 2015-09-23 17:11:09-07 - - $
;; Thomas Burch(tburch1@ucsc.edu)
;; tburch1@ucsc.edu
;; 10/12/15
;; CMPS 112-ASG1

;; == Provided functions =======================================
; Define *stderr*
(define *stderr* (current-error-port))

; Function: Find the basename of the filename provided.
(define *run-file*
   (let-values
      (((dirpath basepath root?)
         (split-path (find-system-path 'run-file))))
      (path->string basepath))
)

; Function: Exit and print the error provided.
(define (die list)
   (for-each (lambda (item) (display item *stderr*)) list)
   (newline *stderr*)
   (exit 1)
)

; Function: Print usage information and die.
(define (usage-exit)
   (die `("Usage: " ,*run-file* " filename"))
)

; Function: Read in the file.
(define (readlist-from-inputfile filename)
   (let ((inputfile (open-input-file filename)))
       (if (not (input-port? inputfile))
          (die `(,*run-file* ": " ,filename ": open failed"))
          (let ((program (read inputfile)))
              (close-input-port inputfile)
                   program))))

;; ==== My functions ==========================================
; Define *function-table*
(define *function-table* (make-hash))
(define (function-put! key value)
        (hash-set! *function-table* key value)
)

;Define *label-table*
(define *label-table* (make-hash))

;Define *variable-table*
(define *variable-table* (make-hash))
(define (variable-put! key value)
        (hash-set! *variable-table* key value)
)

;Function: Evaluates hash expressions
(define (hash_eval expr)
  (cond
    ((string? expr) expr)
    ((number? expr) expr)
    ((hash-has-key? *function-table* expr)
      (hash-ref *function-table* expr))
    ((hash-has-key? *variable-table* expr)
      (hash-ref *variable-table* expr))
    ((list? expr)
      (if (hash-has-key? *function-table* (car expr))
        (let((head (hash-ref *function-table*  (car expr))))
          (cond 
            ((procedure? head)
             (apply head (map (lambda (x) (hash_eval x)) (cdr expr))))
            ((vector? head)
             (vector-ref head (cadr expr)))
            ((number? head)
             head)
           (else
              (die "Fatal: Broken symbol table.")
            )
          )
        )
        (die (list "Fatal error: " 
                  (car expr) " not in symbol table!\n"))
      )
    )
  )
)

;Function: Prints called only when print args is present
(define (sb_print expr)
   (map (lambda (x) (display (hash_eval x))) expr)
   (newline)
)

;Function: Declare an array.
(define (sb_dim expr)
  (set! expr (car expr))
  (let((arr (make-vector (hash_eval (cadr expr)) (car expr))))
    (function-put! (car expr) (+ (hash_eval (cadr expr)) 1))
  )
)

;Function: Assign a variable.
(define (sb_let expr)
  (function-put! (car expr) (hash_eval (cadr expr)))
)

;Function: Take input.
(define (rec_input expr count)
  (if (null? expr)
    count
     (let ((input (read)))
        (if (eof-object? input)
          -1
          (begin
            (function-put! (car expr) input)
            (set! count (+ 1 count))
            (rec_input (cdr expr) count)
          )
        )
     )
  )
)
(define (sb_input expr)
  (function-put! 'inputcount 0)
  (if (null? (car expr))
    (function-put! 'inputcount -1)
    (begin
    (function-put! 'inputcount (rec_input expr 0))
    )
  )
)

;Function: Execute a line passed by eval-line, die if invalid instruction
(define (exec-line instr program line_num)
  (when (not (hash-has-key? *function-table* (car instr)))
        (die "~s is not a valid instruction." (car instr))
  )
  (cond
        ((eq? (car instr) 'goto)
         (eval-line program (hash-ref *label-table* (cadr instr)))
        )
        ((eq? (car instr) 'if)
         (if (hash_eval (car (cdr instr)))
            (eval-line program (hash-ref *label-table* (cadr (cdr instr))))
            (eval-line program (+ line_num 1))
         )
        )
        ((eq? (car instr) 'print)
         (if (null? (cdr instr))
           (newline)
           (sb_print (cdr instr))
         )
           (eval-line program (+ line_num 1))
        )
        (else
          ((hash-ref *function-table* (car instr)) (cdr instr))
          (eval-line program (+ line_num 1))
        )
  )
)

;Function: Find the length of list
(define length
   (lambda (ls)
     (if (null? ls)
         0
         (+ (length (cdr ls)) 1)
      )
   )
)

;Function: Walk through program and execute it. 
;This function takes a line number to execute.
(define (eval-line program line_num)
   (when (> (length program) line_num)
    (let((line (list-ref program line_num)))
    (cond
      ((= (length line) 3)
       (set! line (cddr line))
       (exec-line (car line) program line_num))
      ((and (= (length line) 2) (list? (cadr line)))
       (set! line (cdr line))
       (exec-line (car line) program line_num)
      )
      (else 
        (eval-line program (+ line_num 1))
      )
    )
    )
   )
)

;Push the labels into the *label-table*.
(define (write-program-by-line program)
   (map (lambda (line) 
          (when (not (null? line))
            (when (or (= 3 (length line))
                      (and (= 2 (length line)) 
                           (not (list? (cadr line)))
                      )
                  )
            (hash-set! *label-table* (cadr line) (- (car line) 1 ))
            )
          )
        ) program
   )
)

; This is the main function that gets called.
; readlist-from-inputfile: 
;   Sets program, that lists the commands from inputfile.
; write-program-by-line:
;   Fetch all the labels that occur in program
; eval-line:
;   Executes the program.

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))     
        (usage-exit)       
        (let* ((sbprogfile (car arglist))      
               (program (readlist-from-inputfile sbprogfile)))        
              (write-program-by-line program)      
              (eval-line program 0)
        )
    )
)

; Initialize the *function-table*
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair))
    )
    `(
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<>      ,(lambda (x y) (not (= x y))))
        (+ ,+) 
        (- ,-) 
        (* ,*) 
        (/ ,/)  
        (<= ,<=) 
        (>= ,>=) 
        (= ,=) 
        (> ,>)
        (< ,<) 
        (^ ,expt)
        (abs ,abs)
        (sin ,sin) 
        (cos ,cos)
        (tan ,tan)  
        (ceil ,ceiling) 
        (exp ,exp) 
        (floor ,floor)
        (asin ,asin) 
        (acos ,acos)
        (atan ,atan) 
        (round ,round)
        (log ,log) 
        (sqrt ,sqrt)
        (dim   ,sb_dim)
        (let   ,sb_let)
        (goto  (void))
        (if    (void))
        (print ,sb_print)
        (input ,sb_input)
      )
)

; Initialize the *variable-table*
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair))
    )
    `(
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
      )
)

(main (vector->list (current-command-line-arguments)))