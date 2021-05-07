
                                        ; first need is to read some input file

(defun follow-instruction (accumulator instruction-set index indices-history) 
  "Primary loop where we process instructions"
  (if (gethash index indices-history) accumulator ; base case if we've already run this instruction
    (progn (puthash index t indices-history) 
           (let ((instruction (get-instruction (car instruction-set))) 
                 (amount (get-amount (car instruction-set))))
                                        ; rotate is an O(N) time complexity, worse than array
                                        ; printing because I haven't written tests
             (progn (message "instruction: %s %d" instruction amount) 
                    (message "accumulator: %d" accumulator) 
                    (cond ((string= instruction ";nop") 
                           (follow-instruction accumulator (-rotate (- 0 1) instruction-set) 
                                               (+ index 1) indices-history)) 
                          ((string= instruction ";acc") 
                           (follow-instruction (+ accumulator amount) 
                                               (-rotate (- 0 1) instruction-set) 
                                               (+ index 1) indices-history)) 
                          ((string= instruction ";jmp") 
                           (follow-instruction accumulator (-rotate (- 0 amount) instruction-set) 
                                               (+ index amount) indices-history))))))))

(defun get-amount (raw-instruction) 
  (setq both (split-string raw-instruction " ")) 
  (setq amount (string-to-number (car (cdr both))))
  amount)

(defun get-instruction (raw-instruction) 
  (setq both (split-string raw-instruction " ")) 
  (setq instruction (car both))
  instruction)

(setq input (f-read-text "test-input.txt" 'utf-8))
(setq instruction-set (butlast (split-string input "\n")))

(follow-instruction 0 instruction-set 0 (make-hash-table))
