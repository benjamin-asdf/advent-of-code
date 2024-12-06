
;; let's ⛏ day 6
;; ------------------

(pop-to-buffer (get-buffer-create "example-day6"))

;; up, down, left, right
(defvar-local move-direction 'up)
;;

(defun turn-90-degree (curr)
  (pcase curr
    ('up 'right)
    ('right 'down)
    ('down 'left)
    ('left 'up)))

(defun walk ()
  (pcase
      move-direction
    ('up
     (let ((coll (current-column)))
       (if (<= 1 (current-line))
           (progn
             (forward-line -1)
             (forward-char coll)
             t)
         'out-of-window)))
    ('down
     (let ((coll (current-column)))
       ;; this must be wrong
       (if (< (current-line)
              (save-excursion
                (end-of-buffer)
                (current-line)))
           (progn
             (forward-line)
             (forward-char coll)
             t)
         'out-of-window))
     ;; ok..
     )
    ('left
     (if (< 0 (current-column))
         (forward-char -1)
       'out-of-window))
    ('right
     (if (looking-at-p ".\n")
         'out-of-window
       (forward-char 1)))))

;; on sec, I want to peek sort of
;; so I walk once, 
(defun turn-p ()
  (save-excursion
    (walk)
    (looking-at-p "#")))

;; ok now implement the 90 degree thing


;; ok this should already make the first
;; line

(defun put-X ()
  (sit-for 0.025)
  (delete-char 1)
  (insert "X")
  (forward-char -1))

(with-current-buffer
    (get-buffer-create "example-day6")
  (setq-local move-direction 'up)
  (goto-char (point-min))
  (re-search-forward "\\^")
  ;; now our cursor is behind the
  ;; starting pos
  (forward-char -1)
  ;; ok now everywhere we walk,
  ;; put an X
  (put-X)
  (while (not (eq (walk) 'out-of-window))
    (when (turn-p)
      ;; turn
      (setf
       move-direction
       (turn-90-degree move-direction)))
    (put-X))
  (put-X)
  (let ((cnt 0))
    (goto-char (point-min))
    (while (re-search-forward "X" nil t)
      (setf cnt (+ 1 cnt)))
    cnt))


(defun part-1 (string)
  (with-current-buffer
      (get-buffer-create "*part-1*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (insert string)
    ;; don't know if that is also < > ^ v in the
    ;; the puzzle input
    (setq-local move-direction 'up)
    (goto-char (point-min))
    (re-search-forward "\\^")
    ;; now our cursor is behind the
    ;; starting pos
    (forward-char -1)
    ;; ok now everywhere we walk,
    ;; put an X
    (put-X)
    (while (not (eq (walk) 'out-of-window))
      (when (turn-p)
        ;; turn
        (setf
         move-direction
         (turn-90-degree move-direction)))
      (put-X))
    (put-X)
    (let ((cnt 0))
      (goto-char (point-min))
      (while (re-search-forward "X" nil t)
        (setf cnt (+ 1 cnt)))
      cnt)))

;; ahah
(erase-buffer)

;; ok now real input

(let ((curr-time (current-time)))
  (part-1 (with-current-buffer
              (find-file-noselect
               "/home/benj/repos/advent-of-code/inputs/2024/6/input")
            (buffer-string)))
  ;; (- curr-time)
  ;; (time-to-seconds (time-since curr-time))
  )

;; ok that's it for the moment!

*1

;; ah we can also look at the outcome for fun
4665

;; hahaha!


;; ok this sounds challenging
;; the brute force way is of course:

;; for each position: (minus the guard and current obstacles)
;;    play the game
;;    if out of window - no loop
;;    if loop - position makes loop


;; hm.
;; well, you only need to check the positions
;; where it would walk 'in the first place'
;; that was this 4665 number


;; 
;; loop - the moment you reach a pos you already
;; visited, + with the same move direction
;; 



;; 240ms
;; hm, it has 17030 positions
;; roughtly
(/ (* 0.240 4665) 60)
;; 18minutes lol
;; I could code it and go for a walk

;; I'll think about this.
;; I hope you liked part 1.

;; I'll do it once in slow mow 
