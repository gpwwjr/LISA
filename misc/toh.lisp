;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LISA-USER; Base: 10. -*-

;;;    Towers of Hanoi puzzle, recursive version, rev3 (goal-based)
;;;    Written By:   George Williams

;;;    This version uses deftemplate instead of defclass, and templates
;;;    allow use of deffacts, resulting in a cleaner syntax all around.


(in-package "LISA-USER")
;(lisa:consider-taxonomy) ; not helpful when using deftemplate
#|   ---- useful code fragments to paste into the listener ----
(cl-user::lisa-app-setup)
(require 'lisa-debugger (lisa-system::lisa-debugger))

(progn (reset) (setq *ppfacts-count* 0))
(load "toh.lisp")

(clear-breaks)
(runtoh)
(next)
(resume)
(watch :bindings)
(pptohfacts)
|#
#|
These rules are started out as a version transliterated from the OPS5
(recursive) version in Rule-Based Programming by Kowalski and Levi. I've
modified it significantly to try to make up for the fact that the first
version never worked because it paid no attention to the size of the disks
being moved, and things happened out of order. And the startup magically
assumed that the disks were already in-place. :(
|#

(deftemplate peg ()
  (slot is)
  (slot has-disks))

(deftemplate disk ()
  (slot size)
  (slot peg))

;; ================================================================

#|
The goal/subgoal system that uses the goal class, the goal-related rules, and the
goal-related functions defined below is how I managed to control the sequencing
of the rules. It's probably not the right way to do things in a rule-based approach,
but it works. Also, this is a pretty general mechanism (sans the application-specific
arguments to the goals).
|#

(deftemplate goal ()
  (slot is)
  (slot arg1)
  (slot arg2)
  (slot arg3)
  (slot arg4)
  (slot status)
  (slot subgoals)
  (slot supergoal))

;; ----------------------------------------------------------------

(defun make-goal-of (supergoal goal-name &optional arg1 arg2 arg3 arg4)
  (let* ((?goal-object (make-instance 'goal ;; ?subgoal-object is a CLOS object, not a fact instance
                         :is goal-name
                         :status 'active
                         :supergoal supergoal
                         :arg1 arg1
                         :arg2 arg2
                         :arg3 arg3
                         :arg4 arg4))
         (?goal-instance (assert (?goal-object))))
    ?goal-instance))

(defun make-subgoal-of (supergoal goal-name arg1 arg2 arg3 arg4
                                  &key (suspend-super t))
  (let* ((?subgoal-instance (make-goal-of supergoal goal-name arg1 arg2 arg3 arg4)))
    (when supergoal
      (if suspend-super
          (modify supergoal
            (subgoals (append (get-slot-value supergoal 'subgoals)
                              (list ?subgoal-instance)))
            (status suspended))
          (modify supergoal
            (subgoals (append (get-slot-value supergoal 'subgoals)
                              (list ?subgoal-instance))))))
    ?subgoal-instance))

(defun finish-subgoal (goal supergoal)
  (modify supergoal
          (subgoals (remove goal (get-slot-value supergoal 'subgoals))))
  (let ((subgoals-of-super (get-slot-value supergoal 'subgoals)))
    (if subgoals-of-super
        (modify (first subgoals-of-super) (status active))
        ;; If there are not subgoals of the super, then it's
        ;; work is done and it should be retracted. But we must
        ;; also look at the super's supergoals, and retract￿
        ;; them recursively if they are completed. This is
        ;; handled by changing the super's status to completed,
        ;; which is then handled by a rule that looks for
        ;; completed goals, retracts them, but also checks to
        ;; see if their super needs to be marked as completed,
        ;; which then causes the super's to be retracted
        ;; recursively.
        (modify supergoal (status completed))))
  (retract goal))

;; ================================================================

(defvar *ppfactsmode* nil)
;(setq *ppfactsmode* 'none)
;(setq *ppfactsmode* nil)
(setq *ppfactsmode* 'brief)
(defvar *ppfacts-count* 0)
(defun pptohfacts (&rest fmt-args)
  "This function pretty-prints all the relavent facts in a useful format.
  It has proved to be very useful for debugging."
  (unless (eq *ppfactsmode* 'none)
    (when fmt-args
      (apply #'format (cons t fmt-args))) ; t is the default output stream
    (unless (eq *ppfactsmode* 'brief)
      (format t "================Begin PPTOHFACTS================  # ~a ~%" *ppfacts-count*))
    (let ((facts (get-fact-list (inference-engine)))
          ;; the following 3 arrays are indexed by peg number
          (peg-ids       (make-array 3 :initial-element 0))
          (disk-count    (make-array 3 :initial-element 0))
          (peg-has-disks (make-array 3)) ;; each entry is a list of disk instances (from the peg's has-disks slot), topmost first
          (disk-list nil) ;; each element of disk-list is a list (<disk-fact-id> <disk-size>)
          (goals nil)     ;; a list of goal instances
          )
      ;; collect information about all the facts in WM
      (dolist (fact facts)
        (let ((fact-name (fact-name fact))
              (fact-id (fact-id fact))
              (slot-table (fact-slot-table fact)))
          (cond
           ((string= fact-name "PEG")
            (let ((peg# (gethash 'is slot-table)))
              (setf (aref peg-has-disks peg#)
                    (gethash 'has-disks slot-table))
              (setf (aref disk-count peg#)
                    (length (aref peg-has-disks (gethash 'is slot-table))))
              (setf (aref peg-ids peg#) fact-id)))
           ((string= fact-name "DISK")
            (push (list fact-id (gethash 'size slot-table))
                  disk-list))
           ((string= fact-name "GOAL")
            (push fact goals)))))
      ;; Summarize information about each of the disks
      #| ; this was useful in early debugging, but didn't need it later
      (format t "~%There are ~a disks on the disk-list:~%" (length disk-list))
      (dolist (disk-entry (sort disk-list #'< :key #'second))
        ; each element of disk-list is a list of the form (<disk-fact-id> <disk-size>)
        (let* ((disk-fact (fact (first disk-entry)))
               (slot-table (fact-slot-table disk-fact)))
          (format t "  disk ~a [id ~a] is on peg# ~a~%"
                  (gethash 'size slot-table)
                  (fact-id disk-fact)
                  (gethash 'peg slot-table))))
      (terpri)
      |#
      ;; Summarize information about each of the pegs
      (format t "Disks (top to bottom) on each Peg:~%")
      (dotimes (peg# 3)
        (if (> (aref disk-count peg#) 0)
            (progn
              (format t "  Peg ~a[~a] has  ~a disks:"
                      peg# (aref peg-ids peg#) (aref disk-count peg#))
              (dolist (disk (aref peg-has-disks peg#))
                (format t " ~a[~a]"
                        (get-slot-value disk 'size)
                        (fact-id disk)))
              (terpri))
            (format t "  Peg ~a[~a] has no disks~%"
                    peg# (aref peg-ids peg#))))
      (terpri)
      (unless (eq *ppfactsmode* 'brief)
        ;; print info about all goals, including their subgoals
        (if goals
            (progn
              (format t "There are ~a goals:~%" (length goals))
              ; sort the goals so that the active ones are printed first
              ; ordering will be: active or suspended
              (setq goals (sort goals
                                #'(lambda (g1 g2)
                                    (string-lessp (symbol-name (get-slot-value g1 'status))
                                                  (symbol-name (get-slot-value g2 'status))))))
              (dolist (goal goals)
                (let* ((slot-table (fact-slot-table goal))
                       (goal-is (gethash 'is slot-table)))
                  (format t "  Goal is: ~a [id ~a], status: ~a~%    args: ~a, ~a, ~a, ~a" ; note: no end-of-line
                          goal-is
                          (fact-id goal)
                          (gethash 'status slot-table)
                          (gethash 'arg1 slot-table)
                          (gethash 'arg2 slot-table)
                          (gethash 'arg3 slot-table)
                          (gethash 'arg4 slot-table))
                  (case goal-is
                    (move
                     (format t " (#disks, src, dst, size)~%"))
                    (move-stack
                     (format t " (stack-base-disk-size, dst-peg-num)~%"))
                    (move-1-disk
                     (format t " (src-peg-num, dst-peg-num)~%"))
                    (consolidate-substack
                     (format t " (base-disk-size, other-disk-size)~%"))
                    (otherwise (terpri)))
                  (if (gethash 'supergoal slot-table)
                      (let* ((supergoal (gethash 'supergoal slot-table))
                             (sg-slot-table (fact-slot-table supergoal)))
                        (format t "    supergoal: ~a [id: ~a], status: ~a~%"
                                (gethash 'is sg-slot-table)
                                (fact-id supergoal)
                                (gethash 'status sg-slot-table)))
                      ;(format t "    supergoal: none~%")
                      )
                  (let ((subs (gethash 'subgoals slot-table)))
                    (if subs
                        (progn
                          (format t "    subgoals:~%")
                          (dolist (sub subs)
                            (format t "        ~a~%" sub)))
                        ;(format t "    subgoals: none~%")
                        )))))
            (format t "There are no goals~%"))
        )
      )
    (unless (eq *ppfactsmode* 'brief)
      (format t "================End PPTOHFACTS================  # ~a ~%" *ppfacts-count*)
      (incf *ppfacts-count*))
    (values)))

;; ================================================================

;; These rules need to be defined after pptohfacts is defined.  Otherwise,
;; they'd be grouped with the rest of the goal-related stuff above

(defrule retract-completed-goals ()
  (?goal (goal (status completed)
               (arg1 ?arg1) (arg2 ?arg2) (arg3 ?arg3) (arg4 ?arg4)
               (supergoal ?supergoal)))
  (?goal2 (goal (status ?status-of-super) (subgoals ?subgoals-of-super)))
  (test (eq ?supergoal ?goal2))
  =>
  (when ?supergoal
    (if (and ?subgoals-of-super
             (eql ?status-of-super 'suspended))
        (modify ?supergoal (status check-completion) (subgoals (rest ?subgoals-of-super)))
        (modify ?supergoal (status completed))))
;  (format t "}}} in retract-completed-goals - retracting goal ~%")
  (retract ?goal)
  (pptohfacts "** RETRACT-COMPLETED-GOALS after RHS~%"))

(defrule check-goal-completion ()
  (?goal (goal (status check-completion)
               (arg1 ?arg1) (arg2 ?arg2) (arg3 ?arg3) (arg4 ?arg4)
               (subgoals ?subgoals) (supergoal ?supergoal)))
  (?goal2 (goal (status ?status-of-super)
                (subgoals ?subgoals-of-super)))
  (test (eq ?supergoal ?goal2))
  =>
  (if ?subgoals
      ; since there are still subgoals, the goal has more work to do
      (modify ?goal (status active))
      ; no subgoals, so it's done
      (progn
        (when ?supergoal
          ; clean up the linkage between the supergoal the goal (to be retracted)
          (modify ?supergoal
            (status check-completion)
            (subgoals (rest ?subgoals-of-super))))
        (retract ?goal)))
  (pptohfacts  "** CHECK-GOAL-COMPLETION after RHS~%"))

(defrule check-goal-completion-no-super ()
  (?goal (goal (status check-completion)
               (arg1 ?arg1) (arg2 ?arg2) (arg3 ?arg3) (arg4 ?arg4)
               (subgoals ?subgoals) (supergoal nil)))
  =>
  (if ?subgoals
      (modify ?goal (status active))
      (retract ?goal))
  (pptohfacts "** CHECK-GOAL-COMPLETION-NO-SUPER after RHS~%"))

;; ================================================================

#|   The actual application....
To move n disks from one peg to another, do the following:
 1. move the top n-1 disks to a third peg
 2. move the remaining disk
 3. move the n-1 disks from the third peg to the desired destination.
|#

;; This rule only plans the moves. The next rule performs the action.
(defrule move-many-disks ()
  (?goal (goal (is move)
               (arg1 ?ndisks (> ?ndisks 1)) (arg2 ?src) (arg3 ?dst) (arg4 ?size)
               (status active)
               (subgoals ?subgoals) (supergoal ?supergoal)))
  (peg (is ?src))
  (peg (is ?other))
  (test (and (not (= ?other ?src))
             (not (= ?other ?dst))))
  =>
;  (format t "}}} in move-many-disks - args = ~a, ~a, ~a, ~a~%" ?ndisks ?src ?dst ?size)
;  (format t "}}} in move-many-disks (length ?subgoals) = ~a~%" (length ?subgoals))

  (if ?subgoals
      (progn ;; We've already been here, so activate first subgoal, and suspend self
        ;(format t "}}} in move-many-disks - activating first subgoal, suspending ?goal ~%")
        (modify (first ?subgoals) (status active))
        (modify ?goal (status suspended)))
      (progn
        ;; It's the first time this goal has been activated, so farm out
        ;; work to subgoals

        ;;; has-disks of ?src is not a good way to determine if the job is done
        ;; in the planning stage it doesn't tell us anything, and in the
        ;; execution stage, it could be a false indicator because the goal could
        ;; have been intended to only move some of the disks
  
        ;;; this rule should only be invoked in the planning stage
        ;; subgoals should modify it's list of subgoals as they're completed,
        ;; and should retract this goal when the last one is completed

;        (format t "}}} in move-many-disks - making 3 subgoals, suspending subgoals 2&3 ~%")
        (make-subgoal-of ?goal 'move (- ?ndisks 1) ?src   ?other nil)
        ; the first subgoal is active by default
        (let ((?goal2 (make-subgoal-of ?goal 'move 1             ?src   ?dst   ?size))
              (?goal3 (make-subgoal-of ?goal 'move (- ?ndisks 1) ?other ?dst   nil)))
          (modify ?goal2 (status suspended))
          (modify ?goal3 (status suspended)))

        ;; once the subgoals have been completed and removed from the subgoals
        ;; list, the cleanup-move-goals rule will retract the goal
        ;;; note that this requires all move goals to have supergoals
        ))
  (pptohfacts "** MOVE-MANY-DISKS after RHS~%")
  )

;(set-break 'move-many-disks)

;; The action
(defrule move-1-disk () ; (:salience -1)
  (?goal (goal (is move)
               (arg1 1) (arg2 ?src) (arg3 ?dst) (arg4 ?size) ; arg1 is #disks
               (status active) (supergoal ?supergoal) (subgoals ?subgoals)))
  (?src-peg (peg (is ?src) (has-disks ?src-peg-has-disks)))
  (?dst-peg (peg (is ?dst) (has-disks ?dst-peg-has-disks)))
  (?3rd-peg (peg (is ?3rd) (has-disks ?3rd-peg-has-disks)))
  ;; assure src, dst, and third pegs are all different
  (test (and (/= ?src ?dst)
             (/= ?3rd ?src)
             (/= ?3rd ?dst)))
  =>
;  (format t "}}} in move-1-disk - args = ~a, ~a, ~a, ~a~%" 1 ?src ?dst ?size)
;  (format t "}}} in move-1-disk (length ?src-peg-has-disks) = ~a~%" (length ?src-peg-has-disks))
  (if ?src-peg-has-disks
      (let ((?final-dst nil) ; if this stays nil, then we can't move the disk
            ?final-dst-peg
            ?final-dst-peg-has-disks
            (disk-size (get-slot-value (first ?src-peg-has-disks) 'size))
            (dst-top-disk-size (when ?dst-peg-has-disks
                                 (get-slot-value (first ?dst-peg-has-disks) 'size)))
            (3rd-top-disk-size (when ?3rd-peg-has-disks
                                 (get-slot-value (first ?3rd-peg-has-disks) 'size))))
        ; try to set ?final-dst & related
        (cond
         ((or (null dst-top-disk-size)
              (and dst-top-disk-size
                   (< disk-size dst-top-disk-size)))
          (setq ?final-dst ?dst
                ?final-dst-peg ?dst-peg
                ?final-dst-peg-has-disks ?dst-peg-has-disks))
         ((or (null 3rd-top-disk-size)
              (and 3rd-top-disk-size
                   (< disk-size 3rd-top-disk-size)))
          (setq ?final-dst ?3rd
                ?final-dst-peg ?3rd-peg
                ?final-dst-peg-has-disks ?3rd-peg-has-disks)))
;        (format t "}}} in move-1-disk ?final-dst = ~a~%" ?final-dst)
        (if ?final-dst
            (progn
              (format t " Move disk ~a from peg ~a to peg ~a~%" disk-size ?src ?final-dst)
;              (format t "}}} in move-1-disk ?goal = ~a~%" ?goal)
;              (format t "}}} in move-1-disk ?supergoal = ~a~%" ?supergoal)
              ; modify the peg# of the disk to move
              (modify (first ?src-peg-has-disks) (peg ?final-dst))
              ; remove the disk to move from src-peg
              (modify ?src-peg (has-disks (rest ?src-peg-has-disks)))
              ; put the disk to move on top of the ?final-dst-peg
              (modify ?final-dst-peg (has-disks (cons (first ?src-peg-has-disks) ?final-dst-peg-has-disks)))
              ; retract ?goal, after cleaning up the subgoals list of any
              ; supergoal(s), and activating whatever's next
              (finish-subgoal ?goal ?supergoal))
            ;; we should never get here
            (cl:assert nil () "in move-1-disk: *** CAN'T MOVE *** disk ~a from peg ~a to peg ~a~%" disk-size ?src ?final-dst)))
      ;; we should never get here
      (cl:assert nil () "in move-1-disk: source peg ~a has no disks~%" ?src))
  (pptohfacts "** MOVE-1-DISK after RHS~%")
  )

;(set-break 'move-1-disk)

;; ================================================================

;; Finish
(defrule finish (:salience -10)
  (goal (is finish))
  =>
  (format t " *** all done ***~%"))

;; Default rules to cleanup leftover facts

(defrule cleanup-disks (:salience -100)
  (?fact (disk))
  =>
  (retract ?fact))

(defrule cleanup-pegs (:salience -100)
  (?fact (peg))
  =>
  (retract ?fact))

(defrule cleanup-goals (:salience -100)
  (?fact (goal))
  =>
  (retract ?fact))

;;; initial facts

(deffacts setup-toh-facts ()
  (peg (is 0))
  (peg (is 1))
  (peg (is 2))
  (disk (size 1) (peg 0))
  (disk (size 2) (peg 0))
  (disk (size 3) (peg 0))
  (disk (size 4) (peg 0))
#|
  (disk (size 5) (peg 0))
  (disk (size 6) (peg 0))
  (disk (size 7) (peg 0))
|#
  (goal (is startup-part2) (status active)))

;;; startup rule...

(defrule startup-part1 ()
  =>
  ;; create the goal to run startup-part2
  (assert (goal (is startup-part2) (status active)))
  )

(defrule startup-part2 ()
  (?goal (goal (is startup-part2)))
  (?peg0 (peg (is 0)))
  (?disk1 (disk (size 1)))
  (?disk2 (disk (size 2)))
  (?disk3 (disk (size 3)))
  (?disk4 (disk (size 4)))
#|
  (?disk5 (disk (size 5)))
  (?disk6 (disk (size 6)))
  (?disk7 (disk (size 7)))
|#
  =>
  ;(pptohfacts "** STARTUP-PART2 before RHS~%")
  (retract ?goal) ; so we don't do this again
  ;; modify facts so bookkeeping is correct
;  (modify ?peg0 (has-disks (list ?disk1 ?disk2 ?disk3 ?disk4 ?disk5 ?disk6 ?disk7)))
  (modify ?peg0 (has-disks (list ?disk1 ?disk2 ?disk3 ?disk4)))

  ;; modify the goal to start things up
  ;; need to have a supergoal for all move goals
  (let ((finish-goal (make-goal-of nil 'finish)))
;    (assert ((make-goal-of finish-goal 'move 7 0 1)))	; (#disks, src, dst, size) typically, ndisks is 8
    (assert ((make-goal-of finish-goal 'move 4 0 1)))	; (#disks, src, dst, size) typically, ndisks is 8
    )

  (let ((*ppfactsmode* 'brief))
    (pptohfacts "** STARTUP-PART2 after RHS~%"))
  )


(defun runtoh (&optional (ntimes 1))
  (flet ((repeat-toh ()
           (dotimes (i ntimes)
             (format t "Starting run.~%")
             (reset)
             (run))))
    (time (repeat-toh))))

