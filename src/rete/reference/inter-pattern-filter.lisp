
;;; Copyright (C) 2008 Aneil Mallavarapu

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: inter-pattern-filter.lisp

(in-package "LISA")

(defstruct (inter-pattern-filter (:constructor %make-inter-pattern-filter))
  left-slot-name   ; = binding-slot-name
  left-address     ; = binding-address
  right-slot-name  ; = pattern-slot-name
  ;; left/right-value-memories store hash-tables of tokens which match a particular value
  ;; value memories are hash-tables where
  ;;    key = slot-value, 
  ;;    value = hash-table storing tokens (key=token-hash-code, value=token)
  (negated nil)
  (left-value-memory (make-hash-table :test #'equal))  
  (right-value-memory (make-hash-table :test #'equal)))

(defun make-inter-pattern-filter (slot)
  (let ((binding (pattern-slot-slot-binding slot)))
    (%make-inter-pattern-filter 
     :left-slot-name (binding-slot-name binding)
     :left-address (binding-address binding)
     :right-slot-name (pattern-slot-name slot)
     :negated (pattern-slot-negated slot))))
;;;
;;; LEFT/RIGHT-LOOKUPS: 
;;; 
(defun inter-pattern-filter-left-lookup (self right-token)
  "Returns a hash-table of left-tokens which match the right-token for this slot-pattern binding"
 (let* ((left-value-memory      (inter-pattern-filter-left-value-memory self))
        (right-slot-name  (inter-pattern-filter-right-slot-name self))
        (right-fact       (token-top-fact right-token))
        (right-slot-value (get-slot-value right-fact right-slot-name)))

   (cond
    ((inter-pattern-filter-negated self)  (not-lookup left-value-memory right-slot-value))
    (t                                    (get-token-memory left-value-memory right-slot-value)))))

           
(defun inter-pattern-filter-right-lookup (self left-tokens)
  "Returns a hash-table of right-tokens which match the left-tokens for this slot-pattern binding"
  (let* ((right-value-memory (inter-pattern-filter-right-value-memory self))
         (left-slot-name     (inter-pattern-filter-left-slot-name self))
         (left-fact          (token-find-fact left-tokens (inter-pattern-filter-left-address self)))
         (left-slot-value    (get-slot-value left-fact left-slot-name)))
    (cond
     ((inter-pattern-filter-negated self)  (not-lookup right-value-memory left-slot-value))
     (t                                    (get-token-memory right-value-memory left-slot-value)))))

;;;
;;; NOT handling:
;;;
(defun not-lookup (value-memory value)
  "Returns a hash-table which contains an intersection of all tokens which do not match VALUE"
  (let ((newht (copy-hash-table value-memory)))
    ;; remove tokens which matching VALUE
    (remhash value newht) 
    ;; merge the remaining value memories
    (apply #'merge-hash-tables nil (hash-values-to-list newht))))
        
(defun not-value-remove-token (value-memory value token)
  "Removes TOKEN from all token memories which do not match VALUE"
  (loop for k being the hash-keys of value-memory
        for token-memory being the hash-values of value-memory
        unless (equal k value)
        do (forget-token token-memory token)))

;;;
;;; TOKEN-MEMORY - a value memory stores all tokens matching a particular value
;;;                it is a hash-table mapping token-hash-codes to tokens
;;;
(defun inter-pattern-filter-left-token-memory (self value)
  (get-token-memory (inter-pattern-filter-left-value-memory self) value))

(defun inter-pattern-filter-right-token-memory (self value)
  (get-token-memory (inter-pattern-filter-right-value-memory self) value))

(defun get-token-memory (value-memory value)
  (or (gethash value value-memory)
      (setf (gethash value value-memory) (make-hash-table :test #'equal))))

;;;
;;; ADD LEFT/RIGHT TOKENS:
;;;
(defun inter-pattern-filter-add-left-tokens (self left-tokens)
  (let* ((left-slot-name (inter-pattern-filter-left-slot-name self))
         (fact           (token-find-fact left-tokens 
                                          (inter-pattern-filter-left-address self)))
         (value          (get-slot-value fact left-slot-name)))
    (remember-token (inter-pattern-filter-left-token-memory self value)
                    left-tokens)))

(defun inter-pattern-filter-add-right-token (self right-token)
  (let* ((right-slot-name  (inter-pattern-filter-right-slot-name self))
         (fact             (token-top-fact right-token))
         (value            (get-slot-value fact right-slot-name)))
    (remember-token (inter-pattern-filter-right-token-memory self value)
                    right-token)))

;;;
;;; REMOVE LEFT/RIGHT TOKENS:
;;;

(defun inter-pattern-filter-remove-left-tokens (self left-tokens)
  (let* ((left-slot-name (inter-pattern-filter-left-slot-name self))
         (fact           (token-find-fact left-tokens 
                                          (inter-pattern-filter-left-address self)))
         (value          (get-slot-value fact left-slot-name)))
   (forget-token (inter-pattern-filter-left-token-memory self value)
                      left-tokens)))


(defun inter-pattern-filter-remove-right-token (self right-token)
  (let* ((right-slot-name  (inter-pattern-filter-right-slot-name self))
         (fact             (token-top-fact right-token))
         (value            (get-slot-value fact right-slot-name)))
   (forget-token (inter-pattern-filter-right-token-memory self value)
                    right-token)))

(defun inter-pattern-filter-left-size (self)
  (value-memory-size (inter-pattern-filter-left-value-memory self)))

(defun inter-pattern-filter-right-size (self)
  (value-memory-size (inter-pattern-filter-right-value-memory self)))

(defun value-memory-size (value-memory)
  (hash-table-count 
   (merge-hash-tables 
    nil 
    (hash-values-to-list value-memory))))

(defmethod left-value-memory-count ((self inter-pattern-filter))
  (inter-pattern-filter-left-size self))

(defmethod right-value-memory-count ((self inter-pattern-filter))
  (inter-pattern-filter-right-size self))

(defun inter-pattern-filter-clear-memories (self)
  (clrhash (inter-pattern-filter-left-value-memory self))
  (clrhash (inter-pattern-filter-right-value-memory self)))

;;;
;;; HASH-TABLE utils:
;;;

(defun merge-hash-tables (&optional result &rest hash-tables)
  "Copies the contents of hash-tables into RESULT.  
   If argument RESULT is NIL, a new hash-table is created"
  (flet ((hash-table-merge (ht1 ht2) ; merge ht2 into ht1
           (copy-hash-table ht2 ht1)))
    (reduce #'hash-table-merge 
            (list* 
             (or result
                 (make-hash-table :test (if hash-tables
                                            (hash-table-test (first hash-tables))
                                            #'equal)))
             hash-tables))))

(defun copy-hash-table (ht1 &optional ht2)
  "Copies hash-table HT1 - optionally to a user-provided hash-table, HT2"
  (loop with result = (or ht2 (make-hash-table :test (hash-table-test ht1)))
        for k being the hash-keys of ht1
        for v being the hash-values of ht1
        do (setf (gethash k result) v)
        finally (return result)))

(defun intersect-hash-table-keys (ht1 ht2)
  "Creates a new hash-table with keys representing the intersection of keys from HT1 and HT2,
   and the corresponding values of HT1."
  (loop with newht = (make-hash-table :test (hash-table-test ht1))
        for k being the hash-keys of ht1
        for v being the hash-values of ht1
        when (gethash k ht2)
        do (setf (gethash k newht) v)
        finally (return newht)))

(defun hash-values-to-list (ht)
  (loop for v being the hash-values of ht collect v))

