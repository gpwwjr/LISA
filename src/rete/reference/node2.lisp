;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

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

;;; File: node2.lisp
;;; Description:

;;; $Id: node2.lisp,v 1.21 2007/09/11 21:14:10 youngde Exp $

(in-package "LISA")

;;;
;;; Changes 2/20/2008 made by Aneil Mallavarapu:
;;;         Introduced FILTER slot in node2, which can hold 0 or more inter-pattern-filters.
;;;         An inter-pattern-filter reduces the number of tokens that must be 
;;;         subjected to join-node-tests. It is defined in inter-pattern-filter.lisp.
;;;         The filter uses hash-tables to quickly calculate a set of tokens
;;;
(defclass node2 (join-node) ((filters :initform () :accessor node2-filters)))

(defun make-node2 ()
  (make-instance 'node2))

(defmethod add-slot-filter ((self node2) slot)
  (push (make-inter-pattern-filter slot) (node2-filters self)))

;;; TEST RIGHT MEMORY
(defmethod test-against-right-memory ((self node2) left-tokens)
  (loop for right-token being the hash-values of (node2-filter-right-memory self left-tokens)
        when (test-tokens self left-tokens right-token)
        do (pass-tokens-to-successor self (combine-tokens left-tokens right-token))))

(defun node2-filter-right-memory (self left-tokens)
  (cond
   ((node2-filters self)
    (loop for ipfilter in (node2-filters self)
          for right-tokens = (inter-pattern-filter-right-lookup ipfilter left-tokens)
                        then (intersect-hash-table-keys
                              right-tokens
                              (inter-pattern-filter-right-lookup ipfilter left-tokens))
          until (zerop (hash-table-count right-tokens))
          finally (return right-tokens)))
   (t (join-node-right-memory self))))


;;; TEST LEFT MEMORY
(defmethod test-against-left-memory ((self node2) (right-token add-token))
  (loop for left-tokens being the hash-values of (node2-filter-left-memory self right-token)
        when (test-tokens self left-tokens right-token)
        do   (pass-tokens-to-successor
              self
              (combine-tokens left-tokens right-token))))

(defmethod test-against-left-memory ((self node2) (right-token remove-token))
  (loop for left-tokens being the hash-values of (node2-filter-left-memory self right-token)
        when (test-tokens self left-tokens right-token)
        do   (pass-tokens-to-successor
              self (combine-tokens
                    (make-remove-token left-tokens) right-token))))

(defun node2-filter-left-memory (self right-token)
  (cond 
   ((node2-filters self)
    (loop for ipfilter in (node2-filters self)
          for left-tokens* = (inter-pattern-filter-left-lookup ipfilter right-token)
                        then (intersect-hash-table-keys
                              left-tokens*
                              (inter-pattern-filter-left-lookup ipfilter right-token))
          until (zerop (hash-table-count left-tokens*))
          finally (return left-tokens*)))
   (t (join-node-left-memory self))))

;;;
;;; ACCEPT LEFT/RIGHT ADD-TOKEN
;;;
(defmethod accept-tokens-from-left ((self node2) (left-tokens add-token))
  (node2-add-tokens-to-left-memory self left-tokens)
  (test-against-right-memory self left-tokens))

(defun node2-add-tokens-to-left-memory (self left-tokens)
  (add-tokens-to-left-memory self left-tokens)
  (dolist (ipfilter (node2-filters self))
    (inter-pattern-filter-add-left-tokens ipfilter left-tokens)))
      
(defmethod accept-token-from-right ((self node2) (right-token add-token))
  (node2-add-token-to-right-memory self right-token)
  (test-against-left-memory self right-token))

(defun node2-add-token-to-right-memory (self right-token)
  (add-token-to-right-memory self right-token)
  (dolist (ipfilter (node2-filters self))
    (inter-pattern-filter-add-right-token ipfilter right-token)))

     
;;;
;;; ACCEPT LEFT/RIGHT REMOVE-TOKEN
;;;
(defmethod accept-tokens-from-left ((self node2) (left-tokens remove-token))
  (when (remove-tokens-from-left-memory self left-tokens)
    (dolist (ipfilter (node2-filters self))
      (inter-pattern-filter-remove-left-tokens ipfilter left-tokens))
    (test-against-right-memory self left-tokens)))

(defmethod accept-token-from-right ((self node2) (right-token remove-token))
  (when (remove-token-from-right-memory self right-token)
    (dolist (ipfilter (node2-filters self))
      (inter-pattern-filter-remove-right-token ipfilter right-token))
    (test-against-left-memory self right-token)))

;;; CLEAR-MEMORIES
(defmethod clear-memories ((self node2))
  (dolist (ipfilter (node2-filters self))
    (inter-pattern-filter-clear-memories ipfilter)))
