;;;
;;; Copyright (c) 2020, Jonas Dech <jdech[at]uni-bremen.de
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors 
;;;       may be used to endorse or promote products derived from this software 
;;;       without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cram-pr2-simple-demo)

(defmethod man-int:get-action-gripping-effort :heuristics 20 ((object-type (eql :bowl))) 50)
(defmethod man-int:get-action-gripping-effort :heuristics 20 ((object-type (eql :milk))) 50)
(defmethod man-int:get-action-gripping-effort :heuristics 20 ((object-type (eql :cereal))) 50)
(defmethod man-int:get-action-gripping-effort :heuristics 20 ((object-type (eql :spoon))) 50)

(defmethod man-int:get-action-gripper-opening :heuristics 20 ((object-type (eql :bowl))) 0.01)
(defmethod man-int:get-action-gripper-opening :heuristics 20 ((object-type (eql :milk))) 0.01)
(defmethod man-int:get-action-gripper-opening :heuristics 20 ((object-type (eql :cereal))) 0.01)
(defmethod man-int:get-action-gripper-opening :heuristics 20 ((object-type (eql :spoon))) 0.01)


(man-int:def-object-type-to-gripper-transforms :bowl '(:left :right) :right-side
  :grasp-translation `(0.0d0 0.3 0.03)
  :grasp-rot-matrix man-int:*y-across-z-grasp-rotation*
  :pregrasp-offsets `(0.0 0.03 0.05)
  :2nd-pregrasp-offsets `(0.0 0.3 0.0)
  :lift-translation `(0 0 0.05)
  :2nd-lift-translation `(0 0 0.05))

(man-int:def-object-type-to-gripper-transforms :cereal '(:left :right) :right-side
  :grasp-translation `(0.0d0 0.3 0.03)
  :grasp-rot-matrix man-int:*y-across-z-grasp-rotation*
  :pregrasp-offsets `(0.0 0.03 0.05)
  :2nd-pregrasp-offsets `(0.0 0.3 0.0)
  :lift-translation `(0 0 0.05)
  :2nd-lift-translation `(0 0 0.05))

(man-int:def-object-type-to-gripper-transforms :milk '(:left :right) :left-side
  :grasp-translation `(0.0d0 0.3 0.03)
  :grasp-rot-matrix man-int:*y-across-z-grasp-rotation*
  :pregrasp-offsets `(0.0 0.03 0.05)
  :2nd-pregrasp-offsets `(0.0 0.3 0.0)
  :lift-translation `(0 0 0.05)
  :2nd-lift-translation `(0 0 0.05))

(man-int:def-object-type-to-gripper-transforms :spoon '(:left :right) :right-side
  :grasp-translation `(0.0d0 0.3 0.03)
  :grasp-rot-matrix man-int:*y-across-z-grasp-rotation*
  :pregrasp-offsets `(0.0 0.03 0.05)
  :2nd-pregrasp-offsets `(0.0 0.3 0.0)
  :lift-translation `(0 0 0.05)
  :2nd-lift-translation `(0 0 0.05))
