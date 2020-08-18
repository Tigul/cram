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

(defun init()
  (cram-bullet-reasoning-belief-state::ros-time-init)
  (cram-location-costmap::location-costmap-vis-init)

  (prolog:prolog '(and
                   (btr:bullet-world ?world)
                   (btr:debug-window ?world)))

  (prolog:prolog '(and
                   (btr:bullet-world ?world)
                   (assert (btr:object ?world :static-plane
                            :floor
                            ((0 0 0) (0 0 0 1))
                            :normal (0 0 1) :constant 0
                            :collision-mask (:default-filter))))))

(defun spawn-kitchen ()
  (let ((kitchen-urdf
          (cl-urdf:parse-urdf
           (roslisp:get-param "kitchen_description"))))
    (prolog:prolog
     `(and (btr:bullet-world ?world)
           (rob-int:environment-name ?environment-name)
           (assert (btr:object ?world :urdf ?environment-name ((0 0 0) (0 0 0 1))
                               :urdf ,kitchen-urdf
                               :collision-group :static-filter
                               :collision-mask (:default-filter
                                                :character-filter))))))
  )

(defun spawn-robot ()
  (setf rob-int:*robot-urdf*
        (cl-urdf:parse-urdf
         (roslisp:get-param rob-int:*robot-description-parameter*)))
  (prolog:prolog
   `(and (btr:bullet-world ?world)
         (rob-int:robot ?robot)
         (assert (btr:object ?world :urdf ?robot ((0 0 0) (0 0 0 1))
                             :urdf ,rob-int:*robot-urdf*))
         (assert (btr:joint-state ?world ?robot (("torso_lift_joint" 0.15d0)))))))

(defun spawn-objects ()
  (btr:add-objects-to-mesh-list "cram_pr2_simple_demo")
  (let ((cereal-pose '((1.48 0.7 0.93) (0 0 1 0)))
        (bowl-pose '((1.46 0.85 0.9) (0 0 0 1)))
        (milk-pose '((1.4 0.97 0.93) (0 0 1 0)))
        (spoon-pose '((1.45 0.6 0.87) (0 0 0 1))))
    (btr-utils:spawn-object :cereal :cereal :pose cereal-pose)
    (btr-utils:spawn-object :bowl :bowl :pose bowl-pose)
    (btr-utils:spawn-object :milk :milk :pose milk-pose)
    (btr-utils:spawn-object :spoon :spoon :pose spoon-pose))
  )

(roslisp-utilities:register-ros-init-function init)
(roslisp-utilities:register-ros-init-function spawn-kitchen)
(roslisp-utilities:register-ros-init-function spawn-robot)
(roslisp-utilities:register-ros-init-function spawn-objects)
