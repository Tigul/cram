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
;;;     * Neither the name of the Institute for Artificial Intelligence /
;;;       University of Bremen nor the names of its contributors
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

(defun demo()
  (let ((object-types '( :bowl :milk :spoon))
        (grasps '( :top :front :top))
        (target-poses '(;;((-0.9 0.95 1.05) (0 0 0 1))
                       ((-0.75 1.2 0.9) (0 0 0 1))
                       ((-0.9 1 1.1) (0 0 0 1))
                       ((-1 1 1.15) (0 0 0 1))))
        (?init-pose (cl-transforms-stamped:make-pose-stamped
                     "map" 0
                     (cl-transforms:make-3d-vector 0.75 0.9 0)
                     (cl-transforms:make-identity-rotation))))
  (cram-urdf-projection:with-simulated-robot
    (exe:perform
     (desig:an action
               (type positioning-arm)
               (left-configuration park)
               (right-configuration park)))


    (exe:perform
     (desig:a motion
              (type going)
              (pose ?init-pose)))

    (loop for i from 0 to 4
          do (let ((target-pose (cram-tf:list->pose (nth i target-poses))))
               (move-object (nth i object-types) target-pose (nth i grasps))

               (exe:perform
                (desig:a motion
                         (type going)
                         (pose ?init-pose)))))
    

    )
    )
  )

(defun move-object (?object-type ?target ?grasp)
  (let ((?looking-pose (cl-transforms-stamped:make-pose-stamped
                        "map" 0
                        (cl-transforms:make-3d-vector 1.55 0.85 0.9)
                        (cl-transforms:make-identity-rotation)))
        (?table-pose (cl-transforms-stamped:make-pose-stamped
                                      "map" 0
                                      (cl-transforms:make-3d-vector -0.2 1 0)
                                      (cl-transforms:make-quaternion 0 0 1 0))))
  (exe:perform
   (desig:a motion
            (type looking)
            (pose ?looking-pose)))
  
  (let* ((?obj-det
          (exe:perform
           (desig:a motion
                    (type detecting)
                    (object (desig:an object
                                      (type ?object-type))))))
         (?pick-up-desig
           (desig:an action
                     (type picking-up)
                     ;;(arm :right)
                     (grasp ?grasp)
                     (object ?obj-det)))

         (?place-desig
            (desig:an action
               (type placing)
               (object ?obj-det)
               (target (desig:a location
                                (pose ?target))))))
    
    (exe:perform
     ?pick-up-desig)
    
    (exe:perform
     (desig:a motion
              (type going)
              (pose ?table-pose)))

    (exe:perform
     ?place-desig)

    (robot-state-changed)
    )))
(defun robot-state-changed ()
  (cram-occasions-events:on-event
   (make-instance 'cram-plan-occasions-events:robot-state-changed)))
