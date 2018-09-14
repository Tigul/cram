;;;
;;; Copyright (c) 2017, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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

(in-package :demo)

(defun pose-list->desig (pose-list)
  (let ((?pose (cl-transforms-stamped:pose->pose-stamped
                "map" 0.0
                (btr:ensure-pose pose-list))))
    (desig:a location (pose ?pose))))

(defparameter *object-cad-models*
  '(;; (:cup . "cup_eco_orange")
    ;; (:bowl . "edeka_red_bowl")
    ))

(defparameter *object-colors*
  '((:spoon . "blue")))

(defmethod exe:generic-perform :before (designator)
  (format t "~%PERFORMING~%~A~%~%" designator))

(cpl:def-cram-function initialize-or-finalize ()
  (cpl:with-failure-handling
      ((cpl:plan-failure (e)
         (declare (ignore e))
         (return)))
    (cpl:par
      (pp-plans::park-arms)
      (let ((?pose (cl-transforms-stamped:make-pose-stamped
                    cram-tf:*fixed-frame*
                    0.0
                    (cl-transforms:make-identity-vector)
                    (cl-transforms:make-identity-rotation))))
        (exe:perform
         (desig:an action
                   (type going)
                   (target (desig:a location
                                    (pose ?pose))))))
      (exe:perform (desig:an action (type opening) (gripper (left right))))
      (exe:perform (desig:an action (type looking) (direction forward))))))

#+perceiving-tray-on-kitchen-island
(pr2-pms:with-real-robot
  (desig:copy-designator
   (perform (an action
               (type detecting)
               (object (an object
                           (size "large")
                           (color "yellow")
                           (location (desig:a location
                                              (on (desig:an object
                                                            (owl-name "kitchen_island_counter_top")))))))))
   :new-description '((:type :tray))))

(cpl:def-cram-function demo-random (&optional
                                    (random t)
                                    (list-of-objects '(:bowl :spoon :cup :milk :breakfast-cereal)))

  ;;(when ccl::*is-logging-enabled*
  ;;    (setf ccl::*is-client-connected* nil)
  ;;    (ccl::connect-to-cloud-logger)
  ;;    (ccl::reset-logged-owl))

  (btr:detach-all-objects (btr:get-robot-object))
  (btr:detach-all-objects (btr:object btr:*current-bullet-world* :kitchen))
  (btr-utils:kill-all-objects)
  (setf (btr:joint-state (btr:object btr:*current-bullet-world* :kitchen)
                         "sink_area_left_upper_drawer_main_joint")
        0.0)
  (btr-belief::publish-environment-joint-state
   (btr:joint-states (btr:object btr:*current-bullet-world* :kitchen)))

  (setf desig::*designators* (tg:make-weak-hash-table :weakness :key))

  (cond ((eql cram-projection:*projection-environment*
              'cram-pr2-projection::pr2-bullet-projection-environment)
         (if random
             (spawn-objects-on-sink-counter-randomly)
             (spawn-objects-on-sink-counter)))
        (t
         (json-prolog:prolog-simple "rdf_retractall(A,B,C,belief_state).")
         (btr-belief::call-giskard-environment-service :kill-all "attached")
         (cram-bullet-reasoning-belief-state::call-giskard-environment-service
          :add-kitchen
          "kitchen"
          (cl-transforms-stamped:make-pose-stamped
           "map"
           0.0
           (cl-transforms:make-identity-vector)
           (cl-transforms:make-identity-rotation)))))

  ;; (setf cram-robot-pose-guassian-costmap::*orientation-samples* 3)

  (initialize-or-finalize)

  (let ((object-fetching-locations
          `((:breakfast-cereal . ,(desig:a location
                                           (on (desig:an object
                                                         (type counter-top)
                                                         (urdf-name sink-area-surface)
                                                         (owl-name "kitchen_sink_block_counter_top")
                                                         (part-of kitchen)))
                                           (side left)
                                           (side front)
                                           (range 0.5)))
            (:cup . ,(desig:a location
                              (side left)
                              (on (desig:an object
                                            (type counter-top)
                                            (urdf-name sink-area-surface)
                                            (owl-name "kitchen_sink_block_counter_top")
                                            (part-of kitchen)))))
            (:bowl . ,(desig:a location
                               (on (desig:an object
                                             (type counter-top)
                                             (urdf-name sink-area-surface)
                                             (owl-name "kitchen_sink_block_counter_top")
                                             (part-of kitchen)))
                               (side left)
                               (side front)
                               (range-invert 0.5)))
            (:spoon . ,(desig:a location
                                (in (desig:an object
                                              (type drawer)
                                              (urdf-name sink-area-left-upper-drawer-main)
                                              (owl-name "drawer_sinkblock_upper_open")
                                              (part-of kitchen)))
                                (side front)))
            (:milk . ,(desig:a location
                               (side left)
                               (side front)
                               (range 0.5)
                               (on;; in
                                (desig:an object
                                             (type counter-top)
                                             (urdf-name sink-area-surface ;; iai-fridge-main
                                                        )
                                             (owl-name "kitchen_sink_block_counter_top"
                                                       ;; "drawer_fridge_upper_interior"
                                                       )
                                             (part-of kitchen)))))))
        (object-placing-locations
          (let ((?pose
                  (cl-transforms-stamped:make-pose-stamped
                   "map"
                   0.0
                   (cl-transforms:make-3d-vector -0.78 0.8 0.95)
                   (cl-transforms:make-quaternion 0 0 0.6 0.4))))
            `((:breakfast-cereal . ,(desig:a location
                                             (pose ?pose)
                                             ;; (left-of (an object (type bowl)))
                                             ;; (far-from (an object (type bowl)))
                                             ;; (for (an object (type breakfast-cereal)))
                                             ;; (on (desig:an object
                                             ;;               (type counter-top)
                                             ;;               (urdf-name kitchen-island-surface)
                                             ;;               (owl-name "kitchen_island_counter_top")
                                             ;;               (part-of kitchen)))
                                             ;; (side back)
                                             ))
              (:cup . ,(desig:a location
                                (right-of (an object (type bowl)))
                                ;; (behind (an object (type bowl)))
                                (near (an object (type bowl)))
                                (for (an object (type cup)))))
              (:bowl . ,(desig:a location
                                 (on (desig:an object
                                               (type counter-top)
                                               (urdf-name kitchen-island-surface)
                                               (owl-name "kitchen_island_counter_top")
                                               (part-of kitchen)))
                                 (context table-setting)
                                 (for (an object (type bowl)))
                                 (object-count 3)
                                 (side back)
                                 (side right)
                                 (range-invert 0.5)))
              (:spoon . ,(desig:a location
                                  (right-of (an object (type bowl)))
                                  (near (an object (type bowl)))
                                  (for (an object (type spoon)))))
              (:milk . ,(desig:a location
                                 (left-of (an object (type bowl)))
                                 (far-from (an object (type bowl)))
                                 (for (an object (type milk)))))))))

    ;; (an object
    ;;     (obj-part "drawer_sinkblock_upper_handle"))

    (dolist (?object-type list-of-objects)
      (let* ((?fetching-location
               (cdr (assoc ?object-type object-fetching-locations)))
             (?delivering-location
               (cdr (assoc ?object-type object-placing-locations)))
             (?arm-to-use
               (cdr (assoc ?object-type *object-grasping-arms*)))
             (?cad-model
               (cdr (assoc ?object-type *object-cad-models*)))
             (?color
               (cdr (assoc ?object-type *object-colors*)))
             (?object-to-fetch
               (desig:an object
                         (type ?object-type)
                         (location ?fetching-location)
                         (desig:when ?cad-model
                           (cad-model ?cad-model))
                         (desig:when ?color
                           (color ?color)))))

        (when (eq ?object-type :bowl)
          (cpl:with-failure-handling
              ((common-fail:high-level-failure (e)
                 (roslisp:ros-warn (pp-plans demo) "Failure happened: ~a~%Skipping the search" e)
                 (return)))
            (let ((?loc (cdr (assoc :breakfast-cereal object-fetching-locations))))
              (exe:perform
               (desig:an action
                         (type searching)
                         (object (desig:an object (type breakfast-cereal)))
                         (location ?loc))))))

        (cpl:with-failure-handling
            ((common-fail:high-level-failure (e)
               (roslisp:ros-warn (pp-plans demo) "Failure happened: ~a~%Skipping..." e)
               (return)))
          (if (eq ?object-type :bowl)
              (exe:perform
               (desig:an action
                         (type transporting)
                         (object ?object-to-fetch)
                         ;; (arm right)
                         (location ?fetching-location)
                         (target ?delivering-location)))
              (if (eq ?object-type :breakfast-cereal)
                  (exe:perform
                   (desig:an action
                             (type transporting)
                             (object ?object-to-fetch)
                             ;; (arm right)
                             (location ?fetching-location)
                             (target ?delivering-location)))
                  (exe:perform
                   (desig:an action
                             (type transporting)
                             (object ?object-to-fetch)
                             ;; (arm ?arm-to-use)
                             (location ?fetching-location)
                             (target ?delivering-location))))))

        ;; (setf pr2-proj-reasoning::*projection-reasoning-enabled* nil)
        )))

  ;; (setf pr2-proj-reasoning::*projection-reasoning-enabled* nil)

  (initialize-or-finalize)

  ;;(when ccl::*is-logging-enabled*
  ;;  (ccl::export-log-to-owl "ease_milestone_2018.owl")
  ;;  (ccl::export-belief-state-to-owl "ease_milestone_2018_belief.owl"))

  cpl:*current-path*)



(defun generate-training-data (&optional debug-mode)
  (pr2-proj:with-simulated-robot

    (cram-mobile-pick-place-plans:park-arms)

    (let ((?pose (cl-transforms-stamped:make-pose-stamped
                  "map" 0.0
                  (cl-transforms:make-3d-vector -0.15 1.0 0)
                  (cl-transforms:make-quaternion 0 0 1 0))))
      (exe:perform
       (desig:an action
                 (type going)
                 (target (desig:a location (pose ?pose))))))

    (let ((?pose (cl-transforms-stamped:make-pose-stamped
                  "base_footprint" 0.0
                  (cl-transforms:make-3d-vector 0.5 0 0.9)
                  (cl-transforms:make-identity-rotation))))
      (exe:perform
       (desig:an action
                 (type looking)
                 (target (desig:a location (pose ?pose)))))
      (exe:perform
       (desig:an action
                 (type looking)
                 (target (desig:a location (pose ?pose)))))
      (exe:perform
       (desig:an action
                 (type looking)
                 (target (desig:a location (pose ?pose)))))
      (exe:perform
       (desig:an action
                 (type looking)
                 (target (desig:a location (pose ?pose))))))

    (btr:detach-all-objects (btr:get-robot-object))
    (btr-utils:kill-all-objects)

    (setf cram-pr2-projection::*ik-solution-cache*
          (make-hash-table :test 'cram-pr2-projection::arm-poses-equal-accurate))

    (when debug-mode
      (btr-utils:spawn-object 'red-dot
                              :pancake-maker
                              :color '(1 0 0 0.5)
                              :pose '((0.0 0.0 -1.0) (0 0 0 1)))
      (btr-utils:spawn-object 'green-dot
                              :pancake-maker
                              :color '(0 1 0 0.5)
                              :pose '((0.0 0.0 -1.0) (0 0 0 1)))
      (setf pr2-proj::*debug-long-sleep-duration* 0.5)
      (setf pr2-proj::*debug-short-sleep-duration* 0.1))

    (unwind-protect
         (dolist (?object-type '(:bowl :spoon :cup :milk :breakfast-cereal))
           (let ((btr-object (btr:add-object btr:*current-bullet-world*
                                             :mesh
                                             'object-to-grasp
                                             (cl-transforms:make-identity-pose)
                                             :mesh ?object-type
                                             :mass 0.2
                                             :color '(1 0 0))))
             (dolist (?arm '(:left :right))
               (dolist (rotation-axis (list (cl-transforms:make-3d-vector 1 0 0)
                                            (cl-transforms:make-3d-vector 0 1 0)
                                            (cl-transforms:make-3d-vector 0 0 1)))
                 (dolist (rotation-angle (list (* pi 0.0)
                                               (* pi 0.5)
                                               (* pi 1.0)
                                               (* pi 1.5)))
                   (let* ((orientation (cl-transforms:axis-angle->quaternion
                                        rotation-axis rotation-angle)))
                     (let ((pose-for-bb-calculation (cl-transforms:make-pose
                                                     (cl-transforms:make-3d-vector 0 0 -1)
                                                     orientation)))
                       (setf (btr:pose btr-object) pose-for-bb-calculation)
                       (let* ((bb-dims (cl-bullet:bounding-box-dimensions
                                        (cl-bullet:aabb btr-object)))
                              (z/2 (/ (cl-transforms:z bb-dims) 2)))
                         (dolist (position-y-offset ;; '(0 -0.3 0.3)
                                  '(0.0))
                           (let ((position (cl-transforms:make-3d-vector
                                            -0.75
                                            (+ 1.0 position-y-offset)
                                            (+ 0.8573 z/2))))
                             (setf (btr:pose btr-object) (cl-transforms:make-pose
                                                          position
                                                          orientation))
                             (when debug-mode
                               (cpl:sleep 0.5))
                             (btr:simulate btr:*current-bullet-world* 10)
                             (if (> (abs (cl-transforms:normalize-angle
                                          (cl-transforms:angle-between-quaternions
                                           (cl-transforms:orientation
                                            (btr:pose btr-object))
                                           orientation)))
                                    1.0)
                                 (when debug-mode
                                   (format t "~a with orientation ~a unstable.~%Skipping...~%"
                                           ?object-type orientation)
                                   (btr-utils:move-object 'red-dot '((-1.0 2.0 1.0) (0 0 0 1)))
                                   (btr-utils:move-object 'green-dot '((0.0 0.0 -1.0) (0 0 0 1)))
                                   (cpl:sleep 0.5))
                                 (progn
                                   (when debug-mode
                                     (btr-utils:move-object 'green-dot '((-1.0 2.0 1.0) (0 0 0 1)))
                                     (btr-utils:move-object 'red-dot '((0.0 0.0 -1.0) (0 0 0 1)))
                                     (cpl:sleep 0.5))
                                   (cpl:with-failure-handling
                                       ((cram-language:simple-plan-failure (e)
                                          (when debug-mode
                                            (format t "Error happened: ~a~%Ignoring..." e)
                                            (btr-utils:move-object 'red-dot
                                                                   '((-1.0 2.0 1.0) (0 0 0 1)))
                                            (btr-utils:move-object 'green-dot
                                                                   '((0.0 0.0 -1.0) (0 0 0 1)))
                                            (cpl:sleep 0.5))
                                          (return)))
                                     (cram-mobile-pick-place-plans:park-arms)
                                     (let* ((?object-designator
                                              (exe:perform
                                               (desig:an action
                                                         (type detecting)
                                                         (object (desig:an object
                                                                           (type ?object-type))))))
                                            (pick-up-action-designator
                                              (desig:an action
                                                        (type picking-up)
                                                        (arm ?arm)
                                                        (object ?object-designator))))
                                       (pr2-proj-reasoning:check-picking-up-collisions
                                        pick-up-action-designator)
                                       (setf pick-up-action-designator
                                             (desig:current-desig pick-up-action-designator))
                                       (exe:perform pick-up-action-designator)
                                       (when debug-mode
                                         (cpl:sleep 0.5))
                                       (btr:detach-object (btr:get-robot-object) btr-object)
                                       (cram-mobile-pick-place-plans:park-arms)))))))))))))
             (btr:remove-object btr:*current-bullet-world* 'object-to-grasp)))

      (btr:remove-object btr:*current-bullet-world* 'object-to-grasp)
      (when debug-mode
        (btr-utils:kill-object 'red-dot)
        (btr-utils:kill-object 'green-dot)
        (setf pr2-proj::*debug-long-sleep-duration* 0.5)
        (setf pr2-proj::*debug-short-sleep-duration* 0.1)))))
