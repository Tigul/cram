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

(defgeneric extract-task-error (err)
  (:method ((err cpl:plan-failure))
    err)
  (:method ((err cpl:common-lisp-error-envelope))
    (cpl:envelop-error err)))

(def-fact-group tasks (coe:holds)

  ;; top-level
  (<- (top-level-task ?top-level-name ?top-level-task-node)
    (bound ?top-level-name)
    (lisp-fun cpl:get-top-level-task-tree ?top-level-name ?top-level-task-node))

  (<- (top-level-episode-knowledge ?top-level-name ?top-level-episode)
    (bound ?top-level-name)
    (lisp-fun cet:get-top-level-episode-knowledge ?top-level-name ?top-level-episode))

  ;; util
  (<- (task-full-path ?task-node ?path)
    (bound ?task-node)
    (lisp-fun cpl:task-tree-node-path ?task-node ?path))

  ;; tasks of top-level
  (<- (task-of-top-level ?top-level-name ?task-node)
    (bound ?top-level-name)
    (top-level-task ?top-level-name ?top-level-task-node)
    (lisp-fun cpl:flatten-task-tree ?top-level-task-node ?all-task-nodes)
    (member ?task-node ?all-task-nodes))

  ;; task for subtree
  (<- (task ?top-level-name ?subtree-path ?task-node)
    (bound ?top-level-name)
    (bound ?subtree-path)
    (top-level-task ?top-level-name ?top-level-task)
    (lisp-fun cpl:task-tree-node ?subtree-path ?top-level-task ?subtree-task)
    (lisp-fun cpl:flatten-task-tree ?subtree-task ?all-subtree-tasks)
    (member ?task-node ?all-subtree-tasks))

  ;; subtask
  (<- (subtask ?task ?subtask)
    (bound ?task)
    (lisp-fun cpl:task-tree-node-children ?task ?children)
    (member (?_ . ?subtask) ?children))

  (<- (subtask ?task ?subtask)
    (not (bound ?task))
    (bound ?subtask)
    (lisp-fun cpl:task-tree-node-parent ?subtask ?task)
    (lisp-pred identity ?task))

  ;; (<- (subtask ?task ?subtask)
  ;;   (not (bound ?task))
  ;;   (not (bound ?subtask))
  ;;   (task ?task)
  ;;   (subtask ?task ?subtask))

  ;; subtask+
  (<- (subtask+ ?task ?subtask)
    (subtask ?task ?subtask))

  (<- (subtask+ ?task ?subtask)
    (subtask ?task ?tmp)
    (subtask+ ?tmp ?subtask))

  ;; task-sibling
  (<- (task-sibling ?task ?sibling)
    (bound ?task)
    (subtask ?parent ?task)
    (subtask ?parent ?sibling)
    (not (== ?sibling ?task)))

  ;; (<- (task-sibling ?task ?sibling)
  ;;   (not (bound ?task))
  ;;   (subtask ?parent ?sibling)
  ;;   (subtask ?parent ?task)
  ;;   (not (== ?sibling ?task)))

  ;; task-result
  (<- (task-result ?task ?result)
    (bound ?task)
    (lisp-fun cpl:task-tree-node-result ?task ?result))

  ;; task-parameter
  (<- (task-parameter ?task ?parameter)
    (bound ?task)
    (lisp-fun cpl:task-tree-node-parameters ?task ?parameters)
    (member ?parameter ?parameters))

  ;; task-status-fluent
  (<- (task-status-fluent ?task ?fluent)
    (bound ?task)
    (lisp-fun cpl:task-tree-node-status-fluent ?task ?fluent))

  (<- (fluent-value ?fluent ?value)
    (bound ?fluent)
    (not (equal ?fluent NIL))
    (lisp-fun cpl:value ?fluent ?value))

  (<- (task-status ?task ?status)
    (bound ?task)
    (task-status-fluent ?task ?fluent)
    (fluent-value ?fluent ?status))

  (<- (task-outcome ?task ?outcome)
    (bound ?task)
    (member ?outcome (:succeeded :failed :evaporated))
    (task-status ?task ?outcome))

  (<- (task-error ?task ?error)
    (bound ?task)
    (task-outcome ?task :failed)
    (task-result ?task ?result)
    (lisp-fun extract-task-error ?result ?error))

  ;; execution trace related
  (<- (coe:holds (fluent-value ?fluent ?value) ?top-level-name ?time)
    (bound ?fluent)
    (bound ?top-level-name)
    (top-level-episode-knowledge ?top-level-name ?episode)
    (lisp-pred identity ?fluent)
    (lisp-fun cpl-impl:name ?fluent ?fluent-name)
    (lisp-fun cet:episode-knowledge-fluent-durations ?fluent-name ?episode ?durations)
    (member (?value . ?duration) ?durations)
    (cram-occasions-events:duration-includes ?duration ?time))

  (<- (coe:holds (task-status ?task ?status) ?top-level-name ?time)
    (bound ?top-level-name)
    (task-status-fluent ?task ?status-fluent)
    (coe:holds (fluent-value ?status-fluent ?status) ?top-level-name ?time))

  ;; task times
  (<- (task-created-at ?top-level-name ?task ?time)
    (bound ?top-level-name)
    (bound ?task)
    (coe:holds (task-status ?task :created) ?top-level-name (coe:at ?time)))

  (<- (task-started-at ?top-level-name ?task ?time)
    (bound ?top-level-name)
    (bound ?task)
    ;; (task ?task)
    (bagof ?time
           (coe:holds (task-status ?task :running) ?top-level-name (coe:at ?time))
           ?times)
    (sort ?times < (?time . ?_)))

  (<- (task-ended-at ?top-level-name ?task ?time)
    (bound ?top-level-name)
    (bound ?task)
    ;; (task ?task)
    (member ?status (:succeeded :failed :evaporated))
    (coe:holds (task-status ?task ?status) ?top-level-name (coe:at ?time)))

  ;; task next and previous sibling
  (<- (task-next-sibling ?top-level-name ?task ?next-task)
    (bound ?top-level-name)
    (bound ?task)
    (bagof ?sibling (task-sibling ?task ?sibling) ?siblings)
    (member ?next-task ?siblings)
    (task-created-at ?top-level-name ?task ?created-time-task)
    (task-created-at ?top-level-name ?next-task ?created-time-next-task)
    (<= ?created-time-task ?created-time-next-task)
    (forall (and
             (member ?other-next-task ?siblings)
             (not (== ?next-task ?other-next-task))
             (task-created-at ?top-level-name ?other-next-task ?created-time-other-next-task)
             (<= ?created-time-task ?created-time-other-next-task))
            (<= ?created-time-next-task ?created-time-other-next-task)))

  (<- (task-previous-sibling ?top-level-name ?task ?next-task)
    (bound ?top-level-name)
    (bound ?task)
    (bagof ?sibling (task-sibling ?task ?sibling) ?siblings)
    (member ?next-task ?siblings)
    (task-created-at ?top-level-name ?task ?created-time-task)
    (task-created-at ?top-level-name ?next-task ?created-time-next-task)
    (>= ?created-time-task ?created-time-next-task)
    (forall (and
             (member ?other-next-task ?siblings)
             (not (== ?next-task ?other-next-task))
             (task-created-at ?top-level-name ?other-next-task ?created-time-other-next-task)
             (>= ?created-time-task ?created-time-other-next-task))
            (>= ?created-time-next-task ?created-time-other-next-task)))

  ;; perform tasks
  (<- (perform-task-of-top-level ?top-level-name ?task-node)
    (bound ?top-level-name)
    (task-of-top-level ?top-level-name ?task-node)
    (lisp-fun cpl:task-tree-node-path ?task-node (?path . ?_))
    (equal ?path (cpl:goal (perform ?_) . ?_)))

  (<- (perform-task ?top-level-name ?subtree-path ?task-node)
    (bound ?top-level-name)
    (bound ?subtree-path)
    (task ?top-level-name ?subtree-path ?task-node)
    (lisp-fun cpl:task-tree-node-path ?task-node (?path . ?_))
    (equal ?path (cpl:goal (perform ?_) . ?_)))

  (<- (task-specific-action ?top-level-name ?subtree-path ?action-type ?task ?designator)
    (bound ?top-level-name)
    (bound ?subtree-path)
    (perform-task ?top-level-name ?subtree-path ?task)
    (task-parameter ?task ?designator)
    (lisp-type ?designator desig:action-designator)
    (desig:desig-prop ?designator (:type ?action-type)))

  (<- (task-navigating-action ?top-level-name ?subtree-path ?task ?designator)
    (task-specific-action ?top-level-name ?subtree-path :navigating ?task ?designator))

  (<- (task-fetching-action ?top-level-name ?subtree-path ?task ?designator)
    (task-specific-action ?top-level-name ?subtree-path :fetching ?task ?designator))

  (<- (task-picking-up-action ?top-level-name ?subtree-path ?task ?designator)
    (task-specific-action ?top-level-name ?subtree-path :picking-up ?task ?designator))

  (<- (task-delivering-action ?top-level-name ?subtree-path ?task ?designator)
    (task-specific-action ?top-level-name ?subtree-path :delivering ?task ?designator))

    ;; task next and previous perform action sibling
  (<- (task-next-action-sibling ?top-level-name ?subtree-path ?task ?action-type ?next-task)
    (bound ?top-level-name)
    (bound ?subtree-path)
    (bound ?task)
    ;; (bound ?action-type)
    (bagof ?sibling (task-sibling ?task ?sibling) ?siblings)
    (member ?next-task ?siblings)
    (task-specific-action ?top-level-name ?subtree-path ?action-type ?next-task ?_)
    (task-created-at ?top-level-name ?task ?created-time-task)
    (task-created-at ?top-level-name ?next-task ?created-time-next-task)
    (<= ?created-time-task ?created-time-next-task)
    (forall (and
             (member ?other-next-task ?siblings)
             (task-specific-action ?top-level-name ?subtree-path ?action-type ?other-next-task ?_)
             (not (== ?next-task ?other-next-task))
             (task-created-at ?top-level-name ?other-next-task ?created-time-other-next-task)
             (<= ?created-time-task ?created-time-other-next-task))
            (<= ?created-time-next-task ?created-time-other-next-task)))

  (<- (task-previous-action-sibling ?top-level-name ?subtree-path ?task ?action-type ?next-task)
    (bound ?top-level-name)
    (bound ?subtree-path)
    (bound ?task)
    ;; (bound ?action-type)
    (bagof ?sibling (task-sibling ?task ?sibling) ?siblings)
    (member ?next-task ?siblings)
    (task-specific-action ?top-level-name ?subtree-path ?action-type ?next-task ?_)
    (task-created-at ?top-level-name ?task ?created-time-task)
    (task-created-at ?top-level-name ?next-task ?created-time-next-task)
    (>= ?created-time-task ?created-time-next-task)
    (forall (and
             (member ?other-next-task ?siblings)
             (not (== ?next-task ?other-next-task))
             (task-specific-action ?top-level-name ?subtree-path ?action-type ?other-next-task ?_)
             (task-created-at ?top-level-name ?other-next-task ?created-time-other-next-task)
             (>= ?created-time-task ?created-time-other-next-task))
            (>= ?created-time-next-task ?created-time-other-next-task))))


(defun test ()
  (cet:enable-fluent-tracing)
  (cpl-impl::remove-top-level-task-tree :top-level)

  (pr2-proj:with-simulated-robot
    (demo-random nil))

  (cut:force-ll (prolog:prolog `(task-navigating-action :top-level ((demo-random))
                                                        ?task ?designator))))

(defun test-next-sibling-time ()
  (cet:enable-fluent-tracing)
  (cpl-impl::remove-top-level-task-tree :top-level)

  (pr2-proj:with-simulated-robot
    (demo-random nil))

  (cut:force-ll
   (prolog:prolog `(and (task-navigating-action :top-level ((demo-random)) ?task ?des)
                        (task-next-sibling :top-level ?task ?next-task)
                        (task-created-at :top-level ?task ?created)
                        (task-created-at :top-level ?next-task ?next-created)
                        (format "time: ~a   time next: ~a~%" ?created ?next-created)))))

(defun test-failed-actions ()
  (cet:enable-fluent-tracing)
  (cpl-impl::remove-top-level-task-tree :top-level)

  (pr2-proj:with-simulated-robot
    (demo-random))

  (cut:force-ll
   (prolog:prolog `(and (task-specific-action :top-level ((demo-random)) :fetching ?task ?desig)
                        (task-outcome ?task :failed)
                        (format "desig: ~a~%" ?desig)))))

(defun find-location-for-pick-up-using-occasions ()
  (cet:enable-fluent-tracing)
  (cpl-impl::remove-top-level-task-tree :top-level)

  (proj:with-projection-environment pr2-proj:pr2-bullet-projection-environment
    (cpl-impl::named-top-level (:name :top-level)
      (demo-random nil))

    (let ((top-level-name :top-level))
      (cut:var-value
       '?pick-location
       (car
        (prolog:prolog
         `(and (task-fetching-action ,top-level-name ((demo-random)) ?fetching-task ?_)
               (task-full-path ?fetching-task ?fetching-path)
               (task-picking-up-action ,top-level-name ?fetching-path ?picking-up-task ?_)
               (task-outcome ?picking-up-task :succeeded)
               (task-started-at ,top-level-name ?picking-up-task ?picking-up-start)
               (cram-robot-interfaces:robot ?robot)
               (btr:timeline ?timeline)
               (coe:holds ?timeline (cpoe:loc ?robot ?pick-location)
                          (coe:at ?picking-up-start)))))))))

(defun find-location-for-pick-up (&optional (projection-run-count 5))
  (flet ((get-pick-up-location (top-level-name path)
           (let* ((bindings
                    (car
                     (prolog:prolog
                      `(and
                        (task-fetching-action ,top-level-name ,path
                                              ?fetching-task ?_)
                        (task-full-path ?fetching-task ?fetching-path)
                        (task-picking-up-action ,top-level-name ?fetching-path
                                                ?picking-up-task ?picking-up-designator)
                        (task-outcome ?picking-up-task :succeeded)
                        (task-previous-action-sibling ,top-level-name ?fetching-path
                                                      ?picking-up-task
                                                      :navigating ?navigating-task)
                        (task-navigating-action ,top-level-name ?fetching-path ?navigating-task
                                                ?navigating-designator)))))
                  (picking-action
                    (cut:var-value '?picking-up-designator bindings))
                  (picking-action-newest
                    (unless (cut:is-var picking-action)
                      (desig:newest-effective-designator picking-action)))
                  (picking-arm
                    (when picking-action-newest
                      (third (desig:reference picking-action-newest))))
                  (navigating-action
                    (cut:var-value '?navigating-designator bindings))
                  (navigating-action-newest
                    (unless (cut:is-var navigating-action)
                      (desig:newest-effective-designator navigating-action)))
                  (picking-location
                    (when navigating-action
                      (desig:newest-effective-designator
                       (desig:desig-prop-value navigating-action :location)))))
             (list picking-location picking-arm))))

    (cet:enable-fluent-tracing)
    (cpl-impl::remove-top-level-task-tree :top-level)

    (let (paths)
      (proj:with-projection-environment pr2-proj:pr2-bullet-projection-environment
        (cpl-impl::named-top-level (:name :top-level)
          (dotimes (n projection-run-count)
            (push (demo-random nil) paths))))

      (mapcar (alexandria:curry #'get-pick-up-location :top-level)
              paths))))



(defun extract-successful-transporting-parameters (top-level-name path)
  (let* ((bindings
           (car
            (prolog:prolog
             `(and
               ;; find successful picking-up action
               (task-specific-action ,top-level-name ,path :transporting
                                     ?transporting-task ?_)
               (task-outcome ?transporting-task :succeeded)
               (task-full-path ?transporting-task ?transporting-path)
               (task-specific-action ,top-level-name ?transporting-path :fetching
                                     ?fetching-task ?_)
               (task-full-path ?fetching-task ?fetching-path)
               (task-specific-action ,top-level-name ?fetching-path :picking-up
                                     ?picking-up-task ?picking-up-designator)
               (task-outcome ?picking-up-task :succeeded)
               ;; find closest navigation action before pick-up
               (task-previous-action-sibling ,top-level-name ?fetching-path
                                             ?picking-up-task
                                             :navigating ?picking-navigating-task)
               (task-specific-action ,top-level-name ?fetching-path :navigating
                                     ?picking-navigating-task
                                     ?picking-navigating-designator)
               ;; make sure that the corresponding delivering action succeeded
               (task-specific-action ,top-level-name ?transporting-path :delivering
                                     ?delivering-task ?_)
               (task-outcome ?delivering-task :succeeded)
               ;; find closest navigation action before place
               (task-full-path ?delivering-task ?delivering-path)
               (task-specific-action ,top-level-name ?delivering-path :placing
                                     ?placing-task ?placing-designator)
               (task-outcome ?placing-task :succeeded)
               (task-previous-action-sibling ,top-level-name ?delivering-path
                                             ?placing-task
                                             :navigating ?placing-navigating-task)
               (task-specific-action ,top-level-name ?delivering-path :navigating
                                     ?placing-navigating-task
                                     ?placing-navigating-designator)
               ;; ;; calculate navigation distances
               ;; (btr:timeline ?timeline)
               ;; (bagof ?distance
               ;;        (and
               ;;         (task-specific-action ,top-level-name ?transporting-task
               ;;                               :navigating ?navigating-task ?_)
               ;;         (task-started-at ,top-level-name ?navigating-task ?start-time)
               ;;         (coe:holds ?timeline (cpoe:loc ?robot ?start-location)
               ;;                    (coe:at ?start-time))
               ;;         (task-ended-at ,top-level-name ?navigating-task ?end-time)
               ;;         (coe:holds ?timeline (cpoe:loc ?robot ?end-location)
               ;;                    (coe:at ?end-time))))
               ))))
         (picking-action
           (cut:var-value '?picking-up-designator bindings))
         (picking-action-newest
           (unless (cut:is-var picking-action)
             (desig:newest-effective-designator picking-action)))
         (picking-arm
           (when picking-action-newest
             (third (desig:reference picking-action-newest))))
         (picking-grasp
           (when picking-action-newest
             (sixth (desig:reference picking-action-newest))))
         (picking-navigating-action
           (cut:var-value '?picking-navigating-designator bindings))
         (picking-navigating-action-newest
           (unless (cut:is-var picking-navigating-action)
             (desig:newest-effective-designator picking-navigating-action)))
         (picking-location
           (when picking-navigating-action-newest
             (desig:reference
              (desig:newest-effective-designator
               (desig:desig-prop-value picking-navigating-action-newest :location)))))
         (placing-navigating-action
           (cut:var-value '?placing-navigating-designator bindings))
         (placing-navigating-action-newest
           (unless (cut:is-var placing-navigating-action)
             (desig:newest-effective-designator placing-navigating-action)))
         (placing-location
           (when placing-navigating-action-newest
             (desig:reference
              (desig:newest-effective-designator
               (desig:desig-prop-value placing-navigating-action-newest :location))))))
    (list picking-location picking-arm picking-grasp placing-location)))

(defun estimate-distance-between-pose-stamped (pose-stamped-1 pose-stamped-2
                                               &optional
                                                 (translational-weight 1.0)
                                                 (rotational-weight 1.0))
  (assert pose-stamped-1)
  (assert pose-stamped-2)
  (assert (string-equal (cl-transforms-stamped:frame-id pose-stamped-1)
                        (cl-transforms-stamped:frame-id pose-stamped-2)))
  (+ (* translational-weight
        (cl-transforms:v-dist
         (cl-transforms:origin pose-stamped-1)
         (cl-transforms:origin pose-stamped-2)))
     (* rotational-weight
        (abs
         (cl-transforms:normalize-angle
          (cl-transforms:angle-between-quaternions
           (cl-transforms:orientation pose-stamped-1)
           (cl-transforms:orientation pose-stamped-2)))))))

(defun estimate-distances-starting-from-current-pose (current-pose-stamped list-of-pose-stampeds)
  (unless (some #'null list-of-pose-stampeds)
    (reduce #'+
            (maplist (lambda (sublist)
                       (if (>= (length sublist) 2)
                           (estimate-distance-between-pose-stamped
                            (first sublist)
                            (second sublist))
                           0.0))
                     (cons current-pose-stamped list-of-pose-stampeds)))))

(defun calculate-index-of-shortest-route (current-pose-stamped lists-of-pose-stampeds)
  (let* ((distances
           (mapcar (alexandria:curry #'estimate-distances-starting-from-current-pose
                                     current-pose-stamped)
                   lists-of-pose-stampeds))
         (min-distances
           (reduce (lambda (x y)
                     (if x
                         (if y
                             (min x y)
                             x)
                         (if y
                             y
                             NIL)))
                   distances)))
    (values (position min-distances distances)
            min-distances)))

(defun find-successful-transporting-parameters (paths)
  (let* ((parameter-lists
           (mapcar (alexandria:curry #'extract-successful-transporting-parameters :top-level)
                   paths))
         (parameter-lists-only-poses
           (mapcar (lambda (set-of-params)
                     (list (first set-of-params)
                           (fourth set-of-params)))
                   parameter-lists))
         (current-robot-pose
           ;; todo: actually should use cram-tf:robot-current-pose
           (cl-transforms-stamped:pose->pose-stamped
            cram-tf:*fixed-frame*
            0.0
            (btr:pose (btr:get-robot-object))))
         (best-parameter-list-index
           (calculate-index-of-shortest-route
            current-robot-pose
            parameter-lists-only-poses))
         (best-parameters
           (nth best-parameter-list-index parameter-lists)))
    (print parameter-lists)
    best-parameters))

#+belowfunctioniscommentedout
(cpl:def-cram-function demo-random (&optional
                                    (random t)
                                    (list-of-objects '(:breakfast-cereal :milk :cup :bowl :spoon)))
  (btr:detach-all-objects (btr:get-robot-object))
  (btr-utils:kill-all-objects)

  (when (eql cram-projection:*projection-environment*
             'cram-pr2-projection::pr2-bullet-projection-environment)
    (if random
        (spawn-objects-on-sink-counter-randomly)
        (spawn-objects-on-sink-counter)))

  (setf cram-robot-pose-guassian-costmap::*orientation-samples* 1)

  (initialize-or-finalize)

  (dolist (?object-type list-of-objects)
    (let* ((?cad-model
             (cdr (assoc ?object-type *object-cad-models*)))
           (?object-to-fetch
             (desig:an object
                       (type ?object-type)
                       (desig:when ?cad-model
                         (cad-model ?cad-model))))
           (?fetching-location
             (desig:a location
                      (on "CounterTop")
                      (name "iai_kitchen_sink_area_counter_top")
                      (side left)))
           (?placing-target-pose
             (cl-transforms-stamped:pose->pose-stamped
              "map" 0.0
              (cram-bullet-reasoning:ensure-pose
               (cdr (assoc ?object-type *object-placing-poses*)))))
           (?arm-to-use
             (cdr (assoc ?object-type *object-grasping-arms*)))
           (?delivering-location
             (desig:a location
                      (pose ?placing-target-pose))))

      (cpl:with-failure-handling
          ((common-fail:high-level-failure (e)
             (roslisp:ros-warn (pp-plans demo) "Failure happened: ~a~%Skipping..." e)
             (return)))
        (exe:perform
         (desig:an action
                   (type transporting)
                   (object ?object-to-fetch)
                   ;; (arm ?arm-to-use)
                   (location ?fetching-location)
                   (target ?delivering-location))))))

  (initialize-or-finalize)

  cpl:*current-path*)


(defun main-experiment ()
  (cet:enable-fluent-tracing)
  (cpl-impl::remove-top-level-task-tree :top-level)

  (let (paths)
    (proj:with-projection-environment pr2-proj:pr2-bullet-projection-environment
      (cpl-impl::named-top-level (:name :top-level)
        (dotimes (n 3)
          (push (demo-random nil '(:milk)) paths))))

    (find-successful-transporting-parameters paths)))
