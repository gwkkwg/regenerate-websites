(in-package #:rw)

(defun smallest-system-set (system)
  (let ((all-dependencies (dsc:collect-system-dependencies system)))
    (loop for unneeded =
          (find-if 
           (lambda (sub-system)
             (some (lambda (other-sub-system)
                     (and (not (eq sub-system other-sub-system))
                          (member sub-system 
                                  (dsc:collect-system-dependencies other-sub-system))))
                   all-dependencies))
           all-dependencies)
          while unneeded do
          (print unneeded)
          (print all-dependencies)
          (setf all-dependencies (remove unneeded all-dependencies)))
    all-dependencies))

#+Ignore
(defun make-systems-graph (system)
  (let ((g (make-container 'cl-graph:graph-container 
                           :default-edge-type :directed)))
    (labels ((add-edges (system)
               (dsc:map-system-dependencies 
                system
                (lambda (sub-system)
                  (cl-graph:add-edge-between-vertexes 
                   g system sub-system)
                  (add-edges sub-system)))))
      (add-edges system))
    g))

#+Example
(smallest-system-set 'tinaa)
                          
(defun generate-darcs-commands (project)
  (let ((result nil))
    (html
     (:pre
      (mapc 
       (lambda (system)
         (let ((system (intern (string-upcase (symbol-name system)) :keyword)))
           (if (find-system system)
             (html (lml-format "~%darcs get \"~A\"" (repository (find-system system))))
             (push system result))))
         (append (list project) (smallest-system-set project))))
     (when result
       (flet ((make-project-link (project)
                (html ((:a :href (format nil "http://www.cliki.net/~(~A~)" project))
                       (lml-princ project)))))
         (html
          (:p (lml-princ project) " also requires the following other ASDF-Installable projects: "
              (cond ((= (length result) 1)
                     (make-project-link (first result)))
                    ((= (length result) 2)
                     (make-project-link (first result)) 
                     (html " and ")
                     (make-project-link (second result)))
                    (t 
                     (loop for i from 0 to (- (length result) 2) do
                           (make-project-link (nth i result))
                           (html ", "))
                     (html " and ")
                     (make-project-link (first (last result)))))
              (html "."))))))
    (values result)))

#+Example
(generate-darcs-commands 'metatilities)
   
