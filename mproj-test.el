(require 'ldots-test)
(require 'mproj)

(·deftest mproj--list-directory
    (·testing "get a clean dir listing, skip gremlins")
  (·expect 0 (length
              (mproj--list-directory "test/bad-grp")))
  (·testing "correct list count is received")
  (·expect 2 (length
              (mproj--list-directory "test/projs-grp-1"))))

(·deftest mproj--process-top-dirs-test
    (·expect 4 (length
                (mproj--find-projects-in
                 '("test/projs-grp-1" "test/projs-grp-2")))))

(·deftest project-registration-1
    (·testing "if project get's in the store"
      (·expect 1 (length
                  (mproj--register
                   nil
                   (mproj--make-project nil nil nil)))))
  (·testing "project lookup"
    (let* ((p (mproj--make-project "proj1"
                                   "projs"
                                   "/projs/proj1"))
           (k (mproj--gen-key p)))
      (·is-not (null
                (mproj--lookup (mproj--register nil p) k)))
      (·is (mproj-project-p
            (mproj--lookup (mproj--register nil p) k))))))

(·deftest project-registration-2
    (let ((proj-list (mproj--find-projects-in
                      '("test/projs-grp-1/"))))
      (·expect 2
        (length
         (loop for p in proj-list
               with st and ff
               do (setq st (mproj--register st p))
               finally return st)))))
