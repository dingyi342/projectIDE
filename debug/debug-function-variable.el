(projectIDE-print-variable projectIDE-runtime-Btrace)
(projectIDE-print-variable projectIDE-runtime-cache)
(projectIDE-print-variable projectIDE-runtime-record)
(projectIDE-print-variable projectIDE-runtime-modules)
(projectIDE-print-variable projectIDE-runtime-functions)

(progn "Use this to print the return value of a function."(goto-char (point-max))(insert "\n\n")(let ((beg (point)))(save-excursion (insert (pp
                                                                                                                                             
(projectIDE-get-project-list)
                                                                                                                                             
 ))(comment-region beg (point)))))

(progn "Use this to clean up this page. Just go to the end of this progn and run 'eval-last-sexp'"
       (goto-char (point-min)) (let (kill-ring) (comment-kill (count-lines (point-min) (point-max)))) (save-excursion (save-restriction (widen) (goto-char (point-max)) (delete-blank-lines) (let ((trailnewlines (abs (skip-chars-backward "\n\t")))) (if (> trailnewlines 0) (progn (delete-char trailnewlines)))))))


;; #s(hash-table size 20 test equal rehash-size 1.5 rehash-threshold 0.8 data
;;               ("cc8ba768363d1d3aff7e469607daaacf6dcb4ab1"
;;                [cl-struct-projectIDE-cache
;;                 [cl-struct-projectIDE-project "cc8ba768363d1d3aff7e469607daaacf6dcb4ab1" "projectIDE"
;;                                               ("*.idea/*" "*.eunit/*" "*.git/*" "*.hg/*" "*.fslckout/*" "*.bzr/*" "*_darcs/*" "*.tox/*" "*.svn/*" "*.stack-work/*" "cleanup/*" "*.gitignore")
;;                                               nil 31
;;                                               (init-term helloworld)
;;                                               (projectIDE-session
;;                                                ("t")
;;                                                projectIDE-cleanup
;;                                                ("*.elc" "*#*")
;;                                                projectIDE-precompile-f
;;                                                ("projectIDE-load-script" "cd" "/home/tom/.emacs.d/elpa/projectIDE/test/")
;;                                                projectIDE-compile-f
;;                                                ("projectIDE-load-script" "echo" "${file: *.el}" ">" "${root}/allel.txt")
;;                                                projectIDE-compile-cleanup-f
;;                                                ("projectIDE-load-script" "echo")
;;                                                projectIDE-testaa
;;                                                ("${folder: test/* module/*}")
;;                                                projectIDE-addfile
;;                                                ("/home/tom/.emacs.d/elpa/projectIDE/projectIDE.el::test/${name}.el1" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE.el::test/${name}.el2" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE.el::test/${name}.el3"))
;;                                               nil nil]
;;                 (22270 12353 426869 295000)
;;                 ("*.idea/*" "*.eunit/*" "*.git/*" "*.hg/*" "*.fslckout/*" "*.bzr/*" "*_darcs/*" "*.tox/*" "*.svn/*" "*.stack-work/*" "cleanup/*" "*.gitignore")
;;                 nil 2 #s(hash-table size 200 test equal rehash-size 1.5 rehash-threshold 0.8 data
;;                                     (0
;;                                      [cl-struct-fdexControl "/home/tom/.emacs.d/elpa/projectIDE/" nil nil nil "/home/tom/.emacs.d/elpa/projectIDE/.*\\.gitignore\\'\\|/home/tom/.emacs.d/elpa/projectIDE/cleanup/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.stack-work/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.svn/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.tox/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*_darcs/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.bzr/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.fslckout/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.hg/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.git/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.eunit/.*\\'\\|/home/tom/.emacs.d/elpa/projectIDE/.*\\.idea/.*\\'" nil]
;;                                      ""
;;                                      [cl-struct-fdexNode ""
;;                                                          (22270 16270 945003 743000)
;;                                                          (".projectIDE_cleanup/" "debug/" "module/" "template/" "test/")
;;                                                          (".projectIDE" "LICENSE" "projectAPI.el" "projectIDE-addfile.el" "projectIDE-cleanup.el" "projectIDE-compile.el" "projectIDE-debug.el" "projectIDE-dev.el" "projectIDE-fstream.el" "projectIDE-header.el" "projectIDE-header.elc" "projectIDE-modeline.el" "projectIDE-module.el" "projectIDE-scriptloader.el" "projectIDE-session.el" "projectIDE.el" "READEME.md" "testmodule.el" "testmodule2.el")]
;;                                      "test/"
;;                                      [cl-struct-fdexNode "test/"
;;                                                          (22270 13100 352746 986000)
;;                                                          ("test/bar/" "test/foo/")
;;                                                          ("compile-cleanup.sh" "compile.sh" "omega-old.el" "omg.el1" "omg.el2" "omg.el3" "postcompile.sh" "precompile.sh")]
;;                                      "template/"
;;                                      [cl-struct-fdexNode "template/"
;;                                                          (22266 17984 342188 911000)
;;                                                          ("template/c_executable/")
;;                                                          nil]
;;                                      "module/"
;;                                      [cl-struct-fdexNode "module/"
;;                                                          (22268 64604 264556 193000)
;;                                                          ("module/Guide/")
;;                                                          nil]
;;                                      "debug/"
;;                                      [cl-struct-fdexNode "debug/"
;;                                                          (22268 64604 263914 251000)
;;                                                          nil
;;                                                          ("debug-defun-defmacro.el" "debug-function-variable.el" "debug-testmodule.el" "reset.el")]
;;                                      "test/bar/"
;;                                      [cl-struct-fdexNode "test/bar/"
;;                                                          (22266 17984 345109 187000)
;;                                                          ("test/bar/build/" "test/bar/install/" "test/bar/lib/" "test/bar/src/")
;;                                                          (".projectIDE" "CMakeLists.txt")]
;;                                      "test/foo/"
;;                                      [cl-struct-fdexNode "test/foo/"
;;                                                          (22268 65040 395094 588000)
;;                                                          ("test/foo/build/" "test/foo/install/" "test/foo/lib/" "test/foo/src/")
;;                                                          (".projectIDE" "CMakeLists.txt")]
;;                                      "template/c_executable/"
;;                                      [cl-struct-fdexNode "template/c_executable/"
;;                                                          (22268 64238 394227 446000)
;;                                                          ("template/c_executable/build/" "template/c_executable/install/" "template/c_executable/lib/" "template/c_executable/src/")
;;                                                          ("CMakeLists.txt")]
;;                                      "module/Guide/"
;;                                      [cl-struct-fdexNode "module/Guide/"
;;                                                          (22268 64243 412446 463000)
;;                                                          nil
;;                                                          ("helloworld.el" "init-term.el")]
;;                                      "test/bar/build/"
;;                                      [cl-struct-fdexNode "test/bar/build/"
;;                                                          (22266 17984 349168 494000)
;;                                                          nil nil]
;;                                      "test/bar/src/"
;;                                      [cl-struct-fdexNode "test/bar/src/"
;;                                                          (22266 17984 349613 928000)
;;                                                          nil nil]
;;                                      "test/bar/lib/"
;;                                      [cl-struct-fdexNode "test/bar/lib/"
;;                                                          (22266 17984 350050 70000)
;;                                                          nil nil]
;;                                      "test/bar/install/"
;;                                      [cl-struct-fdexNode "test/bar/install/"
;;                                                          (22266 17984 350491 405000)
;;                                                          nil nil]
;;                                      "test/foo/build/"
;;                                      [cl-struct-fdexNode "test/foo/build/"
;;                                                          (22266 17984 350929 547000)
;;                                                          nil nil]
;;                                      "test/foo/src/"
;;                                      [cl-struct-fdexNode "test/foo/src/"
;;                                                          (22268 65060 16467 966000)
;;                                                          nil nil]
;;                                      "test/foo/lib/"
;;                                      [cl-struct-fdexNode "test/foo/lib/"
;;                                                          (22266 17984 351505 499000)
;;                                                          nil nil]
;;                                      "test/foo/install/"
;;                                      [cl-struct-fdexNode "test/foo/install/"
;;                                                          (22266 17984 351636 577000)
;;                                                          nil nil]
;;                                      "template/c_executable/build/"
;;                                      [cl-struct-fdexNode "template/c_executable/build/"
;;                                                          (22266 17984 354083 738000)
;;                                                          nil nil]
;;                                      "template/c_executable/src/"
;;                                      [cl-struct-fdexNode "template/c_executable/src/"
;;                                                          (22266 17984 354260 274000)
;;                                                          nil nil]
;;                                      "template/c_executable/lib/"
;;                                      [cl-struct-fdexNode "template/c_executable/lib/"
;;                                                          (22266 17984 354409 60000)
;;                                                          nil nil]
;;                                      "template/c_executable/install/"
;;                                      [cl-struct-fdexNode "template/c_executable/install/"
;;                                                          (22266 17984 354556 724000)
;;                                                          nil nil]
;;                                      ".projectIDE_cleanup/20160331_192639/"
;;                                      [cl-struct-fdexNode ".projectIDE_cleanup/20160331_192639/"
;;                                                          (22269 2456 555557 871000)
;;                                                          nil
;;                                                          ("projectIDE-fstream.elc")]
;;                                      ".projectIDE_cleanup/"
;;                                      [cl-struct-fdexNode ".projectIDE_cleanup/"
;;                                                          (22269 2456 554752 360000)
;;                                                          (".projectIDE_cleanup/20160331_192639/")
;;                                                          nil]
;;                                      1
;;                                      (".projectIDE" "LICENSE" "projectAPI.el" "projectIDE-addfile.el" "projectIDE-cleanup.el" "projectIDE-compile.el" "projectIDE-debug.el" "projectIDE-dev.el" "projectIDE-fstream.el" "projectIDE-header.el" "projectIDE-header.elc" "projectIDE-modeline.el" "projectIDE-module.el" "projectIDE-scriptloader.el" "projectIDE-session.el" "projectIDE.el" "READEME.md" "testmodule.el" "testmodule2.el" ".projectIDE_cleanup/20160331_192639/projectIDE-fstream.elc" "debug/debug-defun-defmacro.el" "debug/debug-function-variable.el" "debug/debug-testmodule.el" "debug/reset.el" "module/Guide/helloworld.el" "module/Guide/init-term.el" "template/c_executable/CMakeLists.txt" "test/compile-cleanup.sh" "test/compile.sh" "test/omega-old.el" "test/omg.el1" "test/omg.el2" "test/omg.el3" "test/postcompile.sh" "test/precompile.sh" "test/bar/.projectIDE" "test/bar/CMakeLists.txt" "test/foo/.projectIDE" "test/foo/CMakeLists.txt")))
;;                 ("/home/tom/.emacs.d/elpa/projectIDE/debug/debug-function-variable.el" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE-header.el" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE-addfile.el" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE.el" "/home/tom/.emacs.d/elpa/projectIDE/projectIDE-compile.el")
;;                 nil nil nil]
;;                "cc8ba768363d1d3aff7e469607daaacf6dcb4ab1modulenonpersist" #s(hash-table size 100 test eql rehash-size 1.5 rehash-threshold 0.8 data
;;                                                                                         ())
;;                "cc8ba768363d1d3aff7e469607daaacf6dcb4ab1modulepersist" #s(hash-table size 100 test eql rehash-size 1.5 rehash-threshold 0.8 data
;;                                                                                      ())
;;                "cc8ba768363d1d3aff7e469607daaacf6dcb4ab1association" #s(hash-table size 30 test equal rehash-size 1.5 rehash-threshold 0.8 data
;;                                                                                    ())))
