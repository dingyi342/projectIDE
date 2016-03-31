# projectIDE

An Integrated development environment (IDE) like project management package. projectIDE aims to be project-oriented, allowing each project to have individual settings.


## Motivation

When I first switched to emacs, I tried almost all project management packages. They are good, but not specific.

I used to works with C++ project. When I create a C++ project in an IDE, I will already have a solid template, says, src/, src/main.cpp, inc/, lib/, build/. Why not Emacs?

When I add a C++ class, an IDE will help me add a header file (.h file) to inc/ and the source file (.cpp file) to src/. Why not Emacs? 

I need differnt compile commands for different project. Even for the same project, I need different compile commands at different states. You can always change the compile flag once in project preference of an IDE and you can compile in the normal way. Why not Emacs?

So, I write some codes to make this happen in Emacs.


## Installation

This package requires fdex https://github.com/mola-T/fdex .

If you clone from this repository directly, you need to download fdex as well.

Add this to your `.emacs` file:

``` emacs-lisp
(require 'projectIDE)
(projectIDE-initialize)
```


### Compile

;; Interactive functions:
;; projectIDE-compile
;;
;; Config file:
;; projectIDE-precompile
;; projectIDE-compile
;; projectIDE-postcompile
;; projectIDE-cleanup
