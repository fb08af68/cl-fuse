LISP=sbcl
FLAGS=--non-interactive --load

all:
	$(LISP) $(FLAGS) launch.lisp
