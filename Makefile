.PHONY: update
update:
	git pull
	git submodule foreach git pull origin master

.PHONY: compile
compile:
	emacs --eval '(matt/recompile-settings)'

.PHONY: init
init:
	emacs --eval '(progn (matt/initialise-packages) (matt/install-my-packages))'

.PHONY: clean
clean:
	rm -rf *.elc lisp-matt/*.elc || true
	rm -rf var session.* auto-save-list/ || true

.PHONY: really_clean
really_clean: clean
	rm -rf elpa/* el-get/* || true
