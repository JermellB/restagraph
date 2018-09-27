sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :restagraph)" \
    --eval "(sb-ext:save-lisp-and-die \"restagraph\" :executable t :toplevel #'restagraph::dockerstart)"
