;; The 'nil' configuration applies to all modes.
((scheme-mode . ((indent-tabs-mode . nil)
		 (tab-width . 2)
		 (eval . (progn
                           ;; okvs
                           (put 'with-output-language 'scheme-indent-function 1)
                           (put 'nanopass-case 'scheme-indent-function 2)
                           (put 'okvs-in-transaction 'scheme-indent-function 2)
                           (put 'engine-in-transaction 'scheme-indent-function 2)
			   ;; scheme
			   (put 'switch 'scheme-indent-function 1)
			   (put 'call-with-input-string 'scheme-indent-function 1)
			   (put 'call-with-values 'scheme-indent-function 1)
			   (put 'search-address-info 'scheme-indent-function 3)
			   (put 'call-with-lock 'scheme-indent-function 1)
			   (put 'call-with-port 'scheme-indent-function 1)
			   (put 'with-cursor 'scheme-indent-function 1)
			   (put 'with-directory 'scheme-indent-function 1)
			   (put 'with-env 'scheme-indent-function 1)
			   (put 'with-context 'scheme-indent-function 1)
			   (put 'match 'scheme-indent-function 1)
			   (put 'match-node 'scheme-indent-function 1)
			   (put 'match-tree 'scheme-indent-function 1))))))
