;(require 'midnight)
(custom-set-variables
 '(clean-buffer-list-delay-special (* 3600 3))
 '(clean-buffer-list-delay-general 2) ;every 2 day
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "bbdb" "*RE-Builder*" "*Shell Command Output*" "*ESS*" "*WoMan-Log*" "*magit-process*" "*Dired log*" "*anything*" "*CEDET Global*" "*Pp Eval Output*" "*Completions*")))
 '(clean-buffer-list-kill-regexps (quote ("\\`\\*Customize Group:" "\\`\\*Man " "\\`\\*magit" "\\`\\*RNC Input")))
 '(midnight-mode t nil (midnight))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
)
;(setq midnight-period (* 3600 6))       ; every 6 hours


(provide 'tahti-buffer)

