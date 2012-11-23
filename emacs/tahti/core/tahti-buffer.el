(require 'midnight)
(setq midnight-period (* 3600 6))       ; every 6 hours

(setq clean-buffer-list-delay-general 2           ; every 2 day
      clean-buffer-list-delay-special (* 3600 3)) ; every 3 hours

(provide 'tahti-buffer)

