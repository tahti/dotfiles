;;; tahti-dark-them.el --- Molokai with solarized as fallback for Emacs.


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of tahti to Emacs.
;;
;;; Installation:
;;
;; Drop the `tahti-dark-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code
(deftheme tahti-dark "The dark variant of the molokai based on solarized colour theme customized")

(defun create-tahti-theme (variant theme-name &optional childtheme)
  (let* ((class '((class color) (min-colors 89)))
         ;; primary content
         (tahti-fg        "#D0D0D0")
         (tahti-bg        "#080808")
         (tahti-hl        "#403d3d")
         ;; comments
         (tahti-comments  "#5f5f5f")
         ;; emphasized content
         (tahti-emph      "#93a1a1")

         ;; tahti accented colors
         (yellow    "#afaf5f")
         (orange    "#fd971f")
         (red       "#d7005f")
         (magenta   "#d33682")
         (violet    "#6c71c4")
         (blue      "#268bd2")
         (cyan      "#2aa198")
         (green     "#859900")
         (chartreusel "#A6E22E")
         (grey      "#8F8F8F")
         (purple    "#Ae81ff")
         (black     "#000000")

         ;; Darker and lighter accented colors
         ;; Only use these in exceptional circumstances!
         (yellow-lc  "#7B6000")
         (yellow-l   "#DEB542")
         (orange-lc  "#AA501A")
         (orange-hc  "#F2804F")
         (red-lc     "#960050")
         (red-hc     "#FF6E64")
         (magenta-lc "#93115C")
         (magenta-hc "#F771AC")
         (violet-lc  "#3F4D91")
         (violet-hc  "#9EA0E5")
         (blue-lc    "#00629D")
         (blue-hc    "#5fd7ff")
         (cyan-lc    "#00736F")
         (cyan-hc    "#69CABF")
         (green-lc   "#546E00")
         (green-hc   "#A6E22E")
         (purple-lc  "#AF87D7"))

    (custom-theme-set-faces
     theme-name
     '(button ((t (:underline t))))

     ;; basic coloring
     `(default ((,class (:foreground ,tahti-fg :background ,tahti-bg))))
     `(shadow ((,class (:foreground ,tahti-comments))))
     `(match ((,class (:background ,tahti-hl :foreground ,tahti-emph :weight bold))))
     `(cursor ((,class (:foreground ,tahti-bg :background ,tahti-fg :inverse-video t))))
     `(escape-glyph-face ((,class (:foreground ,yellow))))
     `(fringe ((,class (:foreground ,tahti-fg :background ,tahti-hl))))
     `(header-line ((,class (:foreground ,yellow
                                         :background ,tahti-hl
                                         :box (:line-width -1 :style released-button)))))
     `(highlight ((,class (:background ,tahti-hl))))
     `(link ((,class (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
     `(success ((,class (:foreground ,green ))))
     `(warning ((,class (:foreground ,orange-lc))))
     `(error ((,class (:foreground ,red-lc))))
     `(lazy-highlight ((,class (:foreground ,tahti-emph :background ,tahti-hl :bold t))))
     `(escape-glyph ((,class (:foreground ,violet))))

     ;;column-markers
     `(tahti-column-marker-1-face ((,class (:foreground ,orange-lc))))
     `(tahti-column-marker-2-face ((,class (:foreground ,red-lc))))

     ;; compilation
     `(compilation-column-face ((,class (:foreground ,yellow))))
     `(compilation-enter-directory-face ((,class (:foreground ,green))))
     `(compilation-error-face ((,class (:foreground ,red-lc :weight bold :underline t))))
     `(compilation-face ((,class (:foreground ,tahti-fg))))
     `(compilation-info-face ((,class (:foreground ,blue))))
     `(compilation-info ((,class (:foreground ,green :underline t))))
     `(compilation-leave-directory-face ((,class (:foreground ,green))))
     `(compilation-line-face ((,class (:foreground ,yellow))))
     `(compilation-line-number ((,class (:foreground ,yellow))))
     `(compilation-mode-line-exit
       ((,class (:inherit compilation-info :foreground ,green :weight bold))))
     `(compilation-mode-line-fail
       ((,class (:inherit compilation-error :foreground ,red :weight bold))))
     `(compilation-message-face ((,class (:foreground ,blue))))
     `(compilation-warning-face ((,class (:foreground ,yellow :weight bold :underline t))))

     ;; cua
     `(cua-global-mark ((,class (:background ,yellow :foreground ,tahti-bg))))
     `(cua-rectangle ((,class (:inherit region :background ,magenta :foreground ,tahti-bg))))
     `(cua-rectangle-noselect ((,class (:inherit region :background ,tahti-hl
                                                 :foreground ,tahti-comments))))

     ;; diary
     `(diary ((,class (:foreground ,yellow))))

     ;; dired
     `(dired-directory ((,class (:foreground ,blue :weight normal))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,tahti-bg :background ,blue))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,yellow :weight bold))))
     `(dired-marked ((,class (:foreground ,magenta :weight bold))))
     `(dired-perm-write ((,class (:foreground ,tahti-fg :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,orange :underline t))))

     ;; grep
     `(grep-context-face ((,class (:foreground ,tahti-fg))))
     `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(grep-hit-face ((,class (:foreground ,blue))))
     `(grep-match-face ((,class (:foreground ,orange :weight bold))))

     ;; faces used by isearch
     `(isearch ((,class (:foreground ,yellow :background ,tahti-hl :bold t))))
     `(isearch-fail ((,class (:foreground ,red :background ,tahti-bg :bold t))))

     ;; misc faces
     `(menu ((,class (:foreground ,tahti-fg :background ,tahti-bg))))
     `(minibuffer-prompt ((,class (:foreground ,tahti-emph))))
     `(mode-line
       ((,class (:foreground ,black
                             :background ,yellow
                             :box (:line-width -1 :style released-button)))))
     `(mode-line-buffer-id ((,class (:foreground ,tahti-fg :background ,tahti-bg))))
     `(mode-line-inactive
       ((,class (:foreground ,tahti-fg
                             :background ,tahti-bg
                             :box (:line-width -1 :style released-button)))))
     `(powerline-inactive1 ((,class (:foreground ,tahti-fg :background "grey11"))))
     `(powerline-inactive2 ((,class (:foreground ,tahti-fg :background "grey18"))))
     `(powerline-active1 ((,class (:foreground ,black :background "grey30"))))
     `(powerline-active2 ((,class (:foreground ,black :background "grey51"))))
     `(region ((,class (:foreground ,tahti-bg :background ,tahti-emph))))
     `(secondary-selection ((,class (:background ,tahti-bg))))
     `(trailing-whitespace ((,class (:background ,red))))
     `(vertical-border ((,class (:foreground ,tahti-fg))))

     ;; font lock
     `(font-lock-builtin-face ((,class (:foreground ,blue :slant italic))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,tahti-comments))))
     `(font-lock--delimiter-face ((,class (:foreground ,grey))))
     `(font-lock-comment-face ((,class (:foreground ,tahti-comments))))
     `(font-lock-constant-face ((,class (:foreground ,orange-lc :weight bold))))
     `(font-lock-number-face ((,class (:foreground , purple))))
     `(font-lock-doc-face ((,class (:foreground ,cyan :slant italic))))
     `(font-lock-doc-string-face ((,class (:foreground ,blue))))
     `(font-lock-function-name-face ((,class (:foreground ,chartreusel))))
     `(font-lock-keyword-face ((,class (:foreground ,red :weight bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,tahti-fg))))
     `(font-lock-preprocessor-face ((,class (:foreground ,green-hc))))
     `(font-lock-string-face ((,class (:foreground ,yellow))))
     `(font-lock-type-face ((,class (:foreground ,blue-hc))))
     `(font-lock-variable-name-face ((,class (:foreground ,orange))))
     `(font-lock-warning-face ((,class (:foreground ,orange :weight bold :underline t))))

     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;;; external

     ;; ace-jump-mode
     `(ace-jump-face-background
       ((,class (:foreground ,tahti-comments :background ,tahti-bg :inverse-video nil))))
     `(ace-jump-face-foreground
       ((,class (:foreground ,red :background ,tahti-bg :inverse-video nil))))

     ;; auto highlight symbol
     `(ahs-definition-face ((,class (:foreground ,tahti-bg :background ,blue :underline t))))
     `(ahs-edit-mode-face ((,class (:foreground ,tahti-bg :background ,yellow))))
     `(ahs-face ((,class (:foreground ,tahti-bg :background ,blue))))
     `(ahs-plugin-bod-face ((,class (:foreground ,tahti-bg :background ,blue))))
     `(ahs-plugin-defalt-face ((,class (:foreground ,tahti-bg :background ,cyan))))
     `(ahs-plugin-whole-buffer-face ((,class (:foreground ,tahti-bg :background ,green))))
     `(ahs-warning-face ((,class (:foreground ,red :weight bold))))

     ;; android mode
     `(android-mode-debug-face ((,class (:foreground ,green))))
     `(android-mode-error-face ((,class (:foreground ,orange :weight bold))))
     `(android-mode-info-face ((,class (:foreground ,tahti-fg))))
     `(android-mode-verbose-face ((,class (:foreground ,tahti-comments))))
     `(android-mode-warning-face ((,class (:foreground ,yellow))))

     ;; bm
     `(bm-face ((,class (:background ,yellow-lc :foreground ,tahti-bg))))
     `(bm-fringe-face ((,class (:background ,yellow-lc :foreground ,tahti-bg))))
     `(bm-fringe-persistent-face ((,class (:background ,green-lc :foreground ,tahti-bg))))
     `(bm-persistent-face ((,class (:background ,green-lc :foreground ,tahti-bg))))

     ;; calfw
     `(cfw:face-day-title ((,class (:background ,tahti-hl))))
     `(cfw:face-annotation ((,class (:inherit cfw:face-day-title :foreground ,yellow))))
     `(cfw:face-default-content ((,class (:foreground ,green))))
     `(cfw:face-default-day ((,class (:inherit cfw:face-day-title :weight bold))))
     `(cfw:face-disable ((,class (:inherit cfw:face-day-title :foreground ,tahti-comments))))
     `(cfw:face-grid ((,class (:foreground ,tahti-comments))))
     `(cfw:face-header ((,class (:foreground ,blue-hc :background ,blue-lc :weight bold))))
     `(cfw:face-holiday ((,class (:background nil :foreground ,red :weight bold))))
     `(cfw:face-periods ((,class (:foreground ,magenta))))
     `(cfw:face-select ((,class (:background ,magenta-lc :foreground ,magenta-hc))))
     `(cfw:face-saturday ((,class (:foreground ,cyan-hc :background ,cyan-lc))))
     `(cfw:face-sunday ((,class (:foreground ,red-hc :background ,red-lc :weight bold))))
     `(cfw:face-title ((,class (:inherit variable-pitch :foreground ,yellow :weight bold :height 2.0))))
     `(cfw:face-today ((,class (:weight bold :background ,tahti-hl :foreground nil))))
     `(cfw:face-today-title ((,class (:background ,yellow-lc :foreground ,yellow-l :weight bold))))
     `(cfw:face-toolbar ((,class (:background ,tahti-hl :foreground ,tahti-fg))))
     `(cfw:face-toolbar-button-off ((,class (:background ,yellow-lc :foreground ,yellow-l :weight bold))))
     `(cfw:face-toolbar-button-on ((,class (:background ,yellow-l :foreground ,yellow-lc :weight bold))))

     ;; custom
     `(custom-variable-tag ((,class (:foreground ,cyan))))
     `(custom-comment-tag ((,class (:foreground ,tahti-comments))))
     `(custom-group-tag ((,class (:foreground ,blue))))
     `(custom-state ((,class (:foreground ,green))))

     ;; diff
     `(diff-added ((,class (:foreground ,green))))
     `(diff-changed ((,class (:foreground ,yellow))))
     `(diff-removed ((,class (:foreground ,red))))
     `(diff-header ((,class (:background ,tahti-bg))))
     `(diff-file-header
       ((,class (:background ,tahti-bg :foreground ,tahti-fg :weight bold))))

     ;; eshell
     `(eshell-prompt ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
     `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
     `(eshell-ls-executable ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,tahti-fg))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc))))
     `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))

     ;; flymake
     `(flymake-errline
       ((,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flymake-infoline ((,class (:foreground ,green-hc :background ,green-lc))))
     `(flymake-warnline
       ((,class (:foreground ,yellow-l :background ,yellow-lc :weight bold :underline t))))

     ;; flyspell
     `(flyspell-duplicate ((,class (:foreground ,yellow :weight bold :underline t))))
     `(flyspell-incorrect ((,class (:foreground ,red :weight bold :underline t))))

     ;; erc
     `(erc-action-face ((,class (:inherit erc-default-face))))
     `(erc-bold-face ((,class (:weight bold))))
     `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
     `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
     `(erc-default-face ((,class (:foreground ,tahti-fg))))
     `(erc-direct-msg-face ((,class (:inherit erc-default))))
     `(erc-error-face ((,class (:inherit font-lock-warning))))
     `(erc-fool-face ((,class (:inherit erc-default))))
     `(erc-highlight-face ((,class (:inherit hover-highlight))))
     `(erc-input-face ((,class (:foreground ,yellow))))
     `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
     `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
     `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
     `(erc-nick-msg-face ((,class (:inherit erc-default))))
     `(erc-notice-face ((,class (:foreground ,green))))
     `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
     `(erc-prompt-face ((,class (:foreground ,orange :background ,tahti-bg :weight bold))))
     `(erc-timestamp-face ((,class (:foreground ,green))))
     `(erc-underline-face ((t (:underline t))))

     ;; gnus
     `(gnus-group-mail-1-face ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2-face ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3-face ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-4-face ((,class (:weight bold :inherit gnus-group-mail-4-empty))))
     `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
     `(gnus-group-mail-5-face ((,class (:weight bold :inherit gnus-group-mail-5-empty))))
     `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
     `(gnus-group-mail-6-face ((,class (:weight bold :inherit gnus-group-mail-6-empty))))
     `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
     `(gnus-group-mail-low-face ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1-face ((,class (:weight bold :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2-face ((,class (:weight bold :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3-face ((,class (:weight bold :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4-face ((,class (:weight bold :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5-face ((,class (:weight bold :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6-face ((,class (:weight bold :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low-face ((,class (:weight bold :inherit gnus-group-news-low-empty))))
     `(gnus-header-content-face ((,class (:inherit message-header-other))))
     `(gnus-header-from-face ((,class (:inherit message-header-from))))
     `(gnus-header-name-face ((,class (:inherit message-header-name))))
     `(gnus-header-newsgroups-face ((,class (:inherit message-header-other))))
     `(gnus-header-subject-face ((,class (:inherit message-header-subject))))
     `(gnus-summary-cancelled-face ((,class (:foreground ,orange))))
     `(gnus-summary-high-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-high-read-face ((,class (:foreground ,green :weight bold))))
     `(gnus-summary-high-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-high-unread-face ((,class (:foreground ,tahti-fg :weight bold))))
     `(gnus-summary-low-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-low-read-face ((t (:foreground ,green))))
     `(gnus-summary-low-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-low-unread-face ((,class (:foreground ,tahti-fg))))
     `(gnus-summary-normal-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-normal-read-face ((,class (:foreground ,green))))
     `(gnus-summary-normal-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-normal-unread-face ((,class (:foreground ,tahti-fg))))
     `(gnus-summary-selected-face ((,class (:foreground ,yellow :weight bold))))
     `(gnus-cite-1-face ((,class (:foreground ,blue))))
     `(gnus-cite-10-face ((,class (:foreground ,yellow))))
     `(gnus-cite-11-face ((,class (:foreground ,yellow))))
     `(gnus-cite-2-face ((,class (:foreground ,blue))))
     `(gnus-cite-3-face ((,class (:foreground ,blue))))
     `(gnus-cite-4-face ((,class (:foreground ,green))))
     `(gnus-cite-5-face ((,class (:foreground ,green))))
     `(gnus-cite-6-face ((,class (:foreground ,green))))
     `(gnus-cite-7-face ((,class (:foreground ,red))))
     `(gnus-cite-8-face ((,class (:foreground ,red))))
     `(gnus-cite-9-face ((,class (:foreground ,red))))
     `(gnus-group-news-1-empty-face ((,class (:foreground ,yellow))))
     `(gnus-group-news-2-empty-face ((,class (:foreground ,green))))
     `(gnus-group-news-3-empty-face ((,class (:foreground ,green))))
     `(gnus-group-news-4-empty-face ((,class (:foreground ,blue))))
     `(gnus-group-news-5-empty-face ((,class (:foreground ,blue))))
     `(gnus-group-news-6-empty-face ((,class (:foreground ,tahti-bg))))
     `(gnus-group-news-low-empty-face ((,class (:foreground ,tahti-bg))))
     `(gnus-signature-face ((,class (:foreground ,yellow))))
     `(gnus-x-face ((,class (:background ,tahti-fg :foreground ,tahti-bg))))

     ;; helm (these probably needs tweaking)
     `(helm-apt-deinstalled ((,class (:foreground ,tahti-comments))))
     `(helm-apt-installed ((,class (:foreground ,green))))
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,tahti-fg))))
     `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
     `(helm-bookmark-info ((,class (:foreground ,green))))
     `(helm-bookmark-man ((,class (:foreground ,violet))))
     `(helm-bookmark-w3m ((,class (:foreground ,yellow))))
     `(helm-bookmarks-su ((,class (:foreground ,orange))))
     `(helm-buffer-not-saved ((,class (:foreground ,orange))))
     `(helm-buffer-saved-out ((,class (:foreground ,red :background ,tahti-bg
                                                   :inverse-video t))))
     `(helm-buffer-size ((,class (:foreground ,tahti-comments))))
     `(helm-candidate-number ((,class (:background ,tahti-bg :foreground ,grey
                                                   :bold t))))
     `(helm-ff-directory ((,class (:background ,tahti-bg  :foreground ,blue))))
     `(helm-ff-executable ((,class (:foreground ,green))))
     `(helm-ff-file ((,class (:background ,tahti-bg :foreground ,tahti-fg))))
     `(helm-ff-invalid-symlink ((,class (:background ,tahti-bg :foreground ,orange
                                                     :slant italic))))
     `(helm-ff-prefix ((,class (:background ,yellow :foreground ,tahti-bg))))
     `(helm-ff-symlink ((,class (:foreground ,cyan))))
     `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
     `(helm-grep-finish ((,class (:foreground ,green))))
     `(helm-grep-lineno ((,class (:foreground ,orange))))
     `(helm-grep-match ((,class (:inherit match))))
     `(helm-grep-running ((,class (:foreground ,red))))
     `(helm-header ((,class (:inherit header-line))))
     `(helm-lisp-completion-info ((,class (:foreground ,tahti-bg))))
     `(helm-lisp-show-completion ((,class (:foreground ,yellow  :background ,tahti-hl
                                                       :bold t))))
     `(helm-M-x-key ((,class (:foreground ,orange :underline t))))
     `(helm-match ((,class (:inherit match))))
     `(helm-selection ((,class (:background ,tahti-hl :underline t))))
     `(helm-selection-line ((,class (:background ,tahti-hl :foreground ,tahti-emph
                                                 :underline nil))))
     `(helm-separator ((,class (:foreground ,red))))
     `(helm-source-header ((,class (:background ,blue-lc :foreground ,tahti-bg
                                                :underline nil))))
     `(helm-time-zone-current ((,class (:foreground ,green))))
     `(helm-time-zone-home ((,class (:foreground ,red))))
     `(helm-visible-mark ((,class (:background ,tahti-bg :foreground ,magenta :bold t))))

     ;; hi-lock-mode
     `(hi-yellow ((,class (:foreground ,yellow-lc :background ,yellow-l))))
     `(hi-pink ((,class (:foreground ,magenta-lc :background ,magenta-hc))))
     `(hi-green ((,class (:foreground ,green-lc :background ,green-hc))))
     `(hi-blue ((,class (:foreground ,blue-lc :background ,blue-hc))))
     `(hi-black-b ((,class (:foreground ,tahti-emph :background ,tahti-bg :weight bold))))
     `(hi-blue-b ((,class (:foreground ,blue-lc :weight bold))))
     `(hi-green-b ((,class (:foreground ,green-lc :weight bold))))
     `(hi-red-b ((,class (:foreground ,red :weight bold))))
     `(hi-black-hb ((,class (:foreground ,tahti-emph :background ,tahti-bg :weight bold))))

     ;; highlight-changes
     `(highlight-changes ((,class (:foreground ,orange))))
     `(highlight-changes-delete ((,class (:foreground ,red :underline t))))

     ;; hl-line-mode
     `(hl-line ((,class (:background ,tahti-hl))))
     `(hl-line-face ((,class (:background ,tahti-hl))))

     ;; ido-mode
     `(ido-first-match ((,class (:foreground ,green :weight bold))))
     `(ido-only-match ((,class (:foreground ,tahti-bg :background ,green :weight bold))))
     `(ido-subdir ((,class (:foreground ,blue))))
     `(ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
     `(ido-indicator ((,class (:background ,red :foreground ,tahti-bg :width condensed))))
     `(ido-virtual ((,class (:foreground ,cyan))))

     ;; js2-mode colors
     `(js2-error-face ((,class (:foreground ,red))))
     `(js2-external-variable-face ((,class (:foreground ,orange))))
     `(js2-function-param-face ((,class (:foreground ,green))))
     `(js2-instance-member-face ((,class (:foreground ,magenta))))
     `(js2-jsdoc-html-tag-delimiter-face ((,class (:foreground ,cyan))))
     `(js2-jsdoc-html-tag-name-face ((,class (:foreground ,orange))))
     `(js2-jsdoc-tag-face ((,class (:foreground ,cyan))))
     `(js2-jsdoc-type-face ((,class (:foreground ,blue))))
     `(js2-jsdoc-value-face ((,class (:foreground ,violet))))
     `(js2-magic-paren-face ((,class (:underline t))))
     `(js2-private-function-call-face ((,class (:foreground ,yellow))))
     `(js2-private-member-face ((,class (:foreground ,blue))))
     `(js2-warning-face ((,class (:underline ,orange))))

     ;; linum-mode
     `(linum ((,class (:foreground ,tahti-fg :background ,tahti-bg))))

     ;; magit
     `(magit-section-title ((,class (:foreground ,yellow :weight bold))))
     `(magit-branch ((,class (:foreground ,orange :weight bold))))
     `(magit-item-highlight ((,class (:background ,tahti-hl))))
     `(magit-log-graph ((,class (:foreground ,tahti-comments))))
     `(magit-log-head-label-bisect-bad ((,class (:background ,red-hc :foreground ,red-lc :box 1))))
     `(magit-log-head-label-bisect-good ((,class (:background ,green-hc :foreground ,green-lc
                                                              :box 1))))
     `(magit-log-head-label-default ((,class (:background ,tahti-hl :box 1))))
     `(magit-log-head-label-local ((,class (:background ,blue-lc :foreground ,blue-hc :box 1))))
     `(magit-log-head-label-patches ((,class (:background ,red-lc :foreground ,red-hc :box 1))))
     `(magit-log-head-label-remote ((,class (:background ,green-lc :foreground ,green-hc :box 1))))
     `(magit-log-head-label-tags ((,class (:background ,yellow-lc :foreground ,yellow-l :box 1))))
     `(magit-log-sha1 ((,class (:foreground ,yellow))))

     ;; message-mode
     `(message-cited-text ((,class (:foreground ,tahti-comments))))
     `(message-header-name ((,class (:foreground ,green))))
     `(message-header-other ((,class (:foreground ,green))))
     `(message-header-to ((,class (:foreground ,yellow :weight bold))))
     `(message-header-cc ((,class (:foreground ,orange :weight bold))))
     `(message-header-newsgroups ((,class (:foreground ,yellow :weight bold))))
     `(message-header-subject ((,class (:foreground ,orange))))
     `(message-header-xheader ((,class (:foreground ,cyan))))
     `(message-mml ((,class (:foreground ,yellow :weight bold))))
     `(message-separator ((,class (:foreground ,tahti-comments :slant italic))))

     ;; mew
     `(mew-face-header-subject ((,class (:foreground ,orange))))
     `(mew-face-header-from ((,class (:foreground ,yellow))))
     `(mew-face-header-date ((,class (:foreground ,green))))
     `(mew-face-header-to ((,class (:foreground ,red))))
     `(mew-face-header-key ((,class (:foreground ,green))))
     `(mew-face-header-private ((,class (:foreground ,green))))
     `(mew-face-header-important ((,class (:foreground ,blue))))
     `(mew-face-header-marginal ((,class (:foreground ,tahti-fg :weight bold))))
     `(mew-face-header-warning ((,class (:foreground ,red))))
     `(mew-face-header-xmew ((,class (:foreground ,green))))
     `(mew-face-header-xmew-bad ((,class (:foreground ,red))))
     `(mew-face-body-url ((,class (:foreground ,orange))))
     `(mew-face-body-comment ((,class (:foreground ,tahti-fg :slant italic))))
     `(mew-face-body-cite1 ((,class (:foreground ,green))))
     `(mew-face-body-cite2 ((,class (:foreground ,blue))))
     `(mew-face-body-cite3 ((,class (:foreground ,orange))))
     `(mew-face-body-cite4 ((,class (:foreground ,yellow))))
     `(mew-face-body-cite5 ((,class (:foreground ,red))))
     `(mew-face-mark-review ((,class (:foreground ,blue))))
     `(mew-face-mark-escape ((,class (:foreground ,green))))
     `(mew-face-mark-delete ((,class (:foreground ,red))))
     `(mew-face-mark-unlink ((,class (:foreground ,yellow))))
     `(mew-face-mark-refile ((,class (:foreground ,green))))
     `(mew-face-mark-unread ((,class (:foreground ,red))))
     `(mew-face-eof-message ((,class (:foreground ,green))))
     `(mew-face-eof-part ((,class (:foreground ,yellow))))

     ;; mingus
     `(mingus-directory-face ((,class (:foreground ,blue))))
     `(mingus-pausing-face ((,class (:foreground ,magenta))))
     `(mingus-playing-face ((,class (:foreground ,cyan))))
     `(mingus-playlist-face ((,class (:foreground ,cyan ))))
     `(mingus-song-file-face ((,class (:foreground ,yellow))))
     `(mingus-stopped-face ((,class (:foreground ,red))))

     ;; moccur
     `(moccur-current-line-face ((,class (:underline t))))
     `(moccur-edit-done-face ((,class
                               (:foreground ,tahti-comments
                                            :background ,tahti-bg
                                            :slant italic))))
     `(moccur-edit-face
       ((,class (:background ,yellow :foreground ,tahti-bg))))
     `(moccur-edit-file-face ((,class (:background ,tahti-hl))))
     `(moccur-edit-reject-face ((,class (:foreground ,red))))
     `(moccur-face ((,class (:background ,tahti-hl :foreground ,tahti-emph
                                         :weight bold))))

     ;; mu4e
     `(mu4e-cited-1-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-2-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-cited-3-face ((,class (:foreground ,orange :slant italic :weight normal))))
     `(mu4e-cited-4-face ((,class (:foreground ,yellow :slant italic :weight normal))))
     `(mu4e-cited-5-face ((,class (:foreground ,cyan :slant italic :weight normal))))
     `(mu4e-cited-6-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-7-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-flagged-face ((,class (:foreground ,magenta :weight bold))))
     `(mu4e-view-url-number-face ((,class (:foreground ,orange :weight bold))))
     `(mu4e-warning-face ((,class (:foreground ,red :slant normal :weight bold))))

     ;; mumamo
     `(mumamo-background-chunk-submode1 ((,class (:background ,tahti-hl))))

     ;; nav
     `(nav-face-heading ((,class (:foreground ,yellow))))
     `(nav-face-button-num ((,class (:foreground ,cyan))))
     `(nav-face-dir ((,class (:foreground ,green))))
     `(nav-face-hdir ((,class (:foreground ,red))))
     `(nav-face-file ((,class (:foreground ,tahti-fg))))
     `(nav-face-hfile ((,class (:foreground ,red))))

     ;; nav-flash
     `(nav-flash-face ((,class (:foreground ,orange :background ,tahti-hl))))

     ;; org-mode
     `(org-agenda-structure
       ((,class (:inherit font-lock-comment-face :foreground ,magenta :inverse-video t))))
     `(org-agenda-date
       ((,class (:foreground ,tahti-fg :background ,tahti-hl :weight bold
                             :box (:line-width 4 :color ,tahti-hl) ))) t)
     `(org-agenda-date-weekend ((,class (:inherit org-agenda-date :slant italic))) t)
     `(org-agenda-date-today
       ((,class (:inherit org-agenda-date :slant italic underline: t))) t)
     `(org-agenda-done ((,class (:foreground ,green))) t)
     `(org-archived ((,class (:foreground ,tahti-comments :weight normal))))
     `(org-block ((,class (:foreground ,cyan-hc))))
     `(org-block-begin-line ((,class (:foreground ,tahti-comments :slant italic))))
     `(org-checkbox ((,class (:background ,tahti-bg :foreground ,tahti-fg
                                          :box (:line-width 1 :style released-button)))))
     `(org-code ((,class (:foreground ,green-hc))))
     `(org-date ((,class (:foreground ,blue :underline t))))
     `(org-done ((,class (:weight bold :foreground ,green))))
     `(org-ellipsis ((,class (:foreground ,tahti-comments))))
     `(org-formula ((,class (:foreground ,yellow))))
     `(org-headline-done ((,class (:foreground ,green))))
     `(org-hide ((,class (:foreground ,tahti-bg))))
     `(org-level-1 ((,class (:foreground ,orange))))
     `(org-level-2 ((,class (:foreground ,green))))
     `(org-level-3 ((,class (:foreground ,blue))))
     `(org-level-4 ((,class (:foreground ,yellow))))
     `(org-level-5 ((,class (:foreground ,cyan))))
     `(org-level-6 ((,class (:foreground ,green))))
     `(org-level-7 ((,class (:foreground ,red))))
     `(org-level-8 ((,class (:foreground ,blue))))
     `(org-link ((,class (:foreground ,yellow :underline t))))
     `(org-sexp-date ((,class (:foreground ,violet))))
     `(org-scheduled ((,class (:foreground ,green))))
     `(org-scheduled-previously ((,class (:foreground ,yellow))))
     `(org-scheduled-today ((,class (:foreground ,blue :weight bold))))
     `(org-special-keyword ((,class (:foreground ,tahti-comments :weight bold))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:weight bold))))
     `(org-time-grid ((,class (:foreground ,cyan))))
     `(org-todo ((,class (:foreground ,red :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,yellow ))))
     `(org-warning ((,class (:foreground ,orange :weight bold :underline t))))
     ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
     `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
     `(org-habit-ready-future-face ((,class (:background ,green-lc))))
     `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
     `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
     `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
     `(org-habit-overdue-future-face ((,class (:background ,red-lc))))
     ;; latest additions
     `(org-agenda-dimmed-todo-face ((,class (:foreground ,tahti-comments))))
     `(org-agenda-restriction-lock ((,class (:background ,yellow))))
     `(org-clock-overlay ((,class (:background ,yellow))))
     `(org-column ((,class (:background ,tahti-hl :strike-through nil
                                        :underline nil :slant normal :weight normal))))
     `(org-column-title ((,class (:background ,tahti-hl :underline t :weight bold))))
     `(org-date-selected ((,class (:foreground ,red :inverse-video t))))
     `(org-document-info ((,class (:foreground ,tahti-fg))))
     `(org-document-title ((,class (:foreground ,tahti-emph  :weight bold :height 1.44))))
     `(org-drawer ((,class (:foreground ,cyan))))
     `(org-footnote ((,class (:foreground ,magenta :underline t))))
     `(org-latex-and-export-specials ((,class (:foreground ,orange))))
     `(org-mode-line-clock-overrun ((,class (:inherit modeline :background ,red))))

     ;; outline
     `(outline-8 ((,class (:inherit default))))
     `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
     `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
     `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
     `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
     `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
     `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
     `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

     ;; pretty-mode
     `(pretty-mode-symbol-face  ((,class (:foreground ,green))))

     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,tahti-fg :background ,tahti-bg :inverse-video t))))

     ;; rst-mode
     `(rst-level-1-face ((,class (:background ,yellow   :foreground ,tahti-bg))))
     `(rst-level-2-face ((,class (:background ,cyan    :foreground ,tahti-bg))))
     `(rst-level-3-face ((,class (:background ,blue    :foreground ,tahti-bg))))
     `(rst-level-4-face ((,class (:background ,violet  :foreground ,tahti-bg))))
     `(rst-level-5-face ((,class (:background ,magenta :foreground ,tahti-bg))))
     `(rst-level-6-face ((,class (:background ,red     :foreground ,tahti-bg))))

     ;; rpm-mode
     `(rpm-spec-dir-face ((,class (:foreground ,green))))
     `(rpm-spec-doc-face ((,class (:foreground ,green))))
     `(rpm-spec-ghost-face ((,class (:foreground ,red))))
     `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
     `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
     `(rpm-spec-package-face ((,class (:foreground ,red))))
     `(rpm-spec-section-face ((,class (:foreground ,yellow))))
     `(rpm-spec-tag-face ((,class (:foreground ,blue))))
     `(rpm-spec-var-face ((,class (:foreground ,red))))

     ;; sh-mode
     `(sh-quoted-exec ((,class (:foreground ,violet :weight bold))))
     `(sh-escaped-newline ((,class (:foreground ,yellow :weight bold))))
     `(sh-heredoc ((,class (:foreground ,yellow :weight bold))))

     ;; show-paren
     `(show-paren-match
       ((,class (:foreground ,tahti-bg :background ,orange :weight normal :inverse-video t))))
     `(show-paren-mismatch
       ((,class (:foreground ,tahti-bg :background ,red :weight normal :inverse-video t))))

     ;; mic-paren
     `(paren-face-match
       ((,class (:foreground ,cyan :background ,tahti-bg :weight normal :inverse-video t))))
     `(paren-face-mismatch
       ((,class (:foreground ,red :background ,tahti-bg :weight normal :inverse-video t))))
     `(paren-face-no-match
       ((,class (:foreground ,red :background ,tahti-bg :weight normal :inverse-video t))))

     ;; SLIME
     `(slime-repl-inputed-output-face ((,class (:foreground ,red))))

     ;; speedbar
     `(speedbar-button-face ((,class (:inherit variable-pitch :foreground ,tahti-comments))))
     `(speedbar-directory-face ((,class (:inherit variable-pitch :foreground ,blue))))
     `(speedbar-file-face ((,class (:inherit variable-pitch :foreground ,tahti-fg))))
     `(speedbar-highlight-face ((,class (:inherit variable-pitch :background ,tahti-hl))))
     `(speedbar-selected-face ((,class (:inherit variable-pitch :foreground ,yellow :underline t))))
     `(speedbar-separator-face ((,class (:inherit variable-pitch
                                                  :background ,blue :foreground ,tahti-bg
                                                  :overline ,cyan-lc))))
     `(speedbar-tag-face ((,class (:inherit variable-pitch :foreground ,green))))

     ;; sunrise commander headings
     `(sr-active-path-face ((,class (:background ,blue :foreground ,tahti-bg
                                                 :height 100  :weight bold))))
     `(sr-editing-path-face ((,class (:background ,yellow :foreground ,tahti-bg
                                                  :weight bold :height 100))))
     `(sr-highlight-path-face ((,class (:background ,green :foreground ,tahti-bg
                                                    :weight bold :height 100))))
     `(sr-passive-path-face ((,class (:background ,tahti-comments :foreground ,tahti-bg
                                                  :weight bold :height 100))))
     ;; sunrise commander marked
     `(sr-marked-dir-face ((,class (:inherit dired-marked))))
     `(sr-marked-file-face ((,class (:inherit dired-marked))))
     `(sr-alt-marked-dir-face ((,class (:background ,magenta :foreground ,tahti-bg
                                                    :weight bold))))
     `(sr-alt-marked-file-face ((,class (:background ,magenta :foreground ,tahti-bg
                                                     :weight bold))))
     ;; sunrise commander fstat
     `(sr-directory-face ((,class (:inherit dired-directory :weight normal))))
     `(sr-symlink-directory-face ((,class (:inherit dired-directory :slant italic :weight normal))))
     `(sr-symlink-face ((,class (:inherit dired-symlink :slant italic :weight normal))))
     `(sr-broken-link-face ((,class (:inherit dired-warning :slant italic :weight normal))))
     ;; sunrise commander file types
     `(sr-compressed-face ((,class (:foreground ,tahti-fg))))
     `(sr-encrypted-face ((,class (:foreground ,tahti-fg))))
     `(sr-log-face ((,class (:foreground ,tahti-fg))))
     `(sr-packaged-face ((,class (:foreground ,tahti-fg))))
     `(sr-html-face ((,class (:foreground ,tahti-fg))))
     `(sr-xml-face ((,class (:foreground ,tahti-fg))))
     ;; sunrise commander misc
     `(sr-clex-hotchar-face ((,class (:background ,red  :foreground ,tahti-bg :weight bold))))

     ;; table
     `(table-cell ((,class (:foreground ,tahti-fg :background ,tahti-hl))))

     ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
     ;; zencoding uses this)
     `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-l
                                     :inherit variable-pitch))))

     ;; tuareg
     `(tuareg-font-lock-governing-face ((,class (:foreground ,magenta :weight bold))))
     `(tuareg-font-lock-multistage-face ((,class (:foreground ,blue :background ,tahti-hl :weight bold))))
     `(tuareg-font-lock-operator-face ((,class (:foreground ,tahti-emph))))
     `(tuareg-font-lock-error-face ((,class (:foreground ,yellow :background ,red :weight bold))))
     `(tuareg-font-lock-interactive-output-face ((,class (:foreground ,cyan))))
     `(tuareg-font-lock-interactive-error-face ((,class (:foreground ,red))))

     ;; undo-tree
     `(undo-tree-visualizer-default-face
       ((,class (:foreground ,tahti-comments :background ,tahti-bg))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,cyan :inverse-video t))))
     `(undo-tree-visualizer-active-branch-face
       ((,class (:foreground ,tahti-emph :background ,tahti-bg :weight bold))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; volatile highlights
     `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))

     ;; w3m
     `(w3m-anchor ((,class (:inherit link))))
     `(w3m-arrived-anchor ((,class (:inherit link-visited))))
     `(w3m-form ((,class (:background ,tahti-bg :foreground ,tahti-fg))))
     `(w3m-header-line-location-title ((,class (:background ,tahti-bg :foreground ,yellow))))
     `(w3m-header-line-location-content ((,class (:background ,tahti-bg :foreground ,tahti-fg))))
     `(w3m-bold ((,class (:foreground ,tahti-emph :weight bold))))
     `(w3m-image-anchor ((,class (:background ,tahti-bg :foreground ,cyan :inherit link))))
     `(w3m-image ((,class (:background ,tahti-bg :foreground ,cyan))))
     `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,tahti-emph))))
     `(w3m-lnum-match ((,class (:background ,tahti-hl))))
     `(w3m-lnum ((,class (:underline nil :bold nil :foreground ,red))))

     ;; whitespace-mode
     `(whitespace-space ((,class (:background ,tahti-bg :foreground ,yellow-lc
                                              :inverse-video t))))
     `(whitespace-hspace ((,class (:background ,tahti-bg :foreground ,red-lc
                                               :inverse-video t))))
     `(whitespace-tab ((,class (:background ,tahti-bg :foreground ,orange-lc
                                            :inverse-video t))))
     `(whitespace-newline ((,class (:foreground ,tahti-comments))))
     `(whitespace-trailing ((,class (:foreground ,orange-lc :background ,tahti-bg
                                                 :inverse-video t))))
                                        ; removing inverse video on this
     `(whitespace-line ((,class (:background ,tahti-bg :foreground ,magenta
                                             :inverse-video nil))))
     `(whitespace-space-before-tab ((,class (:background ,tahti-bg :foreground ,green-lc
                                                         :inverse-video t))))
     `(whitespace-indentation ((,class (:background ,tahti-bg :foreground ,magenta-lc
                                                    :inverse-video t))))
     `(whitespace-empty ((,class (:background ,tahti-fg :foreground ,red-lc :inverse-video t))))
     `(whitespace-space-after-tab ((,class (:background ,tahti-bg  :foreground ,violet-lc
                                                        :inverse-video t))))

     ;; wanderlust
     `(wl-highlight-folder-few-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-many-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-path-face ((,class (:foreground ,orange))))
     `(wl-highlight-folder-unread-face ((,class (:foreground ,blue))))
     `(wl-highlight-folder-zero-face ((,class (:foreground ,tahti-fg))))
     `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue))))
     `(wl-highlight-message-citation-header ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green))))
     `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue))))
     `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue))))
     `(wl-highlight-message-header-contents-face ((,class (:foreground ,green))))
     `(wl-highlight-message-headers-face ((,class (:foreground ,red))))
     `(wl-highlight-message-important-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green))))
     `(wl-highlight-message-signature ((,class (:foreground ,green))))
     `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,tahti-fg))))
     `(wl-highlight-summary-answered-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-disposed-face ((,class (:foreground ,tahti-fg
                                                                :slant italic))))
     `(wl-highlight-summary-new-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-normal-face ((,class (:foreground ,tahti-fg))))
     `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow))))
     `(wl-highlight-thread-indent-face ((,class (:foreground ,magenta))))
     `(wl-highlight-summary-refiled-face ((,class (:foreground ,tahti-fg))))
     `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

     ;; which-func-mode
     `(which-func ((,class (:foreground ,green))))

     ;; window-number-mode
     `(window-number-face ((,class (:foreground ,green))))

     ;; yascroll
     `(yascroll:thumb-text-area
       ((,class (:foreground ,tahti-comments :background ,tahti-comments))))
     `(yascroll:thumb-fringe
       ((,class (:foreground ,tahti-comments :background ,tahti-comments))))

     ;; zencoding
     `(zencoding-preview-input ((,class (:background ,tahti-hl :box ,tahti-emph)))))


    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector [,tahti-bg ,red ,green ,yellow
                                             ,blue ,magenta ,cyan ,tahti-fg])
     `(ansi-term-color-vector [unspecific ,tahti-fg ,red ,green ,yellow ,blue ,magenta ,cyan ,tahti-bg])
     ;; fill-column-indicator
     `(fci-rule-color ,tahti-hl)

     ;; highlight-changes
     `(highlight-changes-colors '(,magenta ,violet))

     ;; highlight-tail
     `(highlight-tail-colors
       '((,tahti-hl . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
         (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,tahti-hl . 100))))

    ;; call chained theme function
    (when childtheme (funcall childtheme))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:
(create-tahti-theme 'dark 'tahti-dark)
(provide-theme 'tahti-dark)

;;; tahti.el ends here.
;(deftheme tahti
  ;"Created 2012-11-19.")

;(custom-theme-set-faces
 ;'tahti
 ;'(default ((t (:family "DejaVu Sans Mono" :foundry "bitstream" :width normal :height  :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#dcdccc" :background "#3f3f3f" :stipple nil :inherit nil))))
 ;'(cursor ((t (:foreground "#dcdccc" :background "white"))))
 ;'(fixed-pitch ((t (:family "Monospace"))))
 ;'(variable-pitch ((t (:family "Sans Serif"))))
 ;'(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 ;'(minibuffer-prompt ((t (:foreground "#f0dfaf"))))
 ;'(highlight ((t (:inverse-video t :background "#383838"))))
 ;'(region ((t (:inverse-video nil :background "#2b2b2b" :inherit (highlight)))))
 ;'(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 ;'(secondary-selection ((t (:background "#5f5f5f"))))
 ;'(trailing-whitespace ((t (:background "#cc9393"))))
 ;'(font-lock-builtin-face ((t (:foreground "#93e0e3"))))
 ;'(font-lock-comment-delimiter-face ((t (:foreground "#7f9f7f" :inherit (font-lock-comment-face)))))
 ;'(font-lock-comment-face ((t (:foreground "#7f9f7f"))))
 ;'(font-lock-constant-face ((t (:foreground "#bfebbf"))))
 ;'(font-lock-doc-face ((t (:foreground "#8fb28f" :inherit (font-lock-string-face)))))
 ;'(font-lock-function-name-face ((t (:foreground "#8cd0d3"))))
 ;'(font-lock-keyword-face ((t (:weight bold :foreground "#f0dfaf"))))
 ;'(font-lock-negation-char-face ((t (:foreground "#dcdccc"))))
 ;'(font-lock-preprocessor-face ((t (:foreground "#94bff3" :inherit (font-lock-builtin-face)))))
 ;'(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 ;'(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 ;'(font-lock-string-face ((t (:foreground "#cc9393"))))
 ;'(font-lock-type-face ((t (:foreground "#7cb8bb"))))
 ;'(font-lock-variable-name-face ((t (:foreground "#dfaf8f"))))
 ;'(font-lock-warning-face ((t (:weight bold :underline t :foreground "#dfaf8f" :inherit (error)))))
 ;'(button ((t (:underline t :inherit (link)))))
 ;'(link ((t (:weight bold :underline t :foreground "#f0dfaf"))))
 ;'(link-visited ((t (:weight normal :underline t :foreground "#d0bf8f" :inherit (link)))))
 ;'(fringe ((t (:foreground "#dcdccc" :background "#4f4f4f"))))
 ;'(header-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#f0dfaf" :background "#2b2b2b" :inherit (mode-line)))))
 ;'(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 ;'(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#8fb28f" :background "#2b2b2b"))))
 ;'(mode-line-buffer-id ((t (:weight bold :foreground "#f0dfaf"))))
 ;'(mode-line-emphasis ((t (:weight bold))))
 ;'(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 ;'(mode-line-inactive ((t (:weight light :box (:line-width -1 :color nil :style released-button) :foreground "#5f7f5f" :background "#383838" :inherit (mode-line)))))
 ;'(isearch ((t (:foreground "#f0dfaf" :background "#2b2b2b"))))
 ;'(isearch-fail ((t (:foreground "#dcdccc" :background "#8c5353"))))
 ;'(lazy-highlight ((t (:foreground "#f0dfaf" :background "#5f5f5f"))))
 ;'(match ((t (:weight bold :foreground "#dfaf8f" :background "#2b2b2b"))))
 ;'(next-error ((t (:inherit (region)))))
 ;'(query-replace ((t (:inherit (isearch))))))

