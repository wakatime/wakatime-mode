;;; wakatime-mode.el --- Automatic time tracking extension for WakaTime

;; Copyright (C) 2013  Gabor Torok <gabor@20y.hu>

;; Author: Gabor Torok <gabor@20y.hu>
;; Maintainer: Alan Hamlett <alan@wakatime.com>
;; Website: https://wakatime.com
;; Keywords: calendar, comm
;; Version: 1.0.2

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

;; Enable WakaTime for the current buffer by invoking
;; `wakatime-mode'. If you wish to activate it globally, use
;; `global-wakatime-mode'.

;; Set variable `wakatime-api-key' to your API key. Point
;; `wakatime-cli-path' to the absolute path of the CLI script
;; (wakatime-cli.py).

;;; Code:

(defconst wakatime-version "1.0.2")
(defconst wakatime-user-agent "emacs-wakatime")
(defvar wakatime-noprompt nil)
(defvar wakatime-init-started nil)
(defvar wakatime-init-finished nil)
(defvar wakatime-python-path nil)

(defgroup wakatime nil
  "Customizations for WakaTime"
  :group 'convenience
  :prefix "wakatime-"
)

(defcustom wakatime-api-key nil
  "API key for WakaTime."
  :type 'string
  :group 'wakatime
)

(defcustom wakatime-cli-path nil
  "Path of CLI client for WakaTime."
  :type 'string
  :group 'wakatime
)

(defcustom wakatime-python-bin "python"
  "Path of Python binary."
  :type 'string
  :group 'wakatime
)

(defun wakatime-debug (msg)
  "Write a string to the *messages* buffer."
  (message "%s" msg))

(defun wakatime-guess-actual-script-path (path)
  (let ((true-path (if (null path) nil (file-truename path))))
    (unless (null true-path)
      (cond
       ((string-match-p "\\.pyenv" true-path) ; pyenv
        (with-temp-buffer
          (call-process "pyenv" nil t nil "which" "wakatime")
          (delete-char -1) ; delete newline at the end of output
          (buffer-string)))
       ((string-match-p "Cellar" true-path)  ; Homebrew
        (let* ((libexec (format "%slibexec/" (file-name-directory (directory-file-name (file-name-directory true-path)))))
               (python-path (format "%slib/python2.7/site-packages" libexec)))
          (setq wakatime-python-path python-path)
          (format "%sbin/wakatime" libexec)))
       (t path))
      nil)))

(defun wakatime-init ()
  (unless wakatime-init-started
    (setq wakatime-init-started t)
    (when (null wakatime-cli-path)
      (customize-set-variable 'wakatime-cli-path
                              (wakatime-guess-actual-script-path (executable-find "wakatime")))
    )
    (when (or (not wakatime-cli-path) (not (file-exists-p wakatime-cli-path)))
      (wakatime-prompt-cli-path)
    )
    (when (or (not wakatime-python-bin) (not (wakatime-python-exists wakatime-python-bin)))
      (wakatime-prompt-python-bin)
    )
    (setq wakatime-init-finished t)
  )
)

(defun wakatime-prompt-api-key ()
  "Prompt user for api key."
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((api-key (read-string "WakaTime API key: ")))
      (customize-set-variable 'wakatime-api-key api-key)
      (customize-save-customized)
    )
    (setq wakatime-noprompt nil)
  )
)

(defun wakatime-prompt-cli-path ()
  "Prompt user for cli path."
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((cli-path (read-file-name "WakaTime CLI script path: ")))
      (customize-set-variable 'wakatime-cli-path cli-path)
      (customize-save-customized)
    )
    (setq wakatime-noprompt nil)
  )
)

(defun wakatime-prompt-python-bin ()
  "Prompt user for path to python binary."
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((python-bin (read-string "Path to python binary: ")))
      (customize-set-variable 'wakatime-python-bin python-bin)
      (customize-save-customized)
    )
    (setq wakatime-noprompt nil)
  )
  nil
)

(defun wakatime-python-exists (location)
  "Check if python exists in the specified path location."
  (= (condition-case nil (call-process location nil nil nil "--version") (error 1)) 0)
)

(defun wakatime-client-command (savep &optional dont-use-key)
  "Return client command executable and arguments.
   Set SAVEP to non-nil for write action.
   Set DONT-USE-KEY to t if you want to omit --key from the command
   line."
  (format "%s%s --file \"%s\" --plugin \"%s/%s\" --time %.2f %s %s"
    (unless (= "wakatime" wakatime-cli-path) (format "\"%s\"" wakatime-python-bin) "")
    (unless (= "wakatime" wakatime-cli-path) (format "\"%s\"" wakatime-cli-path) "wakatime")
    (buffer-file-name (current-buffer))
    wakatime-user-agent
    wakatime-version
    (float-time)
    (if savep "--write" "")
    (unless dont-use-key (format "--key %s" wakatime-api-key) "")
  )
)

(defun wakatime-call (savep &optional retrying)
  "Call WakaTime command."
  (let*
    (
      (dont-use-key (or (not wakatime-api-key) (string= "" wakatime-api-key)))
      (command (wakatime-client-command savep dont-use-key))
      (process-environment (if wakatime-python-path
                               (cons (format "PYTHONPATH=%s" wakatime-python-path) process-environment)
                             process-environment))
      (process
        (start-process
          "Shell"
          (generate-new-buffer " *WakaTime messages*")
          shell-file-name
          shell-command-switch
          command
        )
      )
    )

    (set-process-sentinel process
      `(lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (kill-buffer (process-buffer process))
           (let ((exit-status (process-exit-status process)))
             ; first check for config and api-key error
             (cond
              ((or (= 103 exit-status) (= 104 exit-status))
               (if ,retrying
                   (error "WakaTime Error (%s)" exit-status)
                 ; otherwise, ask for an API key and call ourselves
                 ; recursively
                 (wakatime-prompt-api-key)
                 (wakatime-call ,savep t)
                 )
              )
              ((not (= 0 exit-status))
               (error "WakaTime Error (%s)" exit-status)
              )
             )
           )
         )
      )
    )

    (set-process-query-on-exit-flag process nil)
  )
)

(defun wakatime-ping ()
  "Send ping notice to WakaTime."
  (when (buffer-file-name (current-buffer))
    (wakatime-call nil)))

(defun wakatime-save ()
  "Send save notice to WakaTime."
  (when (buffer-file-name (current-buffer))
    (wakatime-call t)))

(defun wakatime-bind-hooks ()
  "Watch for activity in buffers."
  (add-hook 'after-save-hook 'wakatime-save nil t)
  (add-hook 'auto-save-hook 'wakatime-save nil t)
  (add-hook 'first-change-hook 'wakatime-ping nil t)
)

(defun wakatime-unbind-hooks ()
  "Stop watching for activity in buffers."
  (remove-hook 'after-save-hook 'wakatime-save t)
  (remove-hook 'auto-save-hook 'wakatime-save t)
  (remove-hook 'first-change-hook 'wakatime-ping t)
)

(defun wakatime-turn-on (defer)
  "Turn on WakaTime."
  (if defer
    (run-at-time "1 sec" nil 'wakatime-turn-on nil)
    (let ()
      (wakatime-init)
      (if wakatime-init-finished
        (wakatime-bind-hooks)
        (run-at-time "1 sec" nil 'wakatime-turn-on nil)
      )
    )
  )
)

(defun wakatime-turn-off ()
  "Turn off WakaTime."
  (wakatime-unbind-hooks)
)

(defun wakatime-validate-api-key (key)
  "Check if the provided key is a valid API key."

  (not (not (string-match "^[[:xdigit:]]\\{32\\}$"
                          (replace-regexp-in-string "-" "" key)))))

;;;###autoload
(define-minor-mode wakatime-mode
  "Toggle WakaTime (WakaTime mode)."
  :lighter    " waka"
  :init-value nil
  :global     nil
  :group      'wakatime
  (cond
    (noninteractive (setq wakatime-mode nil))
    (wakatime-mode (wakatime-turn-on t))
    (t (wakatime-turn-off))
  )
)

;;;###autoload
(define-globalized-minor-mode global-wakatime-mode wakatime-mode (lambda () (wakatime-mode 1)))

(provide 'wakatime-mode)
;;; wakatime-mode.el ends here
