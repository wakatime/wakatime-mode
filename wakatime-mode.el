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

(defun wakatime-init ()
  (unless wakatime-init-started
    (setq wakatime-init-started t)
    (when (or (not wakatime-api-key) (string= "" wakatime-api-key))
      (wakatime-prompt-api-key)
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

(defun wakatime-client-command (savep)
  "Return client command executable and arguments.
   Set SAVEP to non-nil for write action."
  (format "%s %s --file \"%s\" %s --plugin %s/%s --key %s --time %.2f"
    wakatime-python-bin
    wakatime-cli-path
    (buffer-file-name (current-buffer))
    (if savep "--write" "")
    wakatime-user-agent
    wakatime-version
    wakatime-api-key
    (float-time)
  )
)

(defun wakatime-call (command)
  "Call WakaTime COMMAND."
  (let
    (
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
      (lambda (process signal)
        (when (memq (process-status process) '(exit signal))
          (kill-buffer (process-buffer process))
          (let ((exit-status (process-exit-status process)))
            (when (and (not (= 0 exit-status)) (not (= 102 exit-status)))
              (error "WakaTime Error (%s)" exit-status)
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
    (wakatime-call (wakatime-client-command nil))))

(defun wakatime-save ()
  "Send save notice to WakaTime."
  (when (buffer-file-name (current-buffer))
    (wakatime-call (wakatime-client-command t))))

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
