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
;; `wakatime-cli-path' to the absolute path of the CLI binary available
;; from <https://github.com/wakatime/wakatime-cli/releases>.

;;; Code:

(defconst wakatime-version "1.0.2")
(defconst wakatime-user-agent "emacs-wakatime")
(defvar wakatime-noprompt nil)
(defvar wakatime-init-started nil)
(defvar wakatime-init-finished nil)

(defgroup wakatime nil
  "Customizations for WakaTime"
  :group 'convenience
  :prefix "wakatime-")

(defcustom wakatime-api-key nil
  "API key for WakaTime."
  :type 'string
  :group 'wakatime)

(defcustom wakatime-cli-path nil
  "Path of CLI client for WakaTime."
  :type 'string
  :group 'wakatime)

(defcustom wakatime-disable-on-error nil
  "Turn off wakatime-mode and wakatime-global-mode when errors in
the wakatime subprocess occurs."
  :type 'boolean
  :group 'wakatime)


(defun wakatime-debug (msg)
  "Write a string to the *messages* buffer."
  (message "%s" msg))

(defun s-blank (string)
  "Return true if the string is empty or nil. Expects string."
  (or (null string)
    (zerop (length string))))

(defun wakatime-init ()
  (unless wakatime-init-started
    (setq wakatime-init-started t)
    (when (s-blank wakatime-cli-path)
      (customize-set-variable 'wakatime-cli-path
	(wakatime-find-binary "wakatime-cli")))
    (when (s-blank wakatime-cli-path)
      (wakatime-prompt-cli-path))
    (setq wakatime-init-finished t)))

(defun wakatime-prompt-api-key ()
  "Prompt user for api key."
  (interactive)
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((api-key (read-string "WakaTime API key: ")))
      (customize-set-variable 'wakatime-api-key api-key)
      (customize-save-customized))
    (setq wakatime-noprompt nil)))

(defun wakatime-prompt-cli-path ()
  "Prompt user for wakatime-cli binary path."
  (interactive)
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((cli-path (read-string "WakaTime CLI binary path: ")))
      (customize-set-variable 'wakatime-cli-path cli-path)
      (customize-save-customized))
    (setq wakatime-noprompt nil)))

(defun wakatime-find-binary (program)
  "Find the full path to an executable program."
  (cond
    ((file-exists-p (format "/usr/local/bin/%s" program))
      (format "/usr/local/bin/%s" program))
    ((file-exists-p (format "/usr/bin/%s" program))
      (format "/usr/bin/%s" program))
    ((file-exists-p (format "/bin/%s" program))
      (format "/bin/%s" program))
    ((file-exists-p (format "/usr/local/sbin/%s" program))
      (format "/usr/local/sbin/%s" program))
    ((file-exists-p (format "/usr/sbin/%s" program))
      (format "/usr/sbin/%s" program))
    ((file-exists-p (format "/sbin/%s" program))
      (format "/sbin/%s" program))
    ;; For linux users
    ((file-exists-p "~/.wakatime/wakatime-cli")
      "~/.wakatime/wakatime-cli")
    ;; For windows 10+ fix to get wakatime-cli.exe
    ((file-exists-p (concat
		(string-replace "\\" "/" (concat
		(substitute-env-vars "$HOMEDRIVE")
		(substitute-env-vars "$HOMEPATH")))
		(format "/.wakatime/%s" program)))
      (concat (string-replace "\\" "/" (concat
		(substitute-env-vars "$HOMEDRIVE")
		(substitute-env-vars "$HOMEPATH")))
		(format "/.wakatime/%s" program)))
    ;; For windows 10+ fix to get wakatime-cli-amd64.exe
    ((file-exists-p (concat
		(string-replace "\\" "/" (concat
		(substitute-env-vars "$HOMEDRIVE")
		(substitute-env-vars "$HOMEPATH")))
		"/.wakatime/wakatime-cli-windows-amd64.exe"))
      (concat (string-replace "\\" "/" (concat
		(substitute-env-vars "$HOMEDRIVE")
		(substitute-env-vars "$HOMEPATH")))
		"/.wakatime/wakatime-cli-windows-amd64.exe"))
    ((not (s-blank (executable-find "wakatime")))
      (executable-find "wakatime"))
    (t program)))

(defun wakatime-client-command (savep &optional file)
  "Return client command executable and arguments.
   Set SAVEP to non-nil for write action."
  (format "%s--entity %s --plugin \"%s/%s\" --time %.2f --lineno %d --lines-in-file %d%s%s"
    (if (s-blank wakatime-cli-path) "wakatime-cli " (format "%s " wakatime-cli-path))
    (shell-quote-argument (or file (buffer-file-name (current-buffer))))
    wakatime-user-agent
    wakatime-version
    (float-time)
    (line-number-at-pos)
    (count-lines (point-min) (point-max))
    (if savep " --write" "")
    (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

(defun wakatime-call (savep)
  "Call WakaTime command."
  (let*
    (
      (command (wakatime-client-command savep))
      (coding-system-for-read
       (unless (eq coding-system-for-read 'auto-save-coding)
       coding-system-for-read))
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
             (when (and (not (= 0 exit-status)) (not (= 102 exit-status)) (not (= 112 exit-status)))
               (when wakatime-disable-on-error
                 (wakatime-mode -1)
                 (global-wakatime-mode -1))
               (cond
                 ((= exit-status 103) (error "WakaTime Error (%s) Config file parse error. Check your ~/.wakatime.cfg file." exit-status))
                 ((= exit-status 104) (error "WakaTime Error (%s) Invalid API Key. Set your api key with: (custom-set-variables '(wakatime-api-key \"XXXX\"))" exit-status))
                 ((= exit-status 105) (error "WakaTime Error (%s) Unknown wakatime-cli error. Please check your ~/.wakatime/wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                 ((= exit-status 106) (error "WakaTime Error (%s) Malformed heartbeat error. Please check your ~/.wakatime/wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                 (t (error "WakaTime Error (%s) Make sure this command runs in a Terminal: %s" exit-status (wakatime-client-command nil ,(buffer-file-name (current-buffer)))))
               )
             )
           )
         )
      )
    )

    (set-process-query-on-exit-flag process nil)
  ))

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
  (add-hook 'first-change-hook 'wakatime-ping nil t))

(defun wakatime-unbind-hooks ()
  "Stop watching for activity in buffers."
  (remove-hook 'after-save-hook 'wakatime-save t)
  (remove-hook 'auto-save-hook 'wakatime-save t)
  (remove-hook 'first-change-hook 'wakatime-ping t))

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
  ))

(defun wakatime-turn-off ()
  "Turn off WakaTime."
  (wakatime-unbind-hooks))

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
  ))

;;;###autoload
(define-globalized-minor-mode global-wakatime-mode wakatime-mode (lambda () (wakatime-mode 1)))

(provide 'wakatime-mode)
;;; wakatime-mode.el ends here
