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
  :prefix "wakatime-")

(defcustom wakatime-api-key nil
  "API key for WakaTime."
  :type 'string
  :group 'wakatime)

(defcustom wakatime-cli-path nil
  "Path of CLI client for WakaTime."
  :type 'string
  :group 'wakatime)

(defcustom wakatime-python-bin "python"
  "Path of Python binary."
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

(defun wakatime-guess-actual-script-path (path)
  (let ((true-path (if (null path) nil (file-truename path))))
    (let ((true-path (if (null true-path) path true-path)))
      (cond
        ((s-blank true-path) ; just return nil if path is null
          nil)
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
        ((file-exists-p (format "%s/cli.py" true-path)) ; point to cli.py from source code
            (format "%s/cli.py" true-path))
        ((file-exists-p (format "%s/wakatime/cli.py" true-path)) ; point to cli.py from source code inside wakatime package folder
            (format "%s/wakatime/cli.py" true-path))
        (t path)))))

(defun wakatime-init ()
  (unless wakatime-init-started
    (setq wakatime-init-started t)
    (when (s-blank wakatime-cli-path)
      (customize-set-variable 'wakatime-cli-path
        (wakatime-guess-actual-script-path (wakatime-find-binary "wakatime"))))
    (when (s-blank wakatime-cli-path)
      (wakatime-prompt-cli-path))
    (when (not (s-blank wakatime-cli-path))
      (if (not (string-match-p "cli\\.py$" wakatime-cli-path))
        (customize-set-variable 'wakatime-python-bin nil)
        (when (s-blank wakatime-python-bin)
          (wakatime-prompt-python-bin))))
    (setq wakatime-init-finished t)))

(defun wakatime-prompt-api-key ()
  "Prompt user for api key."
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((api-key (read-string "WakaTime API key: ")))
      (customize-set-variable 'wakatime-api-key api-key)
      (customize-save-customized))
    (setq wakatime-noprompt nil)))

(defun wakatime-prompt-cli-path ()
  "Prompt user for cli.py path."
  (when (and (= (recursion-depth) 0) (not wakatime-noprompt))
    (setq wakatime-noprompt t)
    (let ((cli-path (read-string "WakaTime CLI script path (wakatime/cli.py): ")))
      (customize-set-variable 'wakatime-cli-path cli-path)
      (customize-save-customized))
    (setq wakatime-noprompt nil)))

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
  nil)

(defun wakatime-python-exists (location)
  "Check if python exists in the specified path location."
  (= (condition-case nil (call-process location nil nil nil "--version") (error 1)) 0))

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
    ((not (s-blank (executable-find "wakatime")))
      (executable-find "wakatime"))
    (t program)))

(defun wakatime-client-command (savep)
  "Return client command executable and arguments.
   Set SAVEP to non-nil for write action."
  (format "%s%s--file \"%s\" --plugin \"%s/%s\" --time %.2f%s%s"
    (if (s-blank wakatime-python-bin) "" (format "\"%s\" " wakatime-python-bin))
    (if (s-blank wakatime-cli-path) "wakatime " (format "\"%s\" " wakatime-cli-path))
    (buffer-file-name (current-buffer))
    wakatime-user-agent
    wakatime-version
    (float-time)
    (if savep " --write" "")
    (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

(defun wakatime-call (savep)
  "Call WakaTime command."
  (let*
    (
      (command (wakatime-client-command savep))
      (process-environment (if wakatime-python-path (cons (format "PYTHONPATH=%s" wakatime-python-path) process-environment) process-environment))
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
             (when (and (not (= 0 exit-status)) (not (= 102 exit-status)))
               (when wakatime-disable-on-error
                 (wakatime-mode -1)
                 (global-wakatime-mode -1))
               (cond
                 ((= exit-status 103) (error "WakaTime Error (%s) Config file parse error. Check your ~/.wakatime.cfg file." exit-status))
                 ((= exit-status 104) (error "WakaTime Error (%s) Invalid API Key. Set your api key with: (custom-set-variables '(wakatime-api-key \"XXXX\"))" exit-status))
                 ((= exit-status 105) (error "WakaTime Error (%s) Unknown wakatime-cli error. Please check your ~/.wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                 ((= exit-status 106) (error "WakaTime Error (%s) Malformed heartbeat error. Please check your ~/.wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                 (t (error "WakaTime Error (%s) Make sure this command runs in a Terminal: %s" exit-status (wakatime-client-command nil)))
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
  (when (and
         (buffer-file-name (current-buffer))
         (not (auto-save-file-name-p (buffer-file-name (current-buffer)))))
    (wakatime-call nil)))

(defun wakatime-save ()
  "Send save notice to WakaTime."
  (when (and
         (buffer-file-name (current-buffer))
         (not (auto-save-file-name-p (buffer-file-name (current-buffer)))))
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
