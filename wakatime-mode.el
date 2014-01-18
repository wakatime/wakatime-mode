;;; wakatime-mode.el --- Automatic time tracking extension based on WakaTime

;; Copyright (C) 2013  Gabor Torok <gabor@20y.hu>

;; Author: Gabor Torok <gabor@20y.hu>
;; Keywords: calendar, comm
;; Version: 0.1

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

(defconst wakatime-user-agent "wakatime-mode")

(defconst wakatime-error-codes '((api . 102)))

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

(defcustom wakatime-python-bin "/usr/bin/python"
  "Path of Python binary."
  :type 'string
  :group 'wakatime)

(defun wakatime-client-command (savep)
  "Return client command executable and arguments.
Set SAVEP to non-nil for write action."
  (format "%s %s --file %s %s --plugin %s --key %s --time %.2f"
          wakatime-python-bin
          wakatime-cli-path
          (buffer-file-name (current-buffer))
          (if savep "--write" "")
          wakatime-user-agent
          wakatime-api-key
          (float-time)))

(defun wakatime-init ()
  (if (or (not wakatime-api-key)
          (string= "" wakatime-api-key))
      (let ((api-key (read-string "WakaTime API key: ")))
        (customize-save-variable 'wakatime-api-key api-key)))
  (if (or (not wakatime-cli-path)
          (not (file-exists-p wakatime-cli-path)))
      (let ((cli-path (read-string "WakaTime CLI script path: ")))
        (customize-save-variable 'wakatime-cli-path cli-path))))

(defun wakatime-call (command)
  "Call WakaTime COMMAND."
  (set-process-sentinel
   (start-process "Shell" (generate-new-buffer " *WakaTime messages*")
                  shell-file-name shell-command-switch command)
   (lambda (process signal)
     (when (memq (process-status process) '(exit signal))
       (let ((exit-status (process-exit-status process)))
         (cond
          ((= (cdr (assoc 'api wakatime-error-codes)) exit-status)
           (progn
             (global-wakatime-mode -1)
             (error "An error occured while connecting to WakaTime, check your API key! WakaTime mode has been disabled.")))
          ((< 0 exit-status)
           (progn
             (global-wakatime-mode -1)
             (error "Unexpected WakaTime error occured (code %s)! WakaTime mode has been disabled."
                    exit-status)))) 
         (kill-buffer (process-buffer process)))))))

(defun wakatime-ping ()
  "Send ping notice to WakaTime."
  (wakatime-call (wakatime-client-command nil)))

(defun wakatime-save ()
  "Send save notice to WakaTime."
  (wakatime-call (wakatime-client-command t)))

(defun wakatime-turn-on ()
  "Turn on WakaTime."
  (wakatime-init)
  (add-hook 'after-save-hook 'wakatime-save nil t)
  (add-hook 'auto-save-hook 'wakatime-save nil t)
  (add-hook 'first-change-hook 'wakatime-ping nil t))

(defun wakatime-turn-off ()
  "Turn off WakaTime."
  (remove-hook 'after-save-hook 'wakatime-save t)
  (remove-hook 'auto-save-hook 'wakatime-save t)
  (remove-hook 'first-change-hook 'wakatime-ping t))

;;;###autoload
(define-minor-mode wakatime-mode
  "Toggle WakaTime (WakaTime mode).
With a prefix argument ARG, enable Whitespace mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :lighter    " waka"
  :init-value nil
  :global     nil
  :group      'wakatime
  (cond
   (noninteractive
    (setq wakatime-mode nil))
   (wakatime-mode
    (wakatime-turn-on))
   (t
    (wakatime-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-wakatime-mode wakatime-mode (lambda () (wakatime-mode 1)))


(provide 'wakatime-mode)
;;; wakatime-mode.el ends here
