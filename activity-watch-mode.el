;;; activity-watch-mode.el --- Automatic time tracking extension. -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Gabor Torok <gabor@20y.hu>

;; Author: Gabor Torok <gabor@20y.hu>, Alan Hamlett <alan@wakatime.com>
;; Maintainer: Paul d'Hubert <paul.dhubert@ya.ru>
;; Website: https://activitywatch.net
;; Homepage: https://github.com/pauldub/activity-watch-mode
;; Keywords: calendar, comm
;; Package-Requires: ((emacs "24") (projectile "0") (request "0"))
;; Version: 1.0.0

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

;; ActivityWatch mode based on https://github.com/wakatime/wakatime-mode
;;
;; Enable Activity-Watch for the current buffer by invoking
;; `activity-watch-mode'.  If you wish to activate it globally, use
;; `global-activity-watch-mode'.
;;
;; Requires request.el (https://tkf.github.io/emacs-request/)
;;

;;; Dependencies: request, projectile

;;; Code:

(require 'ert)
(require 'request)
(require 'json)
(require 'cl)

(defconst activity-watch-version "1.0.0")
(defconst activity-watch-user-agent "emacs-activity-watch")
(defvar activity-watch-noprompt nil)
(defvar activity-watch-timer nil)
(defvar activity-watch-idle-timer nil)
(defvar activity-watch-init-started nil)
(defvar activity-watch-init-finished nil)
(defvar activity-watch-bucket-created nil)
(defvar activity-watch-last-file-path nil)
(defvar activity-watch-pulse-time 30)
(defvar activity-watch-max-heartbeat-per-sec 1)
(defvar activity-watch-last-heartbeat-time nil)

(defgroup activity-watch nil
  "Customizations for Activity-Watch"
  :group 'convenience
  :prefix "activity-watch-")

(defcustom activity-watch-api-host "http://localhost:5600"
  "API host for Activity-Watch."
  :type 'string
  :group 'activity-watch)

(defun activity-watch--s-blank (string)
  "Return non-nil if the STRING is empty or nil.  Expects string."
  (or (null string)
      (zerop (length string))))

(defun activity-watch--init ()
  "Initialize symbol ‘activity-watch-mode’."
  (unless activity-watch-init-started
    (setq activity-watch-init-started t)
    (setq activity-watch-init-finished t)))

(defun activity-watch--bucket-id ()
  "Return the bucket-id to be used when submitting heartbeats."
  (concat "aw-watcher-emacs_" (system-name)))

(defun activity-watch--create-bucket ()
  "Create the editor bucket."
  (when (not activity-watch-bucket-created)
    (request (concat activity-watch-api-host "/api/0/buckets/" (activity-watch--bucket-id))
             :type "POST"
             :data (json-encode `((hostname . ,(system-name))
                                  (client . ,activity-watch-user-agent)
                                  (type . "app.editor.activity")))
             :headers '(("Content-Type" . "application/json"))
             :success (function*
                       (lambda (&allow-other-keys)
                         (setq activity-watch-bucket-created t))))))

(defun activity-watch--create-heartbeat (time)
  "Create heartbeart to sent to the activity watch server.
Argument TIME time at which the heartbeat was computed."
  (let ((project-name (projectile-project-name))
        (file-name (buffer-file-name (current-buffer))))
    `((timestamp . ,(ert--format-time-iso8601 time))
      (duration . 0)
      (data . ((language . ,(if (activity-watch--s-blank (symbol-name major-mode)) "unknown" major-mode))
               (project . ,(if (activity-watch--s-blank project-name) "unknown" project-name))
               (file . ,(if (activity-watch--s-blank file-name) "unknown" file-name)))))))


(defun activity-watch--send-heartbeat (heartbeat)
  "Send HEARTBEAT to activity watch server."
  (request (concat activity-watch-api-host "/api/0/buckets/" (activity-watch--bucket-id) "/heartbeat")
           :type "POST"
           :params `(("pulsetime" . ,activity-watch-pulse-time))
           :data (json-encode heartbeat)
           :headers '(("Content-Type" . "application/json"))
           :error (function*
                   (lambda (&key data &allow-other-keys)
                     (message data)))))

(defun activity-watch--call ()
  "Conditionally submit heartbeat to activity watch."
  (activity-watch--create-bucket)
  (let ((now (float-time))
        (current-file-path (buffer-file-name (current-buffer)))
        (time-delta (+ (or activity-watch-last-heartbeat-time 0) activity-watch-max-heartbeat-per-sec)))
    (if (or (not (string= (or activity-watch-last-file-path "") current-file-path))
            (< time-delta now))
        (progn
          (setq activity-watch-last-file-path current-file-path)
          (setq activity-watch-last-heartbeat-time now)
          (activity-watch--send-heartbeat (activity-watch--create-heartbeat (current-time)))))))

(defun activity-watch--save ()
  "Send save notice to Activity-Watch."
  (when (and (buffer-file-name (current-buffer))
             (not (auto-save-file-name-p (buffer-file-name (current-buffer)))))
    (activity-watch--call)))

(defun activity-watch--start-timer ()
  "Start timers for heartbeat submission and idling."
  (unless activity-watch-timer
      (setq activity-watch-timer (run-at-time t 2 #'activity-watch--save)))
  (unless activity-watch-idle-timer
      ;; stop the timer after 30s inactivity
      (setq activity-watch-idle-timer (run-with-idle-timer 30 t #'activity-watch--stop-timer))))

(defun activity-watch--stop-timer ()
  "Stop heartbeat submission timer."
  (when activity-watch-timer
    (cancel-timer activity-watch-timer)
    (setq activity-watch-timer nil)))

(defun activity-watch--stop-idle-timer ()
  "Stop idling timer."
  (when activity-watch-idle-timer
    (cancel-timer activity-watch-idle-timer)
    (setq activity-watch-idle-timer nil)))

(defun activity-watch--bind-hooks ()
  "Watch for activity in buffers."
  (add-hook 'pre-command-hook #'activity-watch--start-timer nil t)
  (add-hook 'after-save-hook #'activity-watch--save nil t)
  (add-hook 'auto-save-hook #'activity-watch--save nil t)
  (add-hook 'first-change-hook #'activity-watch--save nil t))

(defun activity-watch--unbind-hooks ()
  "Stop watching for activity in buffers."
  (remove-hook 'pre-command-hook #'activity-watch--start-timer t)
  (remove-hook 'after-save-hook #'activity-watch--save t)
  (remove-hook 'auto-save-hook #'activity-watch--save t)
  (remove-hook 'first-change-hook #'activity-watch--save t))

(defun activity-watch-turn-on (defer)
  "Turn on Activity-Watch.
Argument DEFER Wether initialization should be deferred."
  (if defer
      (run-at-time "1 sec" nil #'activity-watch-turn-on nil)
    (progn
      (activity-watch--init)
      (if activity-watch-init-finished
          (progn (activity-watch--bind-hooks) (activity-watch--start-timer))
        (run-at-time "1 sec" nil #'activity-watch-turn-on nil)))))

(defun activity-watch-turn-off ()
  "Turn off Activity-Watch."
  (activity-watch--unbind-hooks)
  (activity-watch--stop-timer)
  (activity-watch--stop-idle-timer))

;;;###autoload
(define-minor-mode activity-watch-mode
  "Toggle Activity-Watch (Activity-Watch mode)."
  :lighter    " activity-watch"
  :init-value nil
  :global     nil
  :group      'activity-watch
  (cond
   (noninteractive (setq activity-watch-mode nil))
   (activity-watch-mode (activity-watch-turn-on t))
   (t (activity-watch-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-activity-watch-mode
  activity-watch-mode
  (lambda () (activity-watch-mode 1))
  :require 'activity-watch-mode)

(provide 'activity-watch-mode)
;;; activity-watch-mode.el ends here
