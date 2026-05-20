;;; wakatime-mode.el --- Automatic time tracking extension for WakaTime

;; Copyright (C) 2013  Gabor Torok <gabor@20y.hu>

;; Author: Gabor Torok <gabor@20y.hu>
;; Maintainer: Alan Hamlett <alan@wakatime.com>
;; Website: https://wakatime.com
;; Keywords: calendar, comm
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; Set variable `wakatime-api-key' to your API key. By default
;; `wakatime-cli-path' is nil and wakatime-mode will look for an
;; existing `wakatime-cli' on your $PATH, otherwise auto-downloading the
;; latest release from
;; <https://github.com/wakatime/wakatime-cli/releases> into
;; `wakatime-resources-folder' (~/.wakatime/ by default). Update checks
;; run at most every `wakatime-update-check-interval' seconds. To pin a
;; specific binary, set `wakatime-cli-path' to its absolute path.

;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)

(defconst wakatime-version "1.1.0")
(defconst wakatime-user-agent "emacs-wakatime")
(defconst wakatime-github-download-url
  "https://github.com/wakatime/wakatime-cli/releases/latest/download"
  "Base URL for downloading the latest wakatime-cli release.")
(defconst wakatime-github-releases-api-url
  "https://api.github.com/repos/wakatime/wakatime-cli/releases/latest"
  "GitHub API URL for querying the latest wakatime-cli release tag.")
(defvar wakatime-noprompt nil)
(defvar wakatime-init-started nil)
(defvar wakatime-init-finished nil)
(defvar wakatime-install-in-progress nil
  "Non-nil while wakatime-cli is being downloaded or extracted.")

(defgroup wakatime nil
  "Customizations for WakaTime"
  :group 'convenience
  :prefix "wakatime-")

(defcustom wakatime-api-key nil
  "API key for WakaTime."
  :type 'string
  :group 'wakatime)

(defcustom wakatime-cli-path nil
  "Path of CLI client for WakaTime.
If nil or empty, wakatime-mode will look for an existing wakatime-cli on
the system and, if none is found, automatically download the latest
release from GitHub into `wakatime-resources-folder'."
  :type '(choice (const :tag "Auto-detect / download" nil) string)
  :group 'wakatime)

(defcustom wakatime-resources-folder
  (expand-file-name
    ".wakatime"
    (or (and (getenv "WAKATIME_HOME")
             (file-directory-p (getenv "WAKATIME_HOME"))
             (getenv "WAKATIME_HOME"))
        (getenv (if (memq system-type '(windows-nt cygwin ms-dos)) "USERPROFILE" "HOME"))
        "~"))
  "Folder where wakatime-cli is installed when auto-downloaded."
  :type 'directory
  :group 'wakatime)

(defcustom wakatime-update-check-interval (* 4 60 60)
  "Minimum number of seconds between checks for wakatime-cli updates.
Defaults to 4 hours, matching vscode-wakatime."
  :type 'integer
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
      (setq wakatime-cli-path (wakatime-locate-cli)))
    (setq wakatime-init-finished t)
    ;; Defer install/update so we don't block Emacs startup, and so any
    ;; HTTP work runs after the current command completes.
    (run-at-time 0 nil #'wakatime-check-and-install-cli)))

(defun wakatime-windows-p ()
  "Return non-nil when running on Windows."
  (memq system-type '(windows-nt cygwin ms-dos)))

(defun wakatime-os-name ()
  "Return the wakatime-cli OS identifier for the current system."
  (cond
    ((eq system-type 'darwin) "darwin")
    ((wakatime-windows-p) "windows")
    ((eq system-type 'gnu/linux) "linux")
    ((and (boundp 'system-configuration) (stringp system-configuration))
      (cond
        ((string-match-p "freebsd" system-configuration) "freebsd")
        ((string-match-p "openbsd" system-configuration) "openbsd")
        ((string-match-p "netbsd" system-configuration) "netbsd")
        (t (symbol-name system-type))))
    (t (symbol-name system-type))))

(defun wakatime-architecture ()
  "Return the wakatime-cli architecture identifier for the current system."
  (let ((cfg (or (and (boundp 'system-configuration) system-configuration) "")))
    (cond
      ((string-match-p "\\(aarch64\\|arm64\\)" cfg) "arm64")
      ((string-match-p "\\(x86_64\\|amd64\\)" cfg) "amd64")
      ((string-match-p "armv\\|\\barm\\b" cfg) "arm")
      ((string-match-p "\\(i[3456]86\\|x86\\)" cfg) "386")
      (t "amd64"))))

(defun wakatime-cli-binary-name ()
  "Return the basename of the wakatime-cli binary for this OS/arch."
  (format "wakatime-cli-%s-%s%s"
    (wakatime-os-name)
    (wakatime-architecture)
    (if (wakatime-windows-p) ".exe" "")))

(defun wakatime-expected-cli-path ()
  "Return the absolute path where wakatime-mode auto-installs wakatime-cli."
  (expand-file-name (wakatime-cli-binary-name) wakatime-resources-folder))

(defun wakatime-cli-download-url ()
  "Return the download URL for the latest wakatime-cli zip."
  (format "%s/wakatime-cli-%s-%s.zip"
    wakatime-github-download-url
    (wakatime-os-name)
    (wakatime-architecture)))

(defun wakatime-cli-location-global ()
  "Return path to a globally installed wakatime-cli, or nil."
  (executable-find (if (wakatime-windows-p) "wakatime-cli.exe" "wakatime-cli")))

(defun wakatime-locate-cli ()
  "Find an existing wakatime-cli, or return the expected auto-install path."
  (or (wakatime-cli-location-global)
      (let ((path (wakatime-expected-cli-path)))
        (when (file-exists-p path) path))
      (let ((legacy (wakatime-find-binary "wakatime-cli")))
        (when (and legacy
                   (not (string= legacy "wakatime-cli"))
                   (file-exists-p legacy))
          legacy))
      (wakatime-expected-cli-path)))

(defun wakatime-internal-cfg-file ()
  "Return the path of the wakatime-cli internal config file."
  (expand-file-name "wakatime-internal.cfg" wakatime-resources-folder))

(defun wakatime-read-cfg-setting (section key)
  "Read SECTION/KEY from the wakatime internal cfg file. Return string or nil."
  (let ((file (wakatime-internal-cfg-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((target (downcase section))
              (current "")
              (result nil))
          (while (and (not result) (not (eobp)))
            (let ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
              (cond
                ((string-match "^\\s-*\\[\\(.+\\)\\]\\s-*$" line)
                  (setq current (downcase (match-string 1 line))))
                ((and (string= current target)
                      (string-match
                        "^\\s-*\\([^=]+?\\)\\s-*=\\s-*\\(.*?\\)\\s-*$"
                        line)
                      (string= (match-string 1 line) key))
                  (setq result (match-string 2 line)))))
            (forward-line 1))
          result)))))

(defun wakatime-write-cfg-setting (section key value)
  "Write SECTION/KEY=VALUE into the wakatime internal cfg file."
  (let* ((file (wakatime-internal-cfg-file))
         (dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((existing (if (file-exists-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))
                      ""))
          (target (downcase section))
          (out '())
          (current "")
          (found nil))
      (dolist (line (split-string existing "\n"))
        (cond
          ((string-match "^\\s-*\\[\\(.+\\)\\]\\s-*$" line)
            (when (and (string= current target) (not found))
              (push (format "%s = %s" key value) out)
              (setq found t))
            (setq current (downcase (match-string 1 line)))
            (push line out))
          ((and (string= current target)
                (string-match "^\\s-*\\([^=]+?\\)\\s-*=" line)
                (string= (match-string 1 line) key))
            (unless found
              (push (format "%s = %s" key value) out)
              (setq found t)))
          (t (push line out))))
      (unless found
        (unless (string= current target)
          (push (format "[%s]" section) out))
        (push (format "%s = %s" key value) out))
      (with-temp-file file
        (insert (mapconcat #'identity (nreverse out) "\n"))))))

(defun wakatime-cli-current-version (cli-path)
  "Return the installed wakatime-cli version string, or nil."
  (when (and cli-path (file-executable-p cli-path))
    (with-temp-buffer
      ;; vscode-wakatime concatenates stdout+stderr because some builds
      ;; report version on stderr; mirror that by merging both streams.
      (let ((status (ignore-errors
                      (call-process cli-path nil '(t t) nil "--version"))))
        (when (and (integerp status) (zerop status))
          (let ((v (string-trim (buffer-string))))
            (and (not (string-empty-p v)) v)))))))

(defun wakatime-fetch-latest-version (callback)
  "Fetch latest wakatime-cli tag asynchronously; call CALLBACK with tag or nil."
  (let ((url-request-extra-headers
          '(("User-Agent" . "github.com/wakatime/wakatime-mode")
            ("Accept" . "application/vnd.github+json")))
        (url-show-status nil))
    (condition-case err
      (url-retrieve
        wakatime-github-releases-api-url
        (lambda (status)
          (let ((tag nil))
            (unwind-protect
              (unless (plist-get status :error)
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (let* ((json-object-type 'alist)
                         (data (ignore-errors (json-read))))
                    (when data
                      (setq tag (cdr (assq 'tag_name data)))))))
              (when (buffer-live-p (current-buffer))
                (kill-buffer (current-buffer))))
            (wakatime-write-cfg-setting
              "internal" "cli_version_last_accessed"
              (number-to-string (floor (float-time))))
            (funcall callback tag)))
        nil t t)
      (error
        (wakatime-debug
          (format "WakaTime: failed to query GitHub releases: %s"
            (error-message-string err)))
        (funcall callback nil)))))

(defun wakatime-unzip (zip-file output-dir)
  "Extract ZIP-FILE into OUTPUT-DIR. Return non-nil on success."
  (unless (file-directory-p output-dir)
    (make-directory output-dir t))
  (let ((status
          (cond
            ((executable-find "unzip")
              (call-process "unzip" nil nil nil "-o" zip-file "-d" output-dir))
            ((executable-find "tar")
              (call-process "tar" nil nil nil "-xf" zip-file "-C" output-dir))
            ((wakatime-windows-p)
              (call-process "powershell" nil nil nil "-NoProfile" "-Command"
                (format "Expand-Archive -Force -Path '%s' -DestinationPath '%s'"
                  zip-file output-dir)))
            (t (error "WakaTime: no unzip/tar available to extract %s" zip-file)))))
    (and (integerp status) (zerop status))))

(defun wakatime--download-binary (url output-file callback)
  "Download URL to OUTPUT-FILE asynchronously. CALLBACK is called with success bool."
  (let ((url-show-status nil))
    (condition-case err
      (url-retrieve
        url
        (lambda (status)
          (let ((ok nil))
            (unwind-protect
              (unless (plist-get status :error)
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (let ((coding-system-for-write 'binary)
                        (write-region-annotate-functions nil)
                        (write-region-post-annotation-function nil))
                    (write-region (point) (point-max) output-file nil 'quiet))
                  (setq ok t)))
              (when (buffer-live-p (current-buffer))
                (kill-buffer (current-buffer))))
            (funcall callback ok)))
        nil t t)
      (error
        (wakatime-debug
          (format "WakaTime: download failed for %s: %s"
            url (error-message-string err)))
        (funcall callback nil)))))

(defun wakatime--finish-install (zip-file cli-path)
  "Extract ZIP-FILE, install wakatime-cli at CLI-PATH, return non-nil on success."
  (let* ((dir wakatime-resources-folder)
         (backup (concat cli-path ".backup"))
         (had-existing (file-exists-p cli-path))
         (ok nil))
    (unwind-protect
      (condition-case err
        (progn
          (when had-existing
            (when (file-exists-p backup) (delete-file backup))
            (rename-file cli-path backup t))
          (if (not (wakatime-unzip zip-file dir))
            (progn
              (wakatime-debug "WakaTime: failed to extract wakatime-cli zip")
              (when (and had-existing (file-exists-p backup))
                (rename-file backup cli-path t)))
            (when (file-exists-p cli-path)
              (unless (wakatime-windows-p)
                (set-file-modes cli-path #o755)
                (let ((link (expand-file-name "wakatime-cli" dir)))
                  (unless (string= (file-truename link) (file-truename cli-path))
                    (when (or (file-exists-p link) (file-symlink-p link))
                      (delete-file link))
                    (condition-case _
                      (make-symbolic-link cli-path link t)
                      (error
                        (condition-case copy-err
                          (progn
                            (copy-file cli-path link t t)
                            (set-file-modes link #o755))
                          (error
                            (wakatime-debug
                              (format "WakaTime: could not link wakatime-cli: %s"
                                (error-message-string copy-err))))))))))
              (when (and had-existing (file-exists-p backup))
                (delete-file backup))
              (setq ok t))))
        (error
          (wakatime-debug
            (format "WakaTime: error installing wakatime-cli: %s"
              (error-message-string err)))
          (when (and had-existing
                     (not (file-exists-p cli-path))
                     (file-exists-p backup))
            (ignore-errors (rename-file backup cli-path t)))))
      (when (file-exists-p zip-file)
        (ignore-errors (delete-file zip-file))))
    ok))

(defun wakatime-install-cli (&optional callback)
  "Download and install the latest wakatime-cli. CALLBACK gets called with success bool."
  (if wakatime-install-in-progress
    (when callback (funcall callback nil))
    (setq wakatime-install-in-progress t)
    (let* ((dir wakatime-resources-folder)
           (cli-path (wakatime-expected-cli-path))
           (zip-file (expand-file-name
                       (format "wakatime-cli-%d.zip"
                         (random (max 1 (expt 2 24))))
                       dir))
           (url (wakatime-cli-download-url)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (wakatime-debug (format "WakaTime: downloading wakatime-cli from %s" url))
      (wakatime--download-binary
        url zip-file
        (lambda (downloaded)
          (unwind-protect
            (let ((ok (and downloaded
                           (file-exists-p zip-file)
                           (> (file-attribute-size (file-attributes zip-file)) 0)
                           (wakatime--finish-install zip-file cli-path))))
              (if ok
                (wakatime-debug "WakaTime: wakatime-cli installed")
                (wakatime-debug "WakaTime: failed to install wakatime-cli"))
              (when callback (funcall callback ok)))
            (setq wakatime-install-in-progress nil)))))))

(defun wakatime-cli-managed-p (path)
  "Return non-nil when PATH is the wakatime-mode auto-managed install location."
  (and path
       (file-name-absolute-p path)
       (string= (file-truename path)
                (file-truename (wakatime-expected-cli-path)))))

;;;###autoload
(defun wakatime-update-cli ()
  "Force an immediate check for wakatime-cli updates, ignoring the rate-limit."
  (interactive)
  (wakatime-write-cfg-setting "internal" "cli_version_last_accessed" "0")
  (when (s-blank wakatime-cli-path)
    (setq wakatime-cli-path (wakatime-locate-cli)))
  (wakatime-check-and-install-cli))

(defun wakatime-check-and-install-cli ()
  "Ensure wakatime-cli is installed and reasonably up to date.
Only the auto-installed binary at `wakatime-expected-cli-path' is updated;
globally installed or user-pinned binaries are left alone."
  (let* ((expected (wakatime-expected-cli-path))
         (cli-path (or wakatime-cli-path expected)))
    (cond
      ((not (wakatime-cli-managed-p cli-path))
        ;; User-pinned or globally installed; don't touch it.
        (unless (file-exists-p cli-path)
          (wakatime-debug
            (format "WakaTime: wakatime-cli not found at %s" cli-path))))
      ((not (file-exists-p cli-path))
        (wakatime-install-cli
          (lambda (ok)
            (when (and ok (s-blank wakatime-cli-path))
              (setq wakatime-cli-path expected)))))
      (t
        (let* ((now (floor (float-time)))
               (raw (wakatime-read-cfg-setting "internal" "cli_version_last_accessed"))
               (last (and raw (ignore-errors (string-to-number raw)))))
          (when (or (null last)
                    (not (numberp last))
                    (> (- now last) wakatime-update-check-interval))
            (let ((current (wakatime-cli-current-version cli-path)))
              (cond
                ((null current)
                  (wakatime-install-cli))
                ((string= current "<local-build>")
                  nil)
                (t
                  (wakatime-fetch-latest-version
                    (lambda (latest)
                      (when (and latest
                                 (not (string= current latest))
                                 (not (string= (concat "v" current) latest)))
                        (wakatime-debug
                          (format "WakaTime: updating wakatime-cli %s -> %s"
                            current latest))
                        (wakatime-install-cli)))))))))))))

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
  (format "%s --entity %s --plugin \"%s/%s\" --time %.2f%s%s"
    (if (s-blank wakatime-cli-path)
      "wakatime-cli"
      (shell-quote-argument wakatime-cli-path))
    (shell-quote-argument (or file (buffer-file-name (current-buffer))))
    wakatime-user-agent
    wakatime-version
    (float-time)
    (if savep " --write" "")
    (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

(defun wakatime-cli-ready-p ()
  "Return non-nil when wakatime-cli is installed and runnable."
  (and (not wakatime-install-in-progress)
       (not (s-blank wakatime-cli-path))
       (file-exists-p wakatime-cli-path)))

(defun wakatime-call (savep)
  "Call WakaTime command."
  (when (wakatime-cli-ready-p)
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
  )))

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
