(defconst wakatime-user-agent "wakatime-mode")

(defgroup wakatime nil
  "Customizations for WakaTime"
  :group 'convenience
  :prefix "wakatime-")

(defcustom wakatime-api-key nil
  "API key for WakaTime"
  :type 'string
  :group 'wakatime)

(defcustom wakatime-client-path nil
  "Path of CLI client for WakaTime"
  :type 'string
  :group 'wakatime)

(defun wakatime-client-command (savep)
  (format "python %s --file %s %s --plugin %s --key %s"
          wakatime-client-path
          (buffer-file-name (current-buffer))
          (if savep "--write" "")
          wakatime-user-agent
          wakatime-api-key))

(defun wakatime-ping ()
  (shell-command
   (wakatime-client-command nil)))

(defun wakatime-save ()
  (shell-command
   (wakatime-client-command t)))

(provide 'wakatime-mode)
