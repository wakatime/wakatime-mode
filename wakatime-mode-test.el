(require 'ert)
(require 'wakatime-mode)

(ert-deftest wakatime-client-command ()
  "Test cases for function `wakatime-client-command'."
  (let ((wakatime-api-key "secret")
        (wakatime-cli-path "client")
        (wakatime-user-agent "wakatime-mode"))
    (should
     (equal (wakatime-client-command t) "/usr/bin/python client --file nil --write --plugin wakatime-mode --key secret"))
    (should
     (equal (wakatime-client-command nil) "/usr/bin/python client --file nil  --plugin wakatime-mode --key secret"))))
