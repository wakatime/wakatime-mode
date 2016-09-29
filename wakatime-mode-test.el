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

(ert-deftest wakatime-validate-api-key ()
  "Test API key validity checker `wakatime-validate-api-key`."

  (should
   (not
    (wakatime-validate-api-key "secret")))

  (should
   (not
    (wakatime-validate-api-key "1234567812341234123412345678901p")))

  (should
   (not
    (wakatime-validate-api-key "12345678-1234-1234-1234-12345678901p")))

  (should
   (wakatime-validate-api-key "1234567890abcdef1234567890abcdef"))

  (should
   (wakatime-validate-api-key "12345678-90ab-cdef-1234-567890abcdef"))

  (should
   (wakatime-validate-api-key "1-2-3-4-5-6-7-8-9-0-a-b-c-d-e-f-1-2-3-4-5-6-7-8-9-0-a-b-c-d-e-f")))
