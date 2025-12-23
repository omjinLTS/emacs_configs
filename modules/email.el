;; -----------------------------------------------------
;; 이메일 설정 (mu4e)
;; -----------------------------------------------------

;; 내 이메일 계정 등록
(set-email-account! "Gmail"
  '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
    (mu4e-drafts-folder     . "/[Gmail]/Drafts")
    (mu4e-trash-folder      . "/[Gmail]/Trash")
    (mu4e-refile-folder     . "/[Gmail]/All Mail")
    (smtpmail-smtp-user     . "omjin7g@gmail.com") ; 본인 이메일
    (user-mail-address      . "omjin7g@gmail.com") ; 본인 이메일
    (mu4e-compose-signature . "--\n오명진 올림\nomjin7g@gmail.com\nomjin7n@cau.ac.kr\n")
    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo")) ; 서명
    )
  t)

;; 메일 보내기 설정 (SMTP)
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

;; -----------------------------------------------------
;; Org 이메일 변환 설정 (내용 사라짐 방지)
;; -----------------------------------------------------

(use-package! org-msg
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi %s,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t)
  (org-msg-mode))
(after! mu4e
  ;; 1. 메일 가져오는 명령어 지정 (mbsync 사용 시)
  (setq mu4e-get-mail-command "mbsync -a")

  ;; 2. 자동 업데이트 주기 설정 (초 단위)
  ;; 예: 300초 = 5분마다 메일 가져오고 인덱싱함
  (setq mu4e-update-interval 300)

  ;; 3. (선택) 하단 모델라인에 "메일 가져오는 중" 표시 끄기 (거슬리면)
  ;; (setq mu4e-modeline-support nil)
)
(setq mu4e-sent-messages-behavior 'delete)
