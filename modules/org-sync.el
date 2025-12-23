;;;; modules/org-sync.el -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------
;; 1. 함수 정의
;; ----------------------------------------------------------------

(defun my/org-git-pull ()
  "GitHub에서 최신 노트를 가져오고 버퍼를 새로고침합니다."
  (interactive)

  ;; [핵심] 실행하는 순간 Org 설정을 불러옴 -> org-directory 변수가 100% 생김
  (require 'org)

  ;; 변수가 확실히 있는지 확인 후 실행 (이중 안전장치)
  (if (and (boundp 'org-directory)
           (stringp org-directory)
           (file-directory-p org-directory))
      (let ((default-directory org-directory))
        (message "GitHub에서 최신 노트를 가져오는 중(Update)...")
        ;; 비동기 실행
        (set-process-sentinel
         (start-process "org-git-pull" nil "git" "pull" "origin" "main")
         (lambda (p e)
           (when (string= e "finished\n")
             (dolist (buf (buffer-list))
               (with-current-buffer buf
                 (when (and buffer-file-name
                            (string-prefix-p (expand-file-name org-directory)
                                             (expand-file-name buffer-file-name)))
                   (revert-buffer t t))))
             (message "업데이트(Pull) 완료!")))))
    ;; Org 경로가 없을 경우
    (message "⚠️ 설정을 확인하세요: org-directory가 없습니다.")))

;; [Push] 서버로 보내기 (수동 실행용 - 비동기)
(defun my/org-git-push ()
  "내 노트를 GitHub로 보냅니다."
  (interactive)
  (message "Org 노트를 GitHub로 보내는 중(Push)...")
  ;; 비동기 실행 (이맥스가 멈추지 않음)
  (async-shell-command
   (format "cd %s && git add . && git commit -m 'Auto-sync %s' && git push origin main > /dev/null 2>&1"
           org-directory
           (format-time-string "%Y-%m-%d %H:%M:%S")))
  (message "Push 명령을 보냈습니다!"))

;; [Push] 종료용 (자동 실행용 - 동기)
;; 주의: 종료할 때는 비동기(async)를 쓰면 전송되기 전에 이맥스가 꺼져버립니다.
;; 그래서 멈춰서 기다리는 동기(sync) 방식을 써야 합니다.
(defun my/org-git-push-on-exit ()
  (message "종료 전 노트를 백업(Push) 중입니다...")
  (let ((default-directory org-directory))
    (shell-command
     (format "git add . && git commit -m 'Auto-sync on Exit %s' && git push origin main > /dev/null 2>&1"
             (format-time-string "%Y-%m-%d %H:%M:%S")))))

;; ----------------------------------------------------------------
;; 2. 키바인딩 설정
;; ----------------------------------------------------------------

(map! :leader
      (:prefix "n"
        :desc "Push Note (보내기)"   "p" #'my/org-git-push
        :desc "Update Note (가져오기)" "u" #'my/org-git-pull))

;; ----------------------------------------------------------------
;; 3. 자동화 훅 (Hooks)
;; ----------------------------------------------------------------

;; [시작 시] 이맥스 켜지고 화면이 뜰 때 자동으로 Pull
(run-at-time "2 sec" nil #'my/org-git-pull)

;; [종료 시] 이맥스 끌 때(SPC q q) 자동으로 Push
(add-hook 'kill-emacs-hook #'my/org-git-push-on-exit);;
