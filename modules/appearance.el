;; -----------------------------------------------------
;; 폰트 설정 (영문/한글 이원화 전략)
;; -----------------------------------------------------

;; 1. 메인 폰트 (영문 & 코드용) - JetBrains Mono 추천
;; :size는 본인 모니터에 맞춰 조절 (14 ~ 18 추천)
(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'normal)
      ;; 비-코드 영역(Org-mode 제목 등)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 16))

;; 2. 한글 폰트 별도 지정 (영문 폰트에는 한글이 없으므로 비상용으로 지정)
;; 영문 폰트가 처리 못하는 'hangul' 범위만 D2Coding이 담당합니다.
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))

;; 3. 한글 입력기 설정 (기존 유지)
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
;; -----------------------------------------------------
;; Org-mode 디자인 업그레이드 (Pretty UI)
;; -----------------------------------------------------

(after! org
  ;; 1. 강조 기호 숨기기 (/italic/, *bold* 등에서 기호 안 보이게 함)
  (setq org-hide-emphasis-markers t)

  ;; 2. 목록 기호를 점(•)으로 예쁘게 바꾸기
  (setq org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))

  ;; 3. 제목(Heading) 폰트 크기 조정
  ;; 레벨 1은 1.4배, 레벨 2는 1.2배 크기로 설정
  (custom-set-faces!
    '(org-level-1 :inherit outline-1 :weight extra-bold :height 1.4)
    '(org-level-2 :inherit outline-2 :weight bold :height 1.2)
    '(org-level-3 :inherit outline-3 :weight bold :height 1.1)
    '(org-level-4 :inherit outline-4 :weight bold :height 1.0)
    ;; 문서 제목(Title)도 1.5배로 키움
    '(org-document-title :height 1.5 :weight bold)))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-themes-enable-bold t    ; 볼드체 켜기
      doom-themes-enable-italic t) ; 이탤릭체 켜기
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; 1. 명언 리스트 정의
(defvar my-lab-quotes
  '("Talk is cheap. Show me the code. \n- Linus Torvalds"
    "Premature optimization is the root of all evil. \n- Donald Knuth"
    "The best way to predict the future is to invent it. \n- Alan Kay"
    "First, solve the problem. Then, write the code. \n- John Johnson"
    "It works on my machine. \n- Anonymous Developer"
    "Simplicity is the ultimate sophistication. \n- Leonardo da Vinci"
    "Real programmers don't comment their code. \nIf it was hard to write, it should be hard to understand."
    ))

;; 2. 명언 위젯 함수 정의
(defun my-quote-widget ()
  ;; 1. 명언 리스트가 잘 있는지 확인하고, 없으면 기본 문구 사용 (안전장치)
  (let* ((quotes (if (and (boundp 'my-lab-quotes) my-lab-quotes)
                     my-lab-quotes
                   '("System Ready. (No quotes loaded)"))) ;; 비상용 문구
         (quote (nth (random (length quotes)) quotes))
         ;; 2. 혹시라도 뽑은 게 문자가 아니면 문자열로 변환
         (quote-str (if (stringp quote) quote (format "%s" quote)))
         (lines (split-string quote-str "\n"))
         (width (window-width)))

    (insert "\n\n")
    (dolist (line lines)
      (let ((margin (max 0 (/ (- width (length line)) 2))))
        (insert (make-string margin ?\s))
        (insert (propertize line 'face 'font-lock-comment-face) "\n")))))

(defun my-newline ()
  (insert "\n"))
;; 3. 대시보드 화면 구성 요소 재정의 (가장 확실한 방법)
;; 복잡하게 훅을 넣고 빼지 말고, "그냥 이 목록대로 그려라"고 지정합니다.
(setq +doom-dashboard-functions
      '(my-quote-widget                  ;; 1. 명언 (맨 위)
        my-newline    ;; 공백
        doom-dashboard-widget-shortmenu  ;; 2. 메뉴 (중간)
        my-newline    ;; 공백
        doom-dashboard-widget-loaded))   ;; 3. 로딩 시간 (맨 아래)
