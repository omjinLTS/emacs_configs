(after! org
  (setq org-directory "~/org/")

  ;; 'org' setting for 'agenda'
  (setq org-agenda-files
        (list "todo.org"
              "projects.org"
              "events.org"))
  ;; 2. 캡처 템플릿 설정 (표준 문법)
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)

          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ;; ▼▼▼ [새로 추가한 부분] 일정(Event) 템플릿 ▼▼▼
          ("e" "Event/Appointment" entry
           (file+headline "events.org" "Calendar") ;; events.org의 Calendar 헤더 밑에
           "* %?\n%^T"                             ;; %^T: 날짜/시간 입력창 강제 팝업
           :prepend t)
          ))

  (setq org-log-done 'time)
)
