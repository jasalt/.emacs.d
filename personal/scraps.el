;; Not loaded

;; TODO rename to compose-yml or something as this is maybe needed still


;; Using docker-compose.yml for parsing project specific commands which might
;; be useful in some cases

(defun read-project-setting (setting-key)
  "Read a project setting from docker-compose.yml.
   Traverses up the filesystem from the current buffer's file path
   to find the first docker-compose.yml containing the given setting-key
   x-project-data directive."
  (let ((file-path (buffer-file-name))
        (root-dir "/")
        (setting-value nil))
    (while (and file-path (not (string= file-path root-dir))
				(not setting-value))
      (let ((docker-compose-path
			 (expand-file-name "docker-compose.yml" file-path)))
        (when (file-exists-p docker-compose-path)
          (let* ((yaml-data (yaml-parse-string
                             (with-temp-buffer
                               (insert-file-contents docker-compose-path)
                               (buffer-string))))
                 (custom-data (gethash 'x-project-data yaml-data)))
            (when custom-data
              (setq setting-value (gethash setting-key custom-data))
              (when setting-value
                (setq setting-value
					  (cons (file-name-directory docker-compose-path)
							setting-value))))))
        (setq file-path (file-name-directory (directory-file-name file-path)))))
    setting-value))

(defun read-project-command (setting-key)
  "Read a project command for the given 'setting-key' example:
   (read-project-command 'repl-command)
   (read-project-command 'test-command)"
  (let ((command-data (read-project-setting setting-key)))
    (when command-data
      (let ((project-path (car command-data))
            (command (cdr command-data)))
        (concat "cd " project-path " && " command)))))
