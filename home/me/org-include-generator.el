(require 'org)
(require 'cl-lib)

;; Add org-roam dependency if available
(when (require 'org-roam nil 'noerror)
  (message "Org-roam support enabled"))

(defvar org-include-default-header "Other"
  "Default header for files without a title.")

(defun org-include-get-file-title (file)
  "Extract the #+TITLE from FILE.
Returns the title or the filename if no title is found."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
        (string-trim (match-string 1))
      ;; If no title found, use the filename without extension
      (file-name-base file))))

(defun org-include-resolve-id-link (id)
  "Resolve an org-roam ID link to a file path.
Returns nil if ID cannot be resolved or org-roam is not available."
  (cond
   ;; If org-roam is available, use its functions
   ((fboundp 'org-roam-id-find-file)
    (org-roam-id-find-file id))
   
   ;; If org-id is available, try to use that
   ((fboundp 'org-id-find-id-file)
    (org-id-find-id-file id))
   
   ;; Manual fallback method if neither is available
   (t
    (let ((org-agenda-files (directory-files-recursively 
                             (file-name-directory buffer-file-name) 
                             "\\.org$"))
          result)
      (catch 'found
        (dolist (file org-agenda-files)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (search-forward (format ":ID:%s%s" 
                                         (if (string-match-p "^\\s-+" id) "" " ")
                                         id) nil t)
              (setq result file)
              (throw 'found file)))))
      result))))

(defun org-include-get-links-in-file (file)
  "Extract all org links (including ID links) in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((links '()))
      ;; Process standard file links
      (while (re-search-forward org-link-any-re nil t)
        (let ((link (match-string-no-properties 0)))
          (when (string-match org-link-bracket-re link)
            (let ((path (match-string 1 link)))
              (when (and path (string-match "^\\./.*\\.org" path))
                (push (expand-file-name path (file-name-directory file)) links))))))
      
      ;; Process ID links (need to go back to beginning)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
        (let* ((id (match-string-no-properties 1))
               (resolved-file (org-include-resolve-id-link id)))
          (when resolved-file
            (push resolved-file links))))
      
      links)))

(defun org-include-ensure-header (header)
  "Ensure that a header with name HEADER exists in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s$" (regexp-quote header)) nil t)
      (goto-char (point-max))
      (insert (format "\n\n* %s\n" header)))))

(defun org-include-add-include (file header)
  "Add an #+INCLUDE statement for FILE under HEADER."
  (org-include-ensure-header header)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote header)) nil t)
    (end-of-line)
    ;; Use relative path for the include and add only-contents parameter
    (let ((relative-file (file-relative-name file (file-name-directory buffer-file-name))))
      (insert (format "\n#+INCLUDE: \"%s\" :only-contents t" relative-file)))))

(defun org-include-file-already-included-p (file)
  "Check if FILE is already included in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((relative-file (file-relative-name file (file-name-directory buffer-file-name))))
      (re-search-forward (format "^#\\+INCLUDE: \"%s\" :only-contents t" 
                                 (regexp-quote relative-file)) 
                         nil t))))

(defun org-include-traverse-links-and-include (start-file)
  "Traverse links in START-FILE and include all connected files.
Each file will be included under its own header based on its #+TITLE."
  (let ((visited-files (list (expand-file-name start-file)))
        (files-to-process (list (expand-file-name start-file)))
        (headers-for-files '()))
    
    ;; Process all files reachable from start
    (while files-to-process
      (let ((current-file (pop files-to-process)))
        ;; Get links in the current file
        (dolist (link (org-include-get-links-in-file current-file))
          (when link  ; Make sure link is not nil
            (unless (member link visited-files)
              (push link visited-files)
              (push link files-to-process)
              ;; Store the file and its title as header
              (let ((title (org-include-get-file-title link)))
                (push (cons link title) headers-for-files)))))))
    
    ;; Add includes for all files (except the start file itself)
    (setq headers-for-files (reverse headers-for-files))
    (dolist (file-header headers-for-files)
      (let ((file (car file-header))
            (header (cdr file-header)))
        (unless (or (string= (expand-file-name file) 
                            (expand-file-name start-file))
                    (org-include-file-already-included-p file))
          (org-include-add-include file header))))))

;;;###autoload
(defun org-include-generate-from-current ()
  "Generate #+INCLUDE statements from links in the current org file."
  (interactive)
  (when (and buffer-file-name (string-match "\\.org$" buffer-file-name))
    (org-include-traverse-links-and-include buffer-file-name)
    (message "Generated includes from linked files.")))

(provide 'org-include-generator)
