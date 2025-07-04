;;; package --- Org Include Links
;;; Commentary:
;;; Code:
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
Falls back to org-id if org-roam is not available.
Returns nil if ID cannot be resolved."
  (cond
   ;; If org-roam is available, use its functions
   ((fboundp 'org-roam-node-from-id)
    (message "Resolving ID link using org-roam: %s" id)  ; Debug message
    (let ((node (org-roam-node-from-id id)))
      (if node
          (let ((file (org-roam-node-file node)))
            (message "Resolved ID link using org-roam: %s (File: %s)" id file)  ; Debug message
            file)
        (message "Failed to resolve ID link using org-roam: %s" id)  ; Debug message
        nil)))

   ;; If org-id is available, try to use that
   ((fboundp 'org-id-find-id-file)
    (message "Resolving ID link using org-id: %s" id)  ; Debug message
    (let ((file (org-id-find-id-file id)))
      (if file
          (message "Resolved ID link using org-id: %s (File: %s)" id file)  ; Debug message
        (message "Failed to resolve ID link using org-id: %s" id))  ; Debug message
      file))

   ;; If neither org-roam nor org-id is available, return nil
   (t
    (message "Neither org-roam nor org-id is available to resolve ID link: %s" id)  ; Debug message
    nil)))





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
          (message "Found link: %s" link)  ; Debug message
          (when (string-match org-link-bracket-re link)
            (let ((path (match-string 1 link)))
              (message "Extracted path: %s" path)  ; Debug message
              (when (and path (string-match "^\\./.*\\.org" path))
                (let ((expanded-path (expand-file-name path (file-name-directory file))))
                  (message "Expanded path: %s" expanded-path)  ; Debug message
                  (push expanded-path links)))))))

      ;; Process ID links (need to go back to beginning)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
        (let* ((id (match-string-no-properties 1)))
          (message "Found ID link: %s" id)  ; Debug message
          (let ((resolved-file (org-include-resolve-id-link id)))
            (if resolved-file
                (message "Resolved ID link: %s (File: %s)" id resolved-file)  ; Debug message
              (message "Failed to resolve ID link: %s" id))  ; Debug message
            (when resolved-file
              (push resolved-file links)))))

      (message "All links found in %s: %S" file links)  ; Debug message
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
        (message "Processing file: %s" current-file)  ; Debug message
        ;; Get links in the current file
        (dolist (link (org-include-get-links-in-file current-file))
          (when link  ; Make sure link is not nil
            (unless (member link visited-files)
              (push link visited-files)
              (push link files-to-process)
              ;; Store the file and its title as header
              (let ((title (org-include-get-file-title link)))
                (message "Found link: %s (Title: %s)" link title)  ; Debug message
                (push (cons link title) headers-for-files)))))))

    ;; Add includes for all files (except the start file itself)
    (setq headers-for-files (reverse headers-for-files))
    (message "Headers for files: %S" headers-for-files)  ; Debug message
    (dolist (file-header headers-for-files)
      (let ((file (car file-header))
            (header (cdr file-header)))
        (unless (or (string= (expand-file-name file)
                            (expand-file-name start-file))
                    (org-include-file-already-included-p file))
          (message "Adding include for file: %s (Header: %s)" file header)  ; Debug message
          (org-include-add-include file header))))))



;;;###autoload
(defun org-include-generate-from-current ()
  "Generate #+INCLUDE statements from links in the current org file."
  (interactive)
  (when (and buffer-file-name (string-match "\\.org$" buffer-file-name))
    (org-include-traverse-links-and-include buffer-file-name)
    (message "Generated includes from linked files.")))

(provide 'org-include-generator)
;;; org-include-generator.el ends here
