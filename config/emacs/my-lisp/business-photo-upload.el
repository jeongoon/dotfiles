;;; business-photo-upload.el --- Business Photo Upload Management

;;; Commentary:
;; Business photo upload management for Google Business and Apple Business Connect

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-archive)
(require 'cl-lib)

;; ========== Variables ==========
;; Define variables FIRST before any functions use them

(defvar business-photo-auto-convert nil
  "If non-nil, ask to convert HEIC files when capturing.")

(defvar business-photo-show-images nil
  "If non-nil, automatically display images inline.")

(defvar business-photo-image-width 400
  "Default width for inline images.")

;; ========== User Configuration ==========

(defgroup business-photo-upload nil
  "Business photo upload management settings."
  :group 'org)

(defcustom business-photo-capture-key "B"
  "Key to use for business photo capture templates."
  :type 'string
  :group 'business-photo-upload)

(defcustom business-photo-org-directory "~/org/"
  "Base directory for org files."
  :type 'directory
  :group 'business-photo-upload)

(defcustom business-photo-log-filename "business.org"
  "Filename for the business photo log file."
  :type 'string
  :group 'business-photo-upload)

(defcustom business-photo-search-directories 
  '("~/Pictures/iCloud/" "~/Pictures/" "~/Downloads/" "./")
  "Directories to search for image files."
  :type '(repeat directory)
  :group 'business-photo-upload)

(defcustom business-photo-apple-max-size 4000
  "Maximum pixel size for Apple photos when converting."
  :type 'integer
  :group 'business-photo-upload)

(defcustom business-photo-apple-quality 90
  "JPEG quality for Apple photos."
  :type 'integer
  :group 'business-photo-upload)

(defcustom business-photo-google-quality 85
  "JPEG quality for Google Business photos."
  :type 'integer
  :group 'business-photo-upload)

(defcustom business-photo-create-thumbnail t
  "If non-nil, create small thumbnail for preview."
  :type 'boolean
  :group 'business-photo-upload)

(defcustom business-photo-thumbnail-size 800
  "Maximum size for thumbnail preview images in pixels."
  :type 'integer
  :group 'business-photo-upload)

(defcustom business-photo-archive-file "~/archives/business.org"
  "Archive file for completed business photo uploads."
  :type 'file
  :group 'business-photo-upload)

(defcustom business-photo-google-url nil
  "Google Business photo management URL."
  :type '(choice (const nil) string)
  :group 'business-photo-upload)

(defcustom business-photo-apple-url nil
  "Apple Business Connect photo management URL."
  :type '(choice (const nil) string)
  :group 'business-photo-upload)

;; ========== Helper Functions ==========

(defun business-photo-get-log-file ()
  "Get the full path to the log file."
  (expand-file-name business-photo-log-filename business-photo-org-directory))

(defun business-photo-find-file-by-number (number)
  "Find IMG_NUMBER.HEIC in search directories and subdirectories."
  (let ((patterns (list (format "IMG_%s.HEIC" number)
                       (format "IMG_%s.heic" number)
                       (format "img_%s.HEIC" number)
                       (format "img_%s.heic" number)))
        (found-file nil))
    ;; Try each search directory until we find the file
    (catch 'found
      (dolist (dir business-photo-search-directories)
        (when (and (not found-file) (file-directory-p (expand-file-name dir)))
          (dolist (pattern patterns)
            (when (not found-file)
              (let* ((cmd (format "find %s -name '%s' 2>/dev/null | head -1"
                                 (shell-quote-argument (expand-file-name dir))
                                 pattern))
                     (result (shell-command-to-string cmd))
                     (cleaned (string-trim result)))
                (when (and (not (string-empty-p cleaned))
                          (file-exists-p cleaned))
                  (setq found-file cleaned)
                  (message "Found file: %s" cleaned)
                  (throw 'found cleaned))))))))
    found-file))

(defun business-photo-find-file-recursive (filename base-dir)
  "Recursively find FILENAME starting from BASE-DIR."
  (let ((result (shell-command-to-string
                (format "find %s -name '%s' 2>/dev/null | head -1"
                       (shell-quote-argument (expand-file-name base-dir))
                       filename))))
    (let ((cleaned (string-trim result)))
      (when (and (not (string-empty-p cleaned))
                (file-exists-p cleaned))
        cleaned))))

(defun business-photo-debug-find (number)
  "Debug function to test file finding."
  (interactive "sImage number: ")
  (message "=====================================")
  (message "Searching for IMG_%s.HEIC (recursive)" number)
  (message "Search directories: %s" business-photo-search-directories)
  (message "-------------------------------------")
  
  (dolist (dir business-photo-search-directories)
    (let* ((expanded-dir (expand-file-name dir)))
      (message "\nDirectory: %s" expanded-dir)
      (if (file-directory-p expanded-dir)
          (let* ((patterns (list (format "IMG_%s.HEIC" number)
                               (format "IMG_%s.heic" number)))
                (find-results 
                 (mapcar (lambda (pattern)
                          (shell-command-to-string
                           (format "find %s -name '%s' 2>/dev/null"
                                  (shell-quote-argument expanded-dir)
                                  pattern)))
                        patterns)))
            (dolist (result find-results)
              (unless (string-empty-p (string-trim result))
                (message "  Found: %s" (string-trim result)))))
        (message "  [Directory doesn't exist]"))))
  
  (let ((result (business-photo-find-file-by-number number)))
    (if result
        (message "\n✓ FOUND: %s" result)
      (message "\n✗ File not found"))))

;; ========== Archive Functions ==========

(defun business-photo-setup-archive ()
  "Setup archive file and structure."
  (let ((archive-file (expand-file-name business-photo-archive-file)))
    ;; Create archive directory if needed
    (make-directory (file-name-directory archive-file) t)
    ;; Create archive file if it doesn't exist
    (unless (file-exists-p archive-file)
      (with-temp-buffer
        (insert "#+TITLE: Business Photo Archive\n")
        (insert "#+STARTUP: overview\n")
        (insert "#+FILETAGS: :archive:\n\n")
        (insert "# Archived business photo uploads organized by date\n\n")
        (write-file archive-file)))
    archive-file))

(defun my/business-archive-done ()
  "Archive all DONE entries to the archive file using datetree.
Uses org-mode's built-in datetree archive functionality."
  (interactive)
  (when (not (derived-mode-p 'org-mode))
    (error "This command only works in org-mode"))
  
  ;; Ensure archive file exists
  (business-photo-setup-archive)
  
  ;; Set archive location to use datetree
  ;; This will create date-based structure automatically
  (setq-local org-archive-location 
              (format "%s::datetree/" business-photo-archive-file))
  
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      ;; Find all DONE entries
      (while (re-search-forward "^\\*+ DONE " nil t)
        (beginning-of-line)
        (condition-case err
            (progn
              ;; Use org-archive-subtree which handles datetree properly
              (org-archive-subtree)
              (setq count (1+ count)))
          (error (message "Error archiving entry: %s" err)))))
    
    (cond
     ((> count 0)
      (save-buffer)
      ;; Also save the archive file if it's open
      (when-let ((archive-buffer (find-buffer-visiting business-photo-archive-file)))
        (with-current-buffer archive-buffer
          (save-buffer)))
      (message "✓ Archived %d entries to %s (datetree)" count business-photo-archive-file))
     (t
      (message "No DONE entries found to archive")))))

(defun my/business-archive-single ()
  "Archive the current entry if it's DONE."
  (interactive)
  (when (not (derived-mode-p 'org-mode))
    (error "This command only works in org-mode"))
  
  ;; Check if current entry is DONE
  (save-excursion
    (org-back-to-heading t)
    (if (looking-at "^\\*+ DONE ")
        (progn
          (business-photo-setup-archive)
          (setq-local org-archive-location 
                      (format "%s::datetree/" business-photo-archive-file))
          (org-archive-subtree)
          (save-buffer)
          (message "✓ Archived to %s (datetree)" business-photo-archive-file))
      (message "Current entry is not DONE"))))

;; Alternative: Use org-archive-to-archive-sibling for simpler setup
(defun my/business-archive-done-sibling ()
  "Archive DONE items as siblings in the same file under an Archive heading."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ DONE " nil t)
        (org-archive-to-archive-sibling)
        (setq count (1+ count))))
    (if (> count 0)
        (message "✓ Archived %d entries" count)
      (message "No DONE entries found"))))

;; ========== Capture Template ==========

(defun business-photo-upload-simple ()
  "Simplified capture template for photo upload."
  (let* ((input (read-string "Image number or path (e.g., 1234 or full path): "))
         (original nil))
    
    ;; Find the file
    (setq original
          (cond
           ;; Just numbers - search for IMG_XXXX.HEIC
           ((string-match "^[0-9]+$" input)
            (or (business-photo-find-file-by-number input)
                (progn
                  (message "IMG_%s.HEIC not found in: %s" 
                           input business-photo-search-directories)
                  (read-file-name 
                   (format "IMG_%s.HEIC not found. Select file: " input)
                   (car business-photo-search-directories) nil t))))
           
           ;; Already has path separator or extension
           ((or (string-match "/" input) 
                (string-match "\\.HEIC$" input)
                (string-match "\\.heic$" input))
            (let ((expanded (expand-file-name input)))
              (if (file-exists-p expanded)
                  expanded
                (read-file-name 
                 (format "File not found: %s. Select file: " input)
                 (car business-photo-search-directories) nil t))))
           
           ;; IMG_XXXX format
           ((string-match "^IMG_[0-9]+$" input)
            (or (business-photo-find-file-by-number (replace-regexp-in-string "IMG_" "" input))
                (read-file-name 
                 (format "%s.HEIC not found. Select file: " input)
                 (car business-photo-search-directories) nil t)))
           
           ;; Default - just select file
           (t (read-file-name "Select HEIC file: " 
                              (car business-photo-search-directories) nil t))))
    
    ;; Get other information
    (let* ((title (read-string "Photo title: "))
           (description (read-string "Photo description: "))
           (alt-text (read-string "Alt text (for accessibility): " description))
           (auto-convert-google (y-or-n-p "Convert to JPG for Google? "))
           (auto-convert-apple (y-or-n-p (format "Resize HEIC for Apple (max %dpx)? " 
                                                 business-photo-apple-max-size)))
           (google-file nil)
           (apple-file nil)
           (preview-file nil))
      
      ;; Show selected file
      (message "Selected: %s" original)
      
      ;; Convert for Google if requested
      (when auto-convert-google
        (let ((dir (file-name-directory original))
              (base (file-name-base original)))
          ;; Create Google JPG
          (setq google-file (expand-file-name (concat base ".jpg") dir))
          (shell-command
           (format "magick '%s' -quality %d '%s'"
                   original
                   business-photo-google-quality
                   google-file))
          (message "Created Google JPG: %s" google-file)
          
          ;; Create thumbnail if enabled
          (when business-photo-create-thumbnail
            (setq preview-file (expand-file-name (concat base "_thumb.jpg") dir))
            (shell-command
             (format "magick '%s' -resize '%dx%d>' -quality 70 '%s'"
                     original
                     business-photo-thumbnail-size
                     business-photo-thumbnail-size
                     preview-file))
            (message "Created thumbnail: %s" preview-file))))
      
      ;; Convert/resize for Apple if requested
      (when auto-convert-apple
        (let ((dir (file-name-directory original))
              (base (file-name-base original)))
          ;; Create Apple HEIC (resized)
          (setq apple-file (expand-file-name (concat base "_apple.heic") dir))
          ;; ImageMagick can resize HEIC while keeping format
          (shell-command
           (format "magick '%s' -resize '%dx%d>' -quality %d '%s'"
                   original
                   business-photo-apple-max-size
                   business-photo-apple-max-size
                   business-photo-apple-quality
                   apple-file))
          (message "Created Apple HEIC (resized): %s" apple-file)))
      
      ;; Set default file paths if not converted
      (unless google-file
        (let ((dir (file-name-directory original))
              (base (file-name-base original)))
          (setq google-file (expand-file-name (concat base ".jpg") dir))))
      
      (unless apple-file
        (setq apple-file original))  ; Use original if not resized
      
      ;; Return the formatted template
      (format "* TODO %s
:PROPERTIES:
:ORIGINAL: %s
:GOOGLE_FILE: %s
:APPLE_FILE: %s
:THUMBNAIL: %s
:CAPTURE_DATE: %s
:GOOGLE_CONVERTED: %s
:APPLE_CONVERTED: %s
:END:

** Preview
%s

** Photo Info
- Description: %s
- Alt text: %s

** Upload Status
- [ ] Google Business
- [ ] Apple Business Connect

** Notes

"
              title
              original
              google-file
              apple-file
              (or preview-file "")
              (format-time-string "[%Y-%m-%d %a %H:%M]")
              (if auto-convert-google "yes" "no")
              (if auto-convert-apple "yes" "no")
              ;; Preview section

              (cond
               ((and preview-file (file-exists-p preview-file))
                (format "#+ATTR_ORG: :width %d\n[[file:%s]]" 
                        business-photo-image-width preview-file))
               ((and google-file (file-exists-p google-file))
                (format "#+ATTR_ORG: :width %d\n[[file:%s]]" 
                        business-photo-image-width google-file))
               (t "# No preview - convert to JPG first"))
              description
              alt-text))))


;; ========== Other Functions ==========

(defun my/business-open-google-photos ()
  "Open Google Business photo management page."
  (interactive)
  (let ((url (or 
              ;; 1. Check file-local variable
              (when (boundp 'business-photo-google-url-local)
                business-photo-google-url-local)
              ;; 2. Check for #+GOOGLE_URL: in buffer
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^#\\+GOOGLE_URL: \\(.+\\)" nil t)
                  (match-string 1)))
              ;; 3. Use global setting
              business-photo-google-url
              ;; 4. Default message
              nil)))
    (if url
        (progn
          (browse-url url)
          (message "Opening Google Business Photos..."))
      (message "Google URL not set. Add #+GOOGLE_URL: or set business-photo-google-url"))))

(defun my/business-open-apple-photos ()
  "Open Apple Business Connect photo management page."
  (interactive)
  (let ((url (or 
              ;; 1. Check file-local variable
              (when (boundp 'business-photo-apple-url-local)
                business-photo-apple-url-local)
              ;; 2. Check for #+APPLE_URL: in buffer
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^#\\+APPLE_URL: \\(.+\\)" nil t)
                  (match-string 1)))
              ;; 3. Use global setting
              business-photo-apple-url
              ;; 4. Default message
              nil)))
    (if url
        (progn
          (browse-url url)
          (message "Opening Apple Business Connect..."))
      (message "Apple URL not set. Add #+APPLE_URL: or set business-photo-apple-url"))))

(defun my/business-find-heic ()
  "Quick search and insert HEIC file path."
  (interactive)
  (let* ((input (read-string "Image number (e.g., 1234): "))
         (number (if (string-match "^IMG_" input)
                    (replace-regexp-in-string "^IMG_" "" input)
                  input))
         (result (business-photo-find-file-by-number number)))
    (if result
        (progn
          (insert result)
          (message "Inserted: %s" result))
      (message "Not found: IMG_%s.HEIC" number))))

(defun my/business-mark-uploaded ()
  "Toggle checkbox and add timestamp."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\[ \\]" (line-end-position) t)
        (progn
          (replace-match "[X]")
          (end-of-line)
          (insert (format-time-string " [%Y-%m-%d %a %H:%M]"))
          (message "✓ Marked as complete"))
      (if (re-search-backward "\\[X\\]" (line-beginning-position) t)
          (progn
            (replace-match "[ ]")
            (save-excursion
              (end-of-line)
              (when (re-search-backward " \\[20[0-9][0-9]-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\]" 
                                       (line-beginning-position) t)
                (delete-region (match-beginning 0) (match-end 0))))
            (message "☐ Unmarked"))
        (message "No checkbox on this line")))))

(defun my/business-convert-directory ()
  "Convert all HEIC files in directory."
  (interactive)
  (let ((dir (read-directory-name "Directory to convert: ")))
    (shell-command 
     (format
      "cd %s && \
echo 'Converting HEIC to JPG...' && \
for f in *.HEIC; do \
  if [ -f \"$f\" ]; then \
    base=$(basename \"$f\" .HEIC); \
    magick \"$f\" -quality %d \"${base}.jpg\"; \
    echo \"Converted: $f\"; \
  fi; \
done && \
echo 'Done!'"
      (shell-quote-argument dir)
      business-photo-google-quality))
    (message "Conversion complete in %s" dir)))

(defun my/business-toggle-images ()
  "Toggle inline images display."
  (interactive)
  (if org-inline-image-overlays
      (progn
        (org-remove-inline-images)
        (message "Images hidden"))
    (progn
      (org-display-inline-images nil t)
      (message "Images displayed"))))

(defun my/business-force-display-images ()
  "Force display all images in current buffer."
  (interactive)
  (clear-image-cache)
  (org-remove-inline-images)
  (setq-local org-image-actual-width (or business-photo-image-width 400))
  (org-display-inline-images nil t (point-min) (point-max))
  (message "Forced image display with width %s" org-image-actual-width))

;; ========== Setup Functions ==========

(defun business-photo-setup-org-mode ()
  "Setup org mode for business photo files."
  (when (and (buffer-file-name)
             (string-match-p (regexp-quote business-photo-log-filename) 
                           (buffer-file-name)))
    ;; Set image display
    (setq-local org-image-actual-width (or business-photo-image-width 400))
    (when business-photo-show-images
      (org-display-inline-images nil t))
    ;; Set archive location to datetree
    (setq-local org-archive-location 
                (format "%s::datetree/" business-photo-archive-file))))

;; ========== Capture Templates Setup ==========

(defun business-photo-add-capture-templates ()
  "Add business photo capture templates."
  (let ((log-file (business-photo-get-log-file)))
    
    (make-directory business-photo-org-directory t)
    
    ;; Remove old templates
    (setq org-capture-templates
          (cl-remove-if (lambda (template)
                          (and (stringp (car template))
                               (string-prefix-p business-photo-capture-key (car template))))
                        org-capture-templates))
    
    ;; Add new templates
    (add-to-list 'org-capture-templates
                 `(,business-photo-capture-key "Business"))
    
    (add-to-list 'org-capture-templates
                 `(,(concat business-photo-capture-key "p") "Photo Upload" entry
                   (file+olp+datetree ,log-file)
                   (function business-photo-upload-simple)) t)
    
    (add-to-list 'org-capture-templates
                 `(,(concat business-photo-capture-key "c") "Convert Media" entry
                   (file+headline ,log-file "Media Conversions")
                   "* Convert: %^{Description}
Date: %U
Directory: %^{Directory Path}

** Image Conversion
#+BEGIN_SRC bash :dir %\\2 :results output
cd \"%\\2\"
for file in *.HEIC; do
    [ -f \"$file\" ] || continue
    base=$(basename \"$file\" .HEIC)
    magick \"$file\" -quality 100 \"${base}.jpg\"
    echo \"Converted: $file\"
done
#+END_SRC

** Video Conversion
#+BEGIN_SRC bash :dir %\\2 :results output
cd \"%\\2\"
mkdir -p davinci
for file in *.MOV; do
    [ -f \"$file\" ] || continue
    base=$(basename \"$file\" .MOV)
    ffmpeg -i \"$file\" -c:v dnxhd -profile:v dnxhr_hq -pix_fmt yuv422p \\
           -vf \"scale=in_range=full:out_range=full\" -color_range 2 \\
           -c:a alac -hide_banner -loglevel error -y \"davinci/${base}_dnxhr.mov\"
    echo \"Converted: $file\"
done
#+END_SRC
") t)))

;; ========== Initialize ==========

(with-eval-after-load 'org-capture
  (business-photo-add-capture-templates))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'business-photo-setup-org-mode)
  (define-key org-mode-map (kbd "C-c b m") 'my/business-mark-uploaded)
  (define-key org-mode-map (kbd "C-c b d") 'my/business-convert-directory)
  (define-key org-mode-map (kbd "C-c b i") 'my/business-toggle-images)
  (define-key org-mode-map (kbd "C-c b I") 'my/business-force-display-images)
  (define-key org-mode-map (kbd "C-c b f") 'my/business-find-heic)
  (define-key org-mode-map (kbd "C-c b t") 'business-photo-debug-find)
  (define-key org-mode-map (kbd "C-c b a") 'my/business-archive-done)
  (define-key org-mode-map (kbd "C-c b A") 'my/business-archive-single)
  (define-key org-mode-map (kbd "C-c b s") 'my/business-archive-done-sibling)
  (define-key org-mode-map (kbd "C-c b g") 'my/business-open-google-photos)
  (define-key org-mode-map (kbd "C-c b G") 'my/business-open-apple-photos))

(provide 'business-photo-upload)

;;; business-photo-upload.el ends here
