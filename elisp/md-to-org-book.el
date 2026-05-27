;;; md-to-org-book.el --- Compose a Zenn book directory of .md files into one .org file

;; Usage:
;;   emacs -q --script elisp/md-to-org-book.el BOOK_DIR OUT_ORG

;; Reads BOOK_DIR/config.yaml for chapter order and metadata. For each chapter
;; markdown, extracts the YAML title, sends the body through pandoc, then
;; post-processes the resulting org to match the style of ../org/kyopro-bonsai-hs.org.
;; Zenn-specific constructs (`:::message`, `:::details`, lone image / URL lines)
;; are preserved verbatim inside `#+BEGIN_EXPORT markdown` blocks so ox-zenn can
;; round-trip them back to Markdown.

(require 'subr-x)
(require 'cl-lib)

(defun mbo-slurp (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mbo-spit (file str)
  (with-temp-file file
    (insert str)))

(defun mbo-strip-frontmatter (md)
  "Strip YAML frontmatter from MD, return (TITLE . BODY)."
  (if (string-match "\\`---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n?" md)
      (let* ((front (match-string 1 md))
             (body (substring md (match-end 0)))
             (title (if (string-match
                         "^title:[ \t]*\"\\([^\"]*\\)\"" front)
                        (match-string 1 front)
                      (when (string-match "^title:[ \t]*\\(.+\\)$" front)
                        (string-trim (match-string 1 front))))))
        (cons title body))
    (cons nil md)))

(defvar mbo--preserved nil
  "Alist of (TOKEN . MARKDOWN-CHUNK) for Zenn-specific blocks preserved during pandoc.")

(defun mbo--token (n)
  (format "XPANDOCKEEPX%dXEND" n))

(defun mbo-preserve-zenn-blocks (md)
  "Replace Zenn-specific blocks in MD with sentinel tokens.
Populates `mbo--preserved' as a side effect."
  (setq mbo--preserved nil)
  (let ((idx 0)
        (out md))
    ;; (1) :::message ... :::, :::message alert ... :::, :::details ... :::
    (with-temp-buffer
      (insert out)
      (goto-char (point-min))
      (while (re-search-forward "^:::\\(message\\(?: alert\\)?\\|details[^\n]*\\)\n" nil t)
        (let ((start (match-beginning 0)))
          (if (re-search-forward "^:::[ \t]*$" nil t)
              (let* ((end (match-end 0))
                     (chunk (buffer-substring-no-properties start end))
                     (tok (mbo--token idx)))
                (push (cons tok chunk) mbo--preserved)
                (delete-region start end)
                (goto-char start)
                (insert "\n" tok "\n")
                (setq idx (1+ idx)))
            ;; Unterminated; bail
            (goto-char (point-max)))))
      (setq out (buffer-string)))
    ;; (2) Standalone image lines: ^![alt](path)$
    (with-temp-buffer
      (insert out)
      (goto-char (point-min))
      (while (re-search-forward "^![^\n]*([^\n)]*)[ \t]*$" nil t)
        (let* ((chunk (match-string 0))
               (tok (mbo--token idx)))
          (push (cons tok chunk) mbo--preserved)
          (replace-match (concat "\n" tok "\n") t t)
          (setq idx (1+ idx))))
      (setq out (buffer-string)))
    ;; (3) Standalone URL lines (Zenn embed): ^https?://...$
    (with-temp-buffer
      (insert out)
      (goto-char (point-min))
      (while (re-search-forward "^\\(https?://[^[:space:]]+\\)[ \t]*$" nil t)
        (let* ((chunk (match-string 1))
               (tok (mbo--token idx)))
          (push (cons tok chunk) mbo--preserved)
          (replace-match (concat "\n" tok "\n") t t)
          (setq idx (1+ idx))))
      (setq out (buffer-string)))
    out))

(defun mbo-restore-zenn-blocks (org)
  "Replace preserved tokens in ORG with #+BEGIN_EXPORT markdown blocks."
  (let ((s org))
    (dolist (kv mbo--preserved)
      (let* ((tok (car kv))
             (chunk (cdr kv))
             (rendered (concat "#+BEGIN_EXPORT markdown\n"
                               (string-trim-right chunk)
                               "\n#+END_EXPORT")))
        ;; Pandoc may have wrapped the bare token line in a paragraph and may
        ;; have added link syntax around URL tokens (e.g. [[tok][tok]] is
        ;; unlikely for plain words, but be safe). Match the token anywhere.
        (while (string-match (regexp-quote tok) s)
          (setq s (replace-match rendered t t s)))))
    s))

(defun mbo-run-pandoc (md)
  "Convert MD string to org via pandoc, return the org string."
  (let ((tmp-in (make-temp-file "mbo-in-" nil ".md"))
        (tmp-out (make-temp-file "mbo-out-" nil ".org")))
    (unwind-protect
        (progn
          (mbo-spit tmp-in md)
          (let ((rc (call-process
                     "pandoc" nil nil nil
                     "-f" "markdown+fenced_divs"
                     "-t" "org"
                     "--wrap=preserve"
                     "-o" tmp-out
                     tmp-in)))
            (unless (zerop rc)
              (error "pandoc failed (rc=%d)" rc)))
          (mbo-slurp tmp-out))
      (ignore-errors (delete-file tmp-in))
      (ignore-errors (delete-file tmp-out)))))

(defun mbo-strip-custom-id (org)
  "Remove :PROPERTIES: drawers added by pandoc (CUSTOM_ID) from ORG."
  (replace-regexp-in-string
   "\\(:PROPERTIES:\n\\(?:[ \t]*:CUSTOM_ID:[^\n]*\n\\)+:END:\n\\)"
   ""
   org))

(defun mbo-demote-headings (org)
  "Add one `*' to every org heading line in ORG."
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " org))

(defun mbo-upcase-blocks (org)
  "Match the bonsai style: uppercase BEGIN/END for SRC/QUOTE blocks; rewrite HTML blocks to EXPORT html."
  (let ((s org))
    (setq s (replace-regexp-in-string "^#\\+begin_src" "#+BEGIN_SRC" s))
    (setq s (replace-regexp-in-string "^#\\+end_src" "#+END_SRC" s))
    (setq s (replace-regexp-in-string "^#\\+begin_quote" "#+BEGIN_QUOTE" s))
    (setq s (replace-regexp-in-string "^#\\+end_quote" "#+END_QUOTE" s))
    (setq s (replace-regexp-in-string "^#\\+begin_example" "#+BEGIN_EXAMPLE" s))
    (setq s (replace-regexp-in-string "^#\\+end_example" "#+END_EXAMPLE" s))
    ;; Pandoc emits raw HTML (e.g. `<!-- comment -->`) as `#+begin_html`. Promote
    ;; to `#+BEGIN_EXPORT html` so ox-zenn re-emits raw HTML.
    (setq s (replace-regexp-in-string "^#\\+begin_html[ \t]*$" "#+BEGIN_EXPORT html" s))
    (setq s (replace-regexp-in-string "^#\\+end_html[ \t]*$" "#+END_EXPORT" s))
    s))

(defun mbo-namespace-footnotes (org basename)
  "Rename `[fn:N]' footnote labels to `[fn:BASENAME-N]' to avoid cross-chapter collisions."
  (replace-regexp-in-string
   "\\[fn:\\([0-9]+\\)\\]"
   (lambda (m)
     (let ((n (match-string 1 m)))
       (format "[fn:%s-%s]" basename n)))
   org))

(defun mbo-convert-md-body (md basename)
  "Convert MD body to chapter-body org, performing preservation + post-processing."
  (let* ((md1 (mbo-preserve-zenn-blocks md))
         (org0 (mbo-run-pandoc md1))
         (org1 (mbo-strip-custom-id org0))
         (org2 (mbo-demote-headings org1))
         (org3 (mbo-upcase-blocks org2))
         (org4 (mbo-restore-zenn-blocks org3))
         (org5 (mbo-namespace-footnotes org4 basename)))
    org5))

(defun mbo-chapter-org (basename md-file)
  "Return an org string for one chapter."
  (let* ((md (mbo-slurp md-file))
         (split (mbo-strip-frontmatter md))
         (title (or (car split) basename))
         (body (cdr split))
         (org-body (mbo-convert-md-body body basename)))
    (concat "* " title "\n"
            ":PROPERTIES:\n"
            ":EXPORT_FILE_NAME: " basename "\n"
            ":END:\n\n"
            (string-trim org-body)
            "\n")))

(defun mbo-parse-config (config-file)
  "Parse a minimal subset of Zenn's config.yaml into a plist."
  (let ((text (mbo-slurp config-file))
        title summary topics published price chapters)
    (when (string-match "^title:[ \t]*\"\\([^\"]*\\)\"" text)
      (setq title (match-string 1 text)))
    (when (string-match "^summary:[ \t]*\"\\([^\"]*\\)\"" text)
      (setq summary (match-string 1 text)))
    (when (string-match "^topics:[ \t]*\\(\\[.*\\]\\)" text)
      (setq topics (match-string 1 text)))
    (when (string-match "^published:[ \t]*\\(true\\|false\\)" text)
      (setq published (match-string 1 text)))
    (when (string-match "^price:[ \t]*\\([0-9]+\\)" text)
      (setq price (match-string 1 text)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward "^chapters:[ \t]*$" nil t)
        (forward-line 1)
        (while (and (not (eobp))
                    (looking-at "^[ \t]*\\(#[^\n]*\\|-[ \t]*\\([^# \n][^# \n]*\\)\\|\\s-*\\)$"))
          (let ((c (match-string 2)))
            (when (and c (not (string-empty-p c)))
              (push (string-trim c) chapters)))
          (forward-line 1))))
    (list :title title :summary summary :topics topics
          :published published :price price
          :chapters (nreverse chapters))))

(defun mbo-header (cfg book-dir)
  "Return the file header for the composed org."
  (let ((title (or (plist-get cfg :title) "Untitled")))
    (concat
     "#+TITLE: " title "\n"
     "#+BOOK_DIR: " book-dir "\n"
     "#+PROPERTY: header-args :results output\n"
     "\n"
     "* スクリプト\n"
     "\n"
     "#+NAME: zenn-headings\n"
     "#+BEGIN_SRC elisp :results output\n"
     "(org-map-entries\n"
     "  (lambda ()\n"
     "    (let* ((title (org-entry-get nil \"EXPORT_FILE_NAME\"))\n"
     "           (is-draft (org-entry-get nil \"DRAFT\")))\n"
     "      (when (and title (not is-draft))\n"
     "        (princ (concat \"- \" title \"\\n\")))))\n"
     "  \"LEVEL=1\")\n"
     "#+END_SRC\n"
     "\n"
     "* =config.yaml=\n"
     ":PROPERTIES:\n"
     ":DRAFT:\n"
     ":END:\n"
     "\n"
     "#+BEGIN_SRC yaml :tangle " book-dir "/config.yaml :noweb yes\n"
     "title: " (format "\"%s\"" (or (plist-get cfg :title) "")) "\n"
     "summary: " (format "\"%s\"" (or (plist-get cfg :summary) "")) "\n"
     "topics: " (or (plist-get cfg :topics) "[]") "\n"
     "published: " (or (plist-get cfg :published) "true") "\n"
     "price: " (or (plist-get cfg :price) "0") "\n"
     "chapters:\n"
     "<<zenn-headings()>>\n"
     "#+END_SRC\n\n")))

(defun mbo-find-md (book-dir basename)
  "Return absolute path to BASENAME.md in BOOK-DIR or nil."
  (let ((p (expand-file-name (concat basename ".md") book-dir)))
    (when (file-exists-p p) p)))

(defun mbo-extra-md-files (book-dir chapter-list)
  "Return list of basenames present in BOOK-DIR but not in CHAPTER-LIST."
  (let* ((all (directory-files book-dir nil "\\.md\\'"))
         (basenames (mapcar (lambda (f) (file-name-sans-extension f)) all)))
    (cl-remove-if (lambda (b) (member b chapter-list)) basenames)))

(defun mbo-main ()
  (let* ((book-dir (expand-file-name (nth 0 argv)))
         (out (expand-file-name (nth 1 argv)))
         (cfg (mbo-parse-config (expand-file-name "config.yaml" book-dir)))
         (chapters (plist-get cfg :chapters))
         (book-dir-rel
          (file-relative-name book-dir (file-name-directory out)))
         (out-buf (concat (mbo-header cfg book-dir-rel))))
    (message "Chapters in config: %S" chapters)
    (dolist (basename chapters)
      (let ((md (mbo-find-md book-dir basename)))
        (if md
            (progn
              (message "Converting %s..." basename)
              (setq out-buf (concat out-buf (mbo-chapter-org basename md) "\n")))
          (message "WARN: missing chapter %s.md" basename))))
    (let ((extras (mbo-extra-md-files book-dir chapters)))
      (when extras
        (message "Drafting extra md files: %S" extras))
      (dolist (basename extras)
        (let* ((md (mbo-find-md book-dir basename))
               (chunk (mbo-chapter-org basename md))
               ;; Inject :DRAFT: property so the chapter is excluded from
               ;; config.yaml chapter list generation.
               (drafted
                (replace-regexp-in-string
                 ":PROPERTIES:\n:EXPORT_FILE_NAME:"
                 ":PROPERTIES:\n:DRAFT:\n:EXPORT_FILE_NAME:"
                 chunk)))
          (setq out-buf (concat out-buf drafted "\n")))))
    (mbo-spit out out-buf)
    (message "Wrote %s" out)))

(mbo-main)

;;; md-to-org-book.el ends here
