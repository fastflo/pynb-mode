;;; pynb-mode.el --- Minor mode for .nb.py notebooks -*- lexical-binding: t; -*-

;; Author: Flo & ChatGPT
;; Version: 0.4
;; Keywords: python, notebook
;; Package-Requires: ((emacs "27.2"))

;;; Commentary:
;; pynb-mode provides notebook-like behavior for .nb.py files:
;; - Highlights current cell
;; - Hides technical cell headers visually
;; - Executes cells via a persistent Python process with C-c C-c
;; - Displays execution output inline (overlays, not saved)
;; - Cancels previous execution for a cell
;; - Adds thin separators and full-width gray output blocks

;;; Code:

(require 'python)
(require 'cl-lib)

;; ----------------------
;; Internal State
;; ----------------------
(defvar pynb--executor-process nil)
(defvar pynb--executor-buffer "*pynb-executor*")
(defvar pynb--executor-buffer-data "")

(defvar pynb--cmd-counter 0)
(defvar-local pynb--cmd-to-cell (make-hash-table :test 'equal)) ;; cmd-id -> cell-id
(defvar-local pynb--active-cells (make-hash-table :test 'equal)) ;; cell-id -> cmd-id
(defvar-local pynb--output-overlays (make-hash-table :test 'equal)) ;; cell-id -> overlay
(defvar-local pynb--cell-id-counter 0) ;; increment for each cell

(defvar-local pynb--cell-overlay nil)

;; ----------------------
;; Cell Identification
;; ----------------------
(defun pynb--next-cell-id ()
  (cl-incf pynb--cell-id-counter)
  (number-to-string pynb--cell-id-counter))

(defun pynb--scan-assign-cell-ids ()
  "Assign stable IDs (text property) to all cells in buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^## cell" nil t)
      (let ((pos (line-beginning-position)))
        (unless (get-text-property pos 'pynb-cell-id)
          (put-text-property pos (line-end-position)
                             'pynb-cell-id (pynb--next-cell-id)))))))

(defun pynb--get-current-cell-id ()
  "Return cell-id of current cell or nil."
  (save-excursion
    (when (re-search-backward "^## cell" nil t)
      (get-text-property (line-beginning-position) 'pynb-cell-id))))

(defun pynb--find-current-cell ()
  "Return (START END) positions for current cell block."
  (save-excursion
    (when (re-search-backward "^## cell" nil t)
      (let ((start (line-beginning-position)))
        (when (re-search-forward "^## cell [0-9]+ end ##" nil t)
          (list start (line-end-position)))))))

;; ----------------------
;; Highlight Current Cell
;; ----------------------
(defun pynb--highlight-current-cell ()
  (when pynb--cell-overlay (delete-overlay pynb--cell-overlay))
  (let ((cell (pynb--find-current-cell)))
    (when cell
      (setq pynb--cell-overlay (make-overlay (nth 0 cell) (nth 1 cell)))
      (overlay-put pynb--cell-overlay 'face '(:background "#f0f8ff" :extend t)))))

(defun pynb--post-command-update () (pynb--highlight-current-cell))

;; ----------------------
;; Hide Cell Headers
;; ----------------------
(defun pynb--hide-cell-headers ()
  (font-lock-add-keywords
   nil
   '(("^## cell [0-9]+ input ##$"
      (0 (progn
           (put-text-property (match-beginning 0) (match-end 0)
                              'display "## cell ##")
           nil)))
     ("^## cell [0-9]+ end ##$"
      (0 (progn
           (put-text-property (match-beginning 0) (match-end 0) 'display "")
           nil))))))

;; ----------------------
;; Executor Management
;; ----------------------
(defun pynb--start-executor ()
  (unless (process-live-p pynb--executor-process)
    (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
           (cmd (list "python3" (concat this-dir "executor.py"))))
      (setq pynb--executor-process
            (make-process
             :name "pynb-executor"
             :buffer pynb--executor-buffer
             :command cmd
             :connection-type 'pipe
             :filter 'pynb--executor-filter))
      (message "Started pynb executor: %s" cmd))))

;; ----------------------
;; Execute Current Cell
;; ----------------------
(defun pynb-execute-cell ()
  (interactive)
  (pynb--start-executor)
  (let ((cell (pynb--find-current-cell))
        (cell-id (pynb--get-current-cell-id)))
    (unless (and cell cell-id)
      (message "[pynb] No cell found!")
      (cl-return-from pynb-execute-cell))
    ;; Abort old execution if exists
    (when-let ((old-cmd (gethash cell-id pynb--active-cells)))
      (process-send-string pynb--executor-process (format "cmd %s: abort\n" old-cmd)))
    ;; Remove old overlay
    (when-let ((ov (gethash cell-id pynb--output-overlays)))
      (delete-overlay ov)
      (remhash cell-id pynb--output-overlays))
    ;; Prepare new execution
    (let* ((start (nth 0 cell)) (end (nth 1 cell))
           (content (save-excursion
                      (goto-char start) (forward-line 1)
                      (buffer-substring-no-properties
                       (point)
                       (progn (goto-char end) (forward-line -1) (line-end-position)))))
           (len (string-bytes content))
           (cmd-id (cl-incf pynb--cmd-counter)))
      (puthash cmd-id cell-id pynb--cmd-to-cell)
      (puthash cell-id cmd-id pynb--active-cells)
      ;; Send to executor
      (process-send-string pynb--executor-process (format "cmd %d: execute %d\n" cmd-id len))
      (process-send-string pynb--executor-process content)
      (message "[pynb] Sent cmd %d (%d bytes)" cmd-id len))))

;; ----------------------
;; Process Filter & Output Handling
;; ----------------------
(defun pynb--executor-filter (_proc string)
  (setq pynb--executor-buffer-data (concat pynb--executor-buffer-data string))
  (pynb--parse-executor-output))

(defun pynb--parse-executor-output ()
  (let ((loop t))
    (while loop
      (if (string-match "\\`cmd-output \\([0-9]+\\): \\([0-9]+\\)\n" pynb--executor-buffer-data)
          (let* ((cmd-id (string-to-number (match-string 1 pynb--executor-buffer-data)))
                 (size (string-to-number (match-string 2 pynb--executor-buffer-data)))
                 (header-end (match-end 0))
                 (available (- (length pynb--executor-buffer-data) header-end)))
            (if (>= available size)
                (let ((payload (substring pynb--executor-buffer-data header-end (+ header-end size))))
                  (setq pynb--executor-buffer-data
                        (substring pynb--executor-buffer-data (+ header-end size)))
                  (pynb--insert-output cmd-id payload))
              (setq loop nil)))
        (setq loop nil)))))

(defun pynb--insert-output (cmd-id text)
  (when-let ((cell-id (gethash cmd-id pynb--cmd-to-cell)))
    (let ((overlay (gethash cell-id pynb--output-overlays)))
      (if overlay
          ;; Append text
          (let ((end (overlay-end overlay)))
            (save-excursion
              (goto-char end)
              (unless (bolp) (insert "\n"))
              (insert text)
              (unless (string-suffix-p "\n" text) (insert "\n"))
              (move-overlay overlay (overlay-start overlay) (point))))
        ;; Create new overlay below cell
        (save-excursion
          (goto-char (nth 1 (pynb--find-current-cell)))
          (insert "\n")
          (let ((start (point)))
            (insert text)
            (unless (string-suffix-p "\n" text) (insert "\n"))
            (let ((end (point))
                  (ov (make-overlay start end)))
              (overlay-put ov 'face '(:background "#e0e0e0" :extend t))
              (overlay-put ov 'before-string
                           (propertize "\n" 'face '(:background "black" :height 0.1)))
              (overlay-put ov 'after-string
                           (propertize "\n" 'face '(:background "black" :height 0.1)))
              (puthash cell-id ov pynb--output-overlays))))))))

;; ----------------------
;; Minor Mode Setup
;; ----------------------
(defvar pynb-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "C-c C-c") 'pynb-execute-cell)
                        map))

(define-minor-mode pynb-mode
  "Minor mode for Python notebook files."
  :lighter " pynb" :keymap pynb-mode-map
  (if pynb-mode
      (progn
        (pynb--scan-assign-cell-ids)
        (pynb--hide-cell-headers)
        (add-hook 'post-command-hook #'pynb--post-command-update nil t))
    (remove-hook 'post-command-hook #'pynb--post-command-update t)))

(add-to-list 'auto-mode-alist '("\\.nb\\.py\\'" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match "\\.nb\\.py\\'" buffer-file-name))
              (pynb-mode 1))))

(provide 'pynb-mode)
;;; pynb-mode.el ends here
