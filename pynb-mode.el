;;; pynb-mode.el --- Minor mode for .nb.py notebooks -*- lexical-binding: t; -*-

;; Author: Flo & ChatGPT
;; Version: 0.1
;; Keywords: python, notebook
;; Package-Requires: ((emacs "27.2"))

;;; Commentary:
;; pynb-mode provides notebook-like behavior for .nb.py files:
;; - Based on python-mode
;; - Highlights current cell
;; - Hides cell headers visually
;; - Sends current cell to a persistent Python executor on C-c C-c

;;; Code:

(require 'python)
(defvar pynb--executor-process nil "Python executor process.")
(defvar pynb--executor-buffer "*pynb-executor*" "Buffer for Python executor output.")
(defvar pynb--cmd-counter 0 "Command counter for executions.")

;; Overlays for highlighting
(defvar-local pynb--cell-overlay nil)
(defvar-local pynb--cell-boundaries nil)

(defvar-local pynb--output-overlays (make-hash-table :test 'equal)
  "Map from command IDs to output overlays.")

(defvar pynb--executor-buffer-data ""
  "Accumulator for partial process output.")


(defun pynb--find-current-cell ()
  "Return (START END HEADER END-LINE) positions for current cell."
  (save-excursion
    (let ((header-re "^## cell \\([0-9]+\\) input ##$")
          (end-re "^## cell [0-9]+ end ##")
          start end)
      (when (re-search-backward header-re nil t)
        (setq start (line-beginning-position))
        (let ((cell-num (match-string 1)))
          (when (re-search-forward end-re nil t)
            (setq end (line-end-position))
            (list start end cell-num)))))))

(defun pynb--highlight-current-cell ()
  "Highlight current cell."
  (when pynb--cell-overlay
    (delete-overlay pynb--cell-overlay))
  (let ((cell (pynb--find-current-cell)))
    (when cell
      (let ((start (nth 0 cell))
            (end (nth 1 cell)))
        (setq pynb--cell-overlay (make-overlay start end))
        ;; Full width highlight like org-mode
        (overlay-put pynb--cell-overlay 'face '(:background "#f0f8ff" :extend t))
        (overlay-put pynb--cell-overlay 'evaporate t)))))

(defun pynb--post-command-update ()
  (pynb--highlight-current-cell))

(defun pynb--hide-cell-headers ()
  "Use font-lock and text properties to hide parts of cell headers."
  (font-lock-add-keywords
   nil
   '(("^## cell [0-9]+ input ##$"
      (0 (progn
           (put-text-property (match-beginning 0) (match-end 0) 'display "## cell ##")
           nil)))
     ("^## cell [0-9]+ end ##$"
      (0 (progn
           (put-text-property (match-beginning 0) (match-end 0) 'display "")
           nil))))))

(defun pynb--executor-filter (proc string)
  "Handle output from the Python executor."
  (setq pynb--executor-buffer-data
        (concat pynb--executor-buffer-data string))
  (pynb--parse-executor-output))


(defun pynb--parse-executor-output ()
  "Parse accumulated executor output and update overlays."
  (let ((loop-continue t))
    (while loop-continue
      (if (string-match "\\`cmd-output \\([0-9]+\\): \\([0-9]+\\)\n" pynb--executor-buffer-data)
          (let* ((cmd-id (match-string 1 pynb--executor-buffer-data))
                 (size (string-to-number (match-string 2 pynb--executor-buffer-data)))
                 (header-end (match-end 0))
                 (available (- (length pynb--executor-buffer-data) header-end)))
            (if (>= available size)
                ;; Full payload available
                (let ((payload (substring pynb--executor-buffer-data header-end (+ header-end size))))
                  ;; Remove processed chunk
                  (setq pynb--executor-buffer-data
                        (substring pynb--executor-buffer-data (+ header-end size)))
                  ;; Insert output
                  (pynb--insert-output cmd-id payload))
              ;; Not enough data yet → wait for more
              (setq loop-continue nil)))
        ;; No header match → stop
        (setq loop-continue nil)))))

(defun pynb--insert-output (cmd-id text)
  "Insert TEXT as output for CMD-ID below the corresponding cell."
  (let ((overlay (gethash cmd-id pynb--output-overlays)))
    ;; Remove old overlay if exists
    (when overlay
      (delete-overlay overlay))
    ;; Find the cell position
    (let ((cell-pos (gethash cmd-id pynb--cell-pos)))
      (when cell-pos
        (save-excursion
          (goto-char cell-pos)
          ;; Insert a newline before overlay (for visual separation)
          (insert "\n")
          (let* ((start (point))
                 (end (progn
                        (insert text)
                        (insert "\n")
                        (point)))
                 (ov (make-overlay start end)))
            ;; Apply background with full-width extension
            (overlay-put ov 'face '(:background "#e0e0e0" :extend t))
            (overlay-put ov 'evaporate t)
            ;; Save overlay
            (puthash cmd-id ov pynb--output-overlays)))))))
;; Execution
(defun pynb--start-executor ()
  "Start Python executor process if not running."
  (unless (process-live-p pynb--executor-process)
    (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
           ;(cmd (list "strace" "-o" "executor.strace" "-s512" "python3" (concat this-dir "executor.py"))))
           (cmd (list "python3" (concat this-dir "executor.py"))))
      (setq pynb--executor-buffer (get-buffer-create "*pynb-executor*"))
      (setq pynb--executor-process
	    (make-process
	     :name "pynb-executor"
	     :buffer pynb--executor-buffer
	     :command cmd
	     :connection-type 'pipe
	     :filter 'pynb--executor-filter))
      (set-process-query-on-exit-flag pynb--executor-process nil)
      (message "Started pynb executor: %s" cmd))))

(defvar-local pynb--cell-pos (make-hash-table :test 'equal)
  "Map from cmd-id to cell end position.")

(defun pynb-execute-cell ()
  "Send current cell content to executor."
  (interactive)
  (let ((cell (pynb--find-current-cell)))
    (if (not cell)
        (message "[pynb] No cell found!")
      (pynb--start-executor)
      (let* ((start (nth 0 cell))
             (end (nth 1 cell))
             (content (save-excursion
                        (goto-char start)
                        (forward-line 1)
                        (buffer-substring-no-properties
                         (point)
                         (progn (goto-char end)
                                (forward-line -1)
                                (line-end-position)))))
             (len (string-bytes content))
             (cmd-id (cl-incf pynb--cmd-counter))
             (command (format "cmd %d: execute %d\n" cmd-id len)))
        ;; Store cell end for output insertion
        (puthash (number-to-string cmd-id) end pynb--cell-pos)
        ;; Remove old overlay for this cmd-id
        (when-let ((ov (gethash (number-to-string cmd-id) pynb--output-overlays)))
          (delete-overlay ov)
          (remhash (number-to-string cmd-id) pynb--output-overlays))
        ;; Send command
        (process-send-string pynb--executor-process command)
        (process-send-string pynb--executor-process content)
        (message "[pynb] Sent cmd %d (%d bytes)" cmd-id len)))))

;; Mode definition

;; Define keymap before mode
(defvar pynb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'pynb-execute-cell)
    map)
  "Keymap for pynb-mode.")

(define-minor-mode pynb-mode
  "Minor mode for .nb.py notebook files."
  :lighter " pynb"
  :keymap pynb-mode-map
  (if pynb-mode
      (progn
        ;; Remove (python-mode)
        (pynb--hide-cell-headers)
        (add-hook 'post-command-hook #'pynb--post-command-update nil t))
    (remove-hook 'post-command-hook #'pynb--post-command-update t)
    (when pynb--cell-overlay
      (delete-overlay pynb--cell-overlay))))

(add-to-list 'auto-mode-alist '("\\.nb\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match "\\.nb\\.py\\'" buffer-file-name))
              (pynb-mode 1))))

(provide 'pynb-mode)
;;; pynb-mode.el ends here
