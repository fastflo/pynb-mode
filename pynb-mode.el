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

;; Execution
(defun pynb--start-executor ()
  "Start Python executor process if not running."
  (unless (process-live-p pynb--executor-process)
    (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
           ;(cmd (list "strace" "-o" "executor.strace" "-s512" "python3" (concat this-dir "executor.py"))))
           (cmd (list "python3" (concat this-dir "executor.py"))))
      (setq pynb--executor-buffer (get-buffer-create "*pynb-executor*"))
      ;(setq pynb--executor-process
      ;      (apply 'start-process "pynb-executor" pynb--executor-buffer cmd))
      (setq pynb--executor-process
	    (make-process
	     :name "pynb-executor"
	     :buffer pynb--executor-buffer
	     :command cmd
	     :connection-type 'pipe))
      (set-process-query-on-exit-flag pynb--executor-process nil)
      (message "Started pynb executor: %s" cmd))))

(defun pynb-execute-cell ()
  "Send current cell content to executor (debug version)."
  (interactive)
  (message "[pynb] C-c C-c pressed")
  (let ((cell (pynb--find-current-cell)))
    (if (not cell)
        (message "[pynb] No cell found!")
      ;; Start executor if needed
      (pynb--start-executor)
      ;; Extract content
      (let* ((start (nth 0 cell))
             (end (nth 1 cell))
             ;; Skip header/footer
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
        ;; DEBUG message
        (message "[pynb] Sending cmd %d (%d bytes)" cmd-id len)
        ;; Send
        (process-send-string pynb--executor-process command)
        (process-send-string pynb--executor-process content)))))

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
