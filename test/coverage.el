;;; coverage.el --- Batch coverage runner for cc-switch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defconst cc-switch-coverage--root-directory
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for the batch coverage run.")

(defvar cc-switch-coverage-min 0
  "Minimum total coverage percentage required by the batch coverage run.")

(defvar cc-switch-coverage-directory "coverage"
  "Directory where batch coverage reports are written.")

(defconst cc-switch-coverage--source-files
  '("cc-switch.el")
  "Source files instrumented by the batch coverage run.")

(defconst cc-switch-coverage--test-files
  '("test/cc-switch-test.el")
  "ERT files loaded by the batch coverage run.")

(defvar cc-switch-coverage--instrumented nil
  "Alist mapping source file names to instrumented definition symbols.")

(defun cc-switch-coverage--instrument-file (file)
  "Instrument FILE with `testcover' and remember its definition symbols."
  (let ((source (expand-file-name file cc-switch-coverage--root-directory))
        symbols)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (let ((default-directory cc-switch-coverage--root-directory))
        (testcover-start source)))
    (setq symbols (mapcar #'car edebug-form-data))
    (push (cons file symbols)
          cc-switch-coverage--instrumented)))

(defun cc-switch-coverage--entry-covered-p (entry)
  "Return non-nil if testcover ENTRY is considered covered."
  (or (eq entry 'edebug-ok-coverage)
      (memq (car-safe entry) '(testcover-1value maybe noreturn))))

(defun cc-switch-coverage--symbol-summary (symbol)
  "Return (TOTAL COVERED UNCOVERED) for SYMBOL's coverage vector."
  (let ((coverage (get symbol 'edebug-coverage))
        (total 0)
        (covered 0)
        (uncovered 0))
    (when (vectorp coverage)
      (dotimes (index (length coverage))
        (setq total (1+ total))
        (if (cc-switch-coverage--entry-covered-p
             (aref coverage index))
            (setq covered (1+ covered))
          (setq uncovered (1+ uncovered)))))
    (list total covered uncovered)))

(defun cc-switch-coverage--file-summary (file symbols)
  "Return a plist coverage summary for FILE and SYMBOLS."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (symbol symbols)
      (pcase-let ((`(,sym-total ,sym-covered ,sym-uncovered)
                   (cc-switch-coverage--symbol-summary symbol)))
        (when (> sym-total 0)
          (setq defs (1+ defs))
          (setq total (+ total sym-total))
          (setq covered (+ covered sym-covered))
          (setq uncovered (+ uncovered sym-uncovered)))))
    (list :file file
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun cc-switch-coverage--summaries ()
  "Return coverage summaries for all instrumented files."
  (mapcar (lambda (entry)
            (cc-switch-coverage--file-summary
             (car entry)
             (cdr entry)))
          (nreverse cc-switch-coverage--instrumented)))

(defun cc-switch-coverage--total-summary (summaries)
  "Return total coverage summary for SUMMARIES."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (summary summaries)
      (setq defs (+ defs (plist-get summary :defs)))
      (setq total (+ total (plist-get summary :total)))
      (setq covered (+ covered (plist-get summary :covered)))
      (setq uncovered (+ uncovered (plist-get summary :uncovered))))
    (list :file "TOTAL"
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun cc-switch-coverage--format-summary (summary)
  "Return a human-readable line for coverage SUMMARY."
  (format "%-24s defs=%3d forms=%5d covered=%5d missed=%5d %6.2f%%"
          (plist-get summary :file)
          (plist-get summary :defs)
          (plist-get summary :total)
          (plist-get summary :covered)
          (plist-get summary :uncovered)
          (plist-get summary :percent)))

(defun cc-switch-coverage--write-tsv (summaries total)
  "Write SUMMARIES and TOTAL to the batch coverage TSV report."
  (make-directory cc-switch-coverage-directory t)
  (let ((report (expand-file-name
                 "testcover-summary.tsv"
                 cc-switch-coverage-directory)))
    (with-temp-file report
      (insert "file\tdefs\tforms\tcovered\tmissed\tpercent\n")
      (dolist (summary (append summaries (list total)))
        (insert
         (format "%s\t%d\t%d\t%d\t%d\t%.2f\n"
                 (plist-get summary :file)
                 (plist-get summary :defs)
                 (plist-get summary :total)
                 (plist-get summary :covered)
                 (plist-get summary :uncovered)
                 (plist-get summary :percent)))))
    report))

(defun cc-switch-coverage-run ()
  "Run ERT tests under `testcover' and write a coverage summary."
  (setq cc-switch-coverage--instrumented nil)
  (dolist (file cc-switch-coverage--source-files)
    (cc-switch-coverage--instrument-file file))
  (dolist (file cc-switch-coverage--test-files)
    (load (expand-file-name file cc-switch-coverage--root-directory) nil t))
  (let* ((stats (ert-run-tests-batch t))
         (summaries (cc-switch-coverage--summaries))
         (total (cc-switch-coverage--total-summary summaries))
         (report (cc-switch-coverage--write-tsv summaries total)))
    (princ "\nCoverage summary:\n")
    (dolist (summary summaries)
      (princ (concat (cc-switch-coverage--format-summary summary)
                     "\n")))
    (princ (concat (cc-switch-coverage--format-summary total)
                   "\n"))
    (princ (format "Coverage report: %s\n" report))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (kill-emacs 1))
    (when (< (plist-get total :percent) cc-switch-coverage-min)
      (princ
       (format
        "Coverage %.2f%% is below required minimum %.2f%%\n"
        (plist-get total :percent)
        (float cc-switch-coverage-min)))
      (kill-emacs 1))))

(cc-switch-coverage-run)

;;; coverage.el ends here
