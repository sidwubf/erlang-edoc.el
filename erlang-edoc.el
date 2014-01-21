;;; erlang-edoc.el --- EDoc support for Erlang mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.8.0
;; Keywords: languages, docs
;; Created: 2014-01-21

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ref: http://www.erlang.org/doc/apps/edoc/users_guide.html
;;
;; To use: (add-hook 'erlang-mode-hook 'erlang-edoc-mode)

;;; Code:

;;; XXX: should be in erlang.el.
(defgroup erlang nil
  "The Erlang programming language."
  :group 'languages)

(defcustom erlang-edoc-indent-level 2
  "Indentation level of xhtml in Erlang edoc."
  :safe 'integerp
  :group 'erlang)

(defvar erlang-edoc-generic-tags
  '("clear" "docfile" "end" "headerfile" "todo" "TODO" "type")
  "Tags that can be used anywhere within a module.")

(defvar erlang-edoc-overview-tags
  '("author" "copyright" "reference" "see" "since" "title" "version")
  "Tags that can be used in an overview file.")

(defvar erlang-edoc-module-tags
  '("author" "copyright" "deprecated" "doc" "hidden" "private" "reference"
    "see" "since" "version")
  "Tags that can be used before a module declaration.")

(defvar erlang-edoc-function-tags
  '("deprecated" "doc" "equiv" "hidden" "private" "see" "since" "spec"
    "throws" "type")
  "Tags that can be used before a function definition.")

(defvar erlang-edoc-predefined-macros
  '("date" "docRoot" "link" "module" "package" "section" "time"
    "type" "version"))

(defface erlang-edoc-tag '((t (:inherit font-lock-constant-face)))
  "Face used to highlight edoc tags."
  :group 'erlang)

(defface erlang-edoc-macro '((t (:inherit font-lock-preprocessor-face)))
  "Face used to highlight edoc macros."
  :group 'erlang)

(defface erlang-edoc-verbatim
  '((t (:family "Monospace" :inherit font-lock-keyword-face)))
  "Face used to highlight verbatim text."
  :group 'erlang)

(defface erlang-edoc-todo '((t (:inherit font-lock-warning-face)))
  "Face used to highlight edoc macros."
  :group 'erlang)

(defface erlang-edoc-heading '((t (:inherit bold)))
  "Face used to highlight edoc headings."
  :group 'erlang)

(defvar erlang-edoc-font-lock-keywords
  '(("^%+\\s-*\\(@\\w+\\)\\_>" 1 'erlang-edoc-tag prepend)
    ("^%+\\s-*" ("{\\(@\\w+\\)\\_>" nil nil (1 'erlang-edoc-macro prepend)))
    ("^%+\\s-*" ("\\(?:@@\\)*\\(@[@{}]\\)" nil nil (1 'escape-glyph prepend)))
    ("^%+\\s-*@\\(deprecated\\)\\_>" 1 font-lock-warning-face prepend)
    ;; http://www.erlang.org/doc/apps/edoc/chapter.html#Wiki_notation
    ("^%+\\s-*" ("[^`]`\\([^`]?\\|[^`].*?[^']\\)'"
                 (forward-char -1) nil (1 'erlang-edoc-verbatim prepend)))
    ("^%+\\s-*" ("\\[\\(\\(?:https?\\|file\\|ftp\\)://[^][]+\\)\\]"
                 nil nil (1 'link prepend)))
    ("^%+\\s-*\\(?:\\(?1:@todo\\|@TODO\\)\\_>\\|\\(?1:TODO\\):\\)"
     1 'erlang-edoc-todo prepend)
    ("^%+\\s-*\\(\\(=\\{2,4\\}\\)[^=\n].*[^=\n]\\2\\)\\s-*$"
     1 'erlang-edoc-heading prepend)))

(defun erlang-edoc-xml-context ()
  "Parse edoc x(ht)ml context at comment start of current line."
  (eval-and-compile (require 'xmltok))
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^%+\\s-*")
      (let ((pt (match-end 0)) context)
        (forward-comment (- (point)))
        (while (< (point) pt)
          (xmltok-forward)
          (cond ((eq xmltok-type 'start-tag)
                 (push (cons xmltok-type xmltok-start) context))
                ((eq xmltok-type 'end-tag)
                 (pop context))))
        (goto-char pt)
        (xmltok-forward)
        (push (car (memq xmltok-type '(start-tag end-tag))) context)
        context))))

(defun erlang-edoc-indent-line ()
  (let ((context (erlang-edoc-xml-context)))
    (when context
      (save-excursion
        (beginning-of-line)
        (re-search-forward "^%+\\s-*" (line-end-position))
        (when (or (car context) (cadr context))
          (let ((pad (when (cadr context)
                       (save-excursion
                         (goto-char (cdr (cadr context)))
                         (- (current-column)
                            (progn
                              (beginning-of-line)
                              (skip-chars-forward "%")
                              (current-column)))))))
            (just-one-space (cond ((not pad) 1)
                                  ((eq (car context) 'end-tag) pad)
                                  (t (+ erlang-edoc-indent-level pad)))))))
      (when (looking-back "^%*\\s-*")
        (re-search-forward "\\=%*\\s-*")))))

(defun erlang-edoc-before-module-declaration-p ()
  (save-excursion
    (beginning-of-line)
    (forward-comment (point-max))
    (or (eobp) (re-search-forward "^-module\\s-*(" nil t))))

(defun erlang-edoc-completion-at-point ()
  (when (eq (syntax-ppss-context (syntax-ppss)) 'comment)
    (save-excursion
      (skip-syntax-backward "w_")
      (when (= (preceding-char) ?@)
        (let* ((is-tag (looking-back "^%+\\s-*@"))
               (beg (point))
               (end (progn (skip-syntax-forward "w_") (point)))
               (table (cond
                       ((not is-tag)
                        erlang-edoc-predefined-macros)
                       ((erlang-edoc-before-module-declaration-p)
                        (append erlang-edoc-module-tags
                                erlang-edoc-generic-tags))
                       (t (append erlang-edoc-function-tags
                                  erlang-edoc-generic-tags)))))
          (list beg end table))))))

;;;###autoload
(define-minor-mode erlang-edoc-mode nil
  :lighter " EDoc"
  (if erlang-edoc-mode
      (progn
        (add-hook 'erlang-indent-line-hook #'erlang-edoc-indent-line nil t)
        (font-lock-add-keywords nil erlang-edoc-font-lock-keywords t)
        (add-hook 'completion-at-point-functions
                  #'erlang-edoc-completion-at-point nil t))
    (remove-hook 'erlang-indent-line-hook #'erlang-edoc-indent-line t)
    (font-lock-remove-keywords nil erlang-edoc-font-lock-keywords)
    (remove-hook 'completion-at-point-functions
                 #'erlang-edoc-completion-at-point t))
  (jit-lock-refontify))

(provide 'erlang-edoc)
;;; erlang-edoc.el ends here
