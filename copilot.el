;;; copilot.el --- Emacs Copilot

;; Copyright 2023 Justine Alexandra Roberts Tunney

;; Author: Justine Tunney
;; Email: jtunney@mozilla.com
;; License: Apache 2.0
;; Version: 0.1

;; Copyright 2023 Mozilla Foundation
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; The `copilot-complete' function demonstrates that ~100 lines of LISP
;; is all it takes for Emacs to do that thing Github Copilot and VSCode
;; are famous for doing except superior w.r.t. both quality and freedom
;;
;; Emacs Copilot helps you do pair programming with a local-running LLM
;; that generates code completions within Emacs buffers. The LLM is run
;; as a sub-command that remembers your local editing history on a file
;; by file basis. Tokens stream into your buffer without delay as gen'd
;; and you can hit `C-g' to interrupt your LLM at any time. History and
;; memory can also be deleted from the LLM's context when deleting code
;; from your Emacs buffer that matches up verbatim. Copilot is language
;; agnostic and your programming language is determed by file extension
;;
;; The recommended LLM right now is WizardCoder 34b since it scores the
;; same as GPT-4 on HumanEval. You need a computer like a Mac Studio M2
;; Ultra in order to use it. If you have a modest system then you could
;; consider downloading the WizardCoder-Python-13b llamafile since it's
;; almost as good, and will even go acceptably fast on CPU-only systems
;; having at least AVX2 and 2200 MT/s RAM. If you're even more strapped
;; for compute and use things like Raspberry Pi, then give Phi-2 a spin
;;
;; To get started, try writing the first line of a function, e.g.
;;
;;     bool is_prime(int x) {
;;
;; Then place your caret at the end of the line, and press `C-c C-k` to
;; hand over control to your LLM, which should generate the rest of the
;; function implementation for you. Things are also tuned so the LLM is
;; likely to stop as soon as a function is made. Explanations and other
;; kind of ELI5 commentary is avoided too.
;;
;; Later on, if you were to write something like this:
;;
;;     int main() {
;;       for (int i = 0; i < 100;
;;
;; And ask your LLM to complete that, then your LLM will likely recall
;; that you two wrote an is_prime() function earlier, even though it's
;; only considering those two lines in the current instruction. You'll
;; most likely then see it decide to generate code to print the primes

;;; Code:

(defgroup copilot nil
  "Large language model code completion."
  :prefix "copilot-"
  :group 'editing)

(defcustom copilot-bin
  "wizardcoder-python-34b-v1.0.Q5_K_M.llamafile"
  "Path of llamafile executable with LLM weights."
  :type 'string
  :group 'copilot)

;;;###autoload
(defun copilot-complete ()
  (interactive)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (cash (concat curfile ".cache"))
         (hist (concat curfile ".prompt"))
         (lang (file-name-extension curfile))

         ;; extract current line, to left of caret
         ;; and the previous line, to give the llm
         (code (save-excursion
                 (dotimes (i 2)
                   (when (> (line-number-at-pos) 1)
                     (previous-line)))
                 (beginning-of-line)
                 (buffer-substring-no-properties (point) spot)))

         ;; create new prompt for this interaction
         (system "\
You are an Emacs code generator. \
Writing comments is forbidden. \
Writing test code is forbidden. \
Writing English explanations is forbidden. ")
         (prompt (format
                  "[INST]%sGenerate %s code to complete:[/INST]\n```%s\n%s"
                  (if (file-exists-p cash) "" system) lang lang code)))

    ;; iterate text deleted within editor then purge it from prompt
    (when kill-ring
      (save-current-buffer
        (find-file hist)
        (dotimes (i 10)
          (let ((substring (current-kill i t)))
            (when (and substring (string-match-p "\n.*\n" substring))
              (goto-char (point-min))
              (while (search-forward substring nil t)
                (delete-region (- (point) (length substring)) (point))))))
        (save-buffer 0)
        (kill-buffer (current-buffer))))

    ;; append prompt for current interaction to the big old prompt
    (write-region prompt nil hist 'append 'silent)

    ;; run llamafile streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
                    "--prompt-cache" cash
                    "--prompt-cache-all"
                    "--silent-prompt"
                    "--temp" "0"
                    "-c" "1024"
                    "-ngl" "35"
                    "-r" "```"
                    "-r" "\n}"
                    "-f" hist))

    ;; get rid of most markdown syntax
    (let ((end (point)))
      (save-excursion
        (goto-char spot)
        (while (search-forward "\\_" end t)
          (backward-char)
          (delete-backward-char 1 nil)
          (setq end (- end 1)))
        (goto-char spot)
        (while (search-forward "```" end t)
          (delete-backward-char 3 nil)
          (setq end (- end 3))))

      ;; append generated code to prompt
      (write-region spot end hist 'append 'silent))))

;; define `ctrl-c ctrl-k` keybinding for llm code completion
(defun copilot-c-hook ()
  (define-key c-mode-base-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'c-mode-common-hook 'copilot-c-hook)
(defun copilot-py-hook ()
  (define-key python-mode-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'python-common-hook 'copilot-py-hook)
(global-set-key (kbd "C-c C-k") 'copilot-complete)

(provide 'copilot)

;;; ansi-mode.el ends here
