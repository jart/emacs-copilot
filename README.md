# Emacs Copilot

https://github.com/jart/emacs-copilot/assets/49262/1a79d4e4-9622-452e-9944-950c6f21d67f

The `copilot-complete` function demonstrates that ~100 lines of LISP
is all it takes for Emacs to do that thing Github Copilot and VSCode
are famous for doing except superior w.r.t. both quality and freedom

Emacs Copilot helps you do pair programming with a local-running LLM
that generates code completions within Emacs buffers. The LLM is run
as a sub-command that remembers your local editing history on a file
by file basis. Tokens stream into your buffer without delay as gen'd
and you can hit `C-g` to interrupt your LLM at any time. History and
memory can also be deleted from the LLM's context when deleting code
from your Emacs buffer that matches up verbatim. Copilot is language
agnostic and your programming language is determed by file extension

One really good LLM right now is WizardCoder 34b since it scores the
same as GPT-4 on HumanEval. You need a computer like a Mac Studio M2
Ultra in order to use it. If you have a mere Macbook Pro, then try the
Q3 version. If you have a modest PC then you could consider downloading
the WizardCoder-Python-13b llamafile since it's almost as good, and will
even go acceptably fast on CPU-only systems having at least AVX2 and
2200 MT/s RAM. If you're even more strapped for compute and use things
like Raspberry Pi, then give Phi-2 a spin

To get started, try writing yourself the first line of a function. For
example, you might open up a file in your editor named `hello.c` and
then type:

```c
bool is_prime(int x) {
```

Then place your caret at the end of the line, and press `C-c C-k` to
hand over control to your LLM, which should generate the rest of the
function implementation for you. Things are also tuned so the LLM is
likely to stop as soon as a function is made. Explanations and other
kind of ELI5 commentary is avoided too.

Later on, if you were to write something like this:

```c
int main() {
  for (int i = 0; i < 100;
```

And ask your LLM to complete that, then your LLM will likely recall
that you two wrote an is_prime() function earlier, even though it's
only considering those two lines in the current instruction. You'll
most likely then see it decide to generate code to print the primes

## Reference Implementation

If you've downloaded your LLM (see below) then all you really need is to
copy and paste this code into an Emacs buffer and run `M-x eval-buffer`.
You'll want to tune the code to your own personal taste. That's why it's
being presented in full as a succinct code example here.

```elisp
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
      (call-process "wizardcoder-python-34b-v1.0.Q5_K_M.llamafile"
                    nil (list (current-buffer) nil) t
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
```

## Emacs Download Link

If you don't have Emacs installed, or you use a platform like Windows
where it's normally difficult to obtain, then here's a single-file build
of Emacs that (like llamafile) is directly runnable and needn't be
installed.

- <https://cosmo.zip/pub/cosmos/bin/emacs>

See the [Gotchas](#gotchas) below if you have trouble running it. See
also the [Supported OSes and CPUs](#supported-oses-and-cpus) list too.

## LLM Download Links

Here are some LLMs that are known to work reasonably well with Emacs
Copilot, that are freely available to download online. They're all good,
but the biggest one is the best one. Choose the size that's appropriate
for your hardware.

| Model                       | Size    | License                                                                                   | [llamafile](https://github.com/mozilla-Ocho/llamafile)                                                                                                                                         |
| ---                         | ---     | ---                                                                                       | ---                                                                                                                                                                                            |
| WizardCoder-Python-34b (Q5) | 23.9 GB | [LLaMA 2](https://ai.meta.com/resources/models-and-libraries/llama-downloads/)            | [wizardcoder-python-34b-v1.0.Q5\_K\_M.llamafile](https://huggingface.co/jartine/WizardCoder-Python-34B-V1.0-llamafile/resolve/main/wizardcoder-python-34b-v1.0.Q5_K_M.llamafile?download=true) |
| WizardCoder-Python-34b (Q3) | 16.3 GB | [LLaMA 2](https://ai.meta.com/resources/models-and-libraries/llama-downloads/)            | [wizardcoder-python-34b-v1.0.Q3\_K\_M.llamafile](https://huggingface.co/jartine/WizardCoder-Python-34B-V1.0-llamafile/resolve/main/wizardcoder-python-34b-v1.0.Q3_K_M.llamafile?download=true) |
| WizardCoder-Python-13b      | 7.33 GB | [LLaMA 2](https://ai.meta.com/resources/models-and-libraries/llama-downloads/)            | [wizardcoder-python-13b-main.llamafile](https://huggingface.co/jartine/wizardcoder-13b-python/resolve/main/wizardcoder-python-13b-main.llamafile?download=true)                                |
| Phi-2                       | 2.09 GB | [microsoft-research-license](https://huggingface.co/microsoft/phi-2/resolve/main/LICENSE) | [phi-2.Q5\_K\_M.llamafile](https://huggingface.co/jartine/phi-2-llamafile/resolve/main/phi-2.Q5_K_M.llamafile?download=true)                                                                   |

Be sure to `chmod +x` your llamafile executable after you download it.
Then consider placing it on the system path. If you have any trouble
running the llamafile, then see the [Gotchas](#gotchas) section.

## Cache files

If you decide to switch models, then be sure to delete all the
`FILE.cache` files that got generated on your local filesystem.

```sh
find . -name \*.cache | xargs rm -f
```

You can also tune the Emacs LISP code above to just not use prompt
caching at all, by removing those flags. That might have a negative
impact on code completion latency though. On Apple Metal GPU, which has
extremely fast prompt loading, the slowdown might be ~1 second, but for
systems that need CPU inference it could be significantly higher.

## Gotchas

On macOS with Apple Silicon you need to have Xcode installed for
llamafile to be able to bootstrap itself.

If you use zsh and have trouble running llamafile, try saying `sh -c
./llamafile`. This is due to a bug that was fixed in zsh 5.9+. The same
is the case for Python `subprocess`, old versions of Fish, etc.

On some Linux systems, you might get errors relating to `run-detectors`
or WINE. This is due to `binfmt_misc` registrations. You can fix that by
adding an additional registration for the APE file format llamafile
uses:

```sh
sudo wget -O /usr/bin/ape https://cosmo.zip/pub/cosmos/bin/ape-$(uname -m).elf
sudo chmod +x /usr/bin/ape
sudo sh -c "echo ':APE:M::MZqFpD::/usr/bin/ape:' >/proc/sys/fs/binfmt_misc/register"
sudo sh -c "echo ':APE-jart:M::jartsr::/usr/bin/ape:' >/proc/sys/fs/binfmt_misc/register"
```

As mentioned above, on Windows you may need to rename your llamafile by 
adding `.exe` to the filename. 

Also as mentioned above, Windows also has a maximum file size limit of 4GB
for executables. The LLaVA server executable above is just 30MB shy of
that limit, so it'll work on Windows, but with larger models like
WizardCoder 13B, you need to store the weights in a separate file. An 
example is provided above; see "Using llamafile with external weights."

On WSL, it's recommended that the WIN32 interop feature be disabled:

```sh
sudo sh -c "echo -1 > /proc/sys/fs/binfmt_misc/WSLInterop"
```

On any platform, if your llamafile process is immediately killed, check
if you have CrowdStrike and then ask to be whitelisted.

## Supported OSes and CPUs

llamafile supports the following operating systems, which require a minimum 
stock install:

- Linux 2.6.18+ (ARM64 or AMD64) i.e. any distro RHEL5 or newer
- Darwin (macOS) 23.1.0+ [1] (ARM64 or AMD64, with GPU only supported on ARM64)
- Windows 8+ (AMD64)
- FreeBSD 13+ (AMD64, GPU should work in theory)
- NetBSD 9.2+ (AMD64, GPU should work in theory)
- OpenBSD 7+ (AMD64, no GPU support)

llamafile supports the following CPUs:

- AMD64 microprocessors must have SSSE3. Otherwise llamafile will print
  an error and refuse to run. This means that if you have an Intel CPU,
  it needs to be Intel Core or newer (circa 2006+), and if you have an
  AMD CPU, then it needs to be Bulldozer or newer (circa 2011+). If you
  have a newer CPU with AVX, or better yet AVX2, then llamafile will
  utilize your chipset features to go faster. There is no support for
  AVX512+ runtime dispatching yet.
- ARM64 microprocessors must have ARMv8a+. This means everything from
  Apple Silicon to 64-bit Raspberry Pis will work, provided your weights
  fit into memory.

[1] Darwin kernel versions 15.6+ *should* be supported, but we currently
    have no way of testing that.

## A note about models

The example llamafiles provided above should not be interpreted as 
endorsements or recommendations of specific models, licenses, or data 
sets on the part of Mozilla.
