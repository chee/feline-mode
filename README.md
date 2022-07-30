<div align="center" style="text-align: center">
  <h1> feline-mode </h1>

  <div>🐈‍⬛</div>
  <p>
    <i>A light, fast, no-frills emacs modeline.</i>
  </p>
</div>

The modeline is really noisy. The prettier ones add like 300ms to my emacs
start-up time. `feline-mode` is not noisy, it loads almost instantly.

With the built in mode-line the `global-mode-string` is usually pushed right off
the edge of the screen by minor modes. `feline-mode` does not show minor modes.

`feline-mode` shows the following items:

- evil state, if you use evil
- *, if the buffer is modified
- the major mode (which can be aliased to save space)
- the buffer name
- the current `project.el` project, if there is one
- the line and column (like L17C34)
- the `global-mode-string`

## configuration

install it however you like! download it and put it in your `load-path` or use
`straight.el` pointed at this repo.

there are options. there are faces. see `M-x` `customize-group` `feline` :)

```elisp
(use-package feline
  :config (feline-mode)
  :custom
  (feline-line-prefix "L")
  (feline-column-prefix "C")
  (feline-mode-symbols
   '(emacs-lisp-mode "λ"
     python-mode "py"
     typescript-mode "ts"
     rustic-mode "🦀"
     rust-mode "🦀"
     zig-mode "🦎"
     scheme-mode "🐔")))`
```
