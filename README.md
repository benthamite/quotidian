# `quotidian`: Display quotations from authoritative sources

`quotidian` shows quotations from the Oxford Dictionary of Quotations and other authoritative sources. The corpus is a list of plain JSON files, read once per session, so you can extend it with anything you care to transcribe.

There are two ways to read them:

- **In the echo area.** `quotidian-echo` puts a quotation where Emacs puts its own messages, and is meant to be called from a hook that fires when you are about to wait — a compilation, a package update, a long shell command. It never writes to `*Messages*`, never signals, and never truncates: a quotation too tall for the echo area is passed over in favour of another one, not shortened.
- **In a review buffer.** `quotidian` opens a full-frame buffer with one quotation centred in it, wrapped for reading rather than stretched to the width of your frame. `n` and `SPC` move forward, `p` back through what you have already seen, `w` copies, and `q` leaves and puts your windows back as they were.

Both entry points share one corpus, one formatter, and three faces, so a quotation looks the same wherever it appears.

The package ships public-domain quotations only. To read from a dictionary that is still under copyright, transcribe the material into a corpus file of your own and add it to `quotidian-corpus-files`.

## Installation

**Requirements:** Emacs 29.1 or later. No dependencies beyond what Emacs bundles.

The bundled corpus lives in the repository's `data` directory. Elpaca and straight.el build a package from the files its recipe names, and neither copies `data` by default, so **the recipe must name it explicitly** — without that, the package finds no quotations at all.

### Elpaca

```emacs-lisp
(use-package quotidian
  :ensure (:host github
           :repo "benthamite/quotidian"
           :files (:defaults "data"))
  :defer t)
```

### straight.el

```emacs-lisp
(use-package quotidian
  :straight (:host github
             :repo "benthamite/quotidian"
             :files (:defaults "data")))
```

### package-vc (built-in since Emacs 30)

`package-vc` clones the whole repository, so no files directive is needed.

```emacs-lisp
(use-package quotidian
  :vc (:url "https://github.com/benthamite/quotidian"))
```

## Quick start

```emacs-lisp
(use-package quotidian
  :ensure (:host github
           :repo "benthamite/quotidian"
           :files (:defaults "data"))
  :bind ("C-c q" . quotidian)
  :init
  (add-hook 'compilation-start-hook (lambda (_process) (quotidian-echo))))
```

Run `M-x quotidian` to read quotations one at a time. To add your own, write a JSON array of objects with `text` and `author` — and optionally `source`, `date`, and `tags` — add the file to `quotidian-corpus-files`, and run `M-x quotidian-reload`, which reports how many quotations it loaded and how many entries it had to drop.

## Documentation

For a comprehensive description of the corpus format, all user options, commands, and faces, see the [manual](README.org).

## License

`quotidian` is licensed under the GPL-3. See [COPYING.txt](COPYING.txt) for details.
