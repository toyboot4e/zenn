---
title: "付録 A. ox-zenn.el"
---


# 使用技術

この投稿は Emacs の [org-mode](https://orgmode.org/) で作成し、 [`ox-zenn.el`](https://github.com/conao3/ox-zenn.el) により Zeen Markdown に変換して生成しました。

https://zenn.dev/conao3/articles/ox-zenn-usage


## 見出しの見え方

`org-mode` 上の見出しの並びがそのまま Zenn Book と対応します。ファイル名の順番は Zenn の表示順と一致しなくて不便でしたが、 `ox-zenn.el` のおかげで大層便利になりました。

![ox-zenn.el](/images/kyopro-bonsai-hs/ox-zenn.png)

https://github.com/conao3/ox-zenn.el


## 全体像

以下のような `.org` ファイルを自作関数に食わせて、 `config.yaml` や `.md` ファイルを生成しています。

```org
#+TITLE: 競プロ盆栽.hs
#+BOOK_DIR: ../books/kyopro-bonsai-hs
#+PROPERTY: header-args :results output

* 記事一覧の生成スクリプト

#+NAME: zenn-headings
#+BEGIN_SRC elisp :results output
(org-map-entries
  (lambda ()
    (let* ((title (org-entry-get nil "EXPORT_FILE_NAME"))
           (is-draft (org-entry-get nil "DRAFT")))
      (when (and title (not is-draft))
        (princ (concat "- " title "\n")))))
  "LEVEL=1")
#+END_SRC

* =config.yaml=

#+BEGIN_SRC yaml :tangle ../books/kyopro-bonsai-hs/config.yaml :noweb yes
:title: "競プロ盆栽.hs"
:summary: "Haskell の AtCoder 用自作ライブラリ解説"
:topics: ["haskell", "atcoder"]
:published: false
:price: 0
:chapters:
<<zenn-headings()>>
#+END_SRC

* 表紙
:PROPERTIES:
:EXPORT_FILE_NAME: cover
:END:

..

* 始めに
:PROPERTIES:
:EXPORT_FILE_NAME: intro
:END:

..
```

Export 用の関数は以下です。参考に……なることがあるんでしょうか？:

```haskell
(defun org-zenn-export-buffer-to-book (&optional org-dir)
    "Runs subtree export to each level 1 heading, respecting `#+BOOK_DIR'."
    (interactive)
    ;; tangle `config.yml'
    (org-babel-tangle)
    ;; export all the headings
    (let* ((default-dir default-directory)
           (pub-dir (car (cdr (car (org-collect-keywords '("BOOK_DIR")))))))
        ;; cd into the target directory
        (when pub-dir (cd pub-dir))
        ;; export all
        (unwind-protect
                (org-map-entries
                 (lambda ()
                     (let ((exports (org-entry-get nil "EXPORT_FILE_NAME")))
                         (when exports
                             (org-zenn-export-to-markdown nil t))))
                 "LEVEL=1")
            ;; be sure to come back to the default directory
            (when pub-dir (cd default-dir)))))
```
