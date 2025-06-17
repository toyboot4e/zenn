---
title: "Zenn Book = One Org File"
topics: [org]
emoji: 🗿
type: tech
published: true
---



# 背景

:::message
[Vim 駅伝](https://vim-jp.org/ekiden/) 2025/06/18 の投稿です。前回の記事は sinotca さんの [Excel マクロランチャーを作ろう！](https://zenn.dev/sinotca/articles/6bf254d056af28) でした。
:::

Zenn ユーザの皆様は [`zenn-cli`](https://zenn.dev/zenn/articles/zenn-cli-guide) を使い、快適な執筆生活を送っていると思います。僕も `zenn-cli` のレスポンスの良さが好きで、満足度の高い生活を送っていました。

ただ、本はチャプターが増えると管理が大変になりました。特に記事一覧の表示方法が 3 種類になって混乱します:

| 表示箇所      | 表示形式         | 並び     |
| ------------- | ---------------- | -------- |
| Zenn サイドバー | タイトル         | チャプター順 |
| `config.yaml` | ファイル名 (`.md` 無し) | チャプター順 |
| エディタ      | ファイル名       | アルファベット順 |

ファイル数が 20 を超えると、チャプターの移動がままならなくなります。少なくとも、 [思考の速度で編集](https://www.kadokawa.co.jp/product/311865500010/) できない環境です。

この 3 つの並びを一元化できないか考えた時に [`ox-zenn.el`](https://github.com/conao3/ox-zenn.el) と出会いました。


## Zenn Book の見出しについて

[`ox-zenn.el`](https://github.com/conao3/ox-zenn.el) は Org ファイルを Markdown ファイルに変換できます。また Org ファイルの一部 (*subtree*) を Markdown ファイルに変換する機能があります。したがって 1 つの Org ファイルに全てのチャプターを収納し、 Org から `config.yaml` や `*.md` を展開できます。

これを活かせば、エディタとブラウザ間で見出しの内容が一致する環境を作ることができます。 `config.yaml` も手動で編集する必要はありません:

![cover](/images/org-zenn-headings.png)

実際 [競プロ盆栽.hs](https://zenn.dev/toyboot4e/books/kyopro-bonsai-hs) は Org ファイルとして作成しました。こうした環境の作り方を紹介して行こうと思います。


## 備考. Org とは

Org はマークアップ言語です。主に Emacs で使用されており、他のエディタでは人気が高くありません。理由の一端は、 Emacs における Org の実装 (`org-mode`) があまりにも重厚過ぎて、移植版の実装が追いつかないためだと思います。

しかし Emacs 以外のエディタでも、 Org を普通のマークアップ言語として使うことはできます。誰でも気兼ねなく Org を使える風潮になれば良いなと思います。


## ソースファイル

今回作成のスクリプトは以下に保存しています:

https://github.com/toyboot4e/org-zenn-example

:::message
Windows では動作未確認です。
:::


# `emacs` コマンドの導入

今回はエディタに依存しない Zenn 執筆環境を構築しますから、テキストエディタとしての Emacs は封印します。代わりに `emacs` コマンドを ELisp (Emacs Lisp) のインタープリタとして使用します。

`emacs` はインストール済みとして進めて行きます。


## ファイル構成

通常、 `zenn-cli` は次のようなファイル構成を作成します:

```txt
.
├── articles/
│   └── my-article.md
└── books/my-book/
    ├── config.yaml
    ├── example1.md
    └── example2.md
```

今回は Org ファイル使用のため、以下の構成を追加します:

```txt
.
├── org-articles/
│   └── my-org-article.org
├── org-books/
│   └── my-org-book.org
└── elisp/
    ├── cat.el
    ├── export-org-article.el
    └── export-org-book.el
```

-   `org-articles/`: Org で書く記事を保存します。
-   `org-books/` Org で書く本を保存します。
-   `elisp/`: Org, Markdown 間の変換用スクリプトを保存します。


## Hello, ELisp!

`emacs` は次の形で ELisp スクリプトを実行できます:

```sh
$ emacs --quick --script <スクリプトファイル>
```

-   [-Q, &#x2013;quick](https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html#index-_002d_002dquick)
-   [&#x2013;batch](https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html#index-_002d_002dscript)

たとえば引数指定のファイル内容を表示するだけのコマンドを作ってみます:

```elisp:elisp/cat.el
(with-temp-buffer
  (insert-file-contents (car argv))
  (princ (buffer-string)))
```

これは次のような ELisp プログラムです:

-   空の一時バッファを作成する
-   引数指定のファイル内容をバッファに流し込む
-   バッファの内容を出力する

`emacs` は自分をテキストエディタだと思い込んでいるので、奇妙なスクリプトになりました。

このスクリプトを使って、 `./org-articles/my-org-article.org` のファイル内容を表示してみます:

```sh
$ emacs --quick --script elisp/cat.el ./org-articles/my-org-article.org
#+TITLE: Org Article
#+GFM_TAGS: org
#+GFM_CUSTOM_FRONT_MATTER: :emoji 🗿 :type tech
<略>
```

ファイル内容を表示できました。 `cat.el` 完成です 🐱


## 記事を export する

次は Org ファイル製の記事を Markdown に変換するスクリプトを作成します。具体例としては、 `org-articles/file.org` に適用すると `articles/file.md` を生成します。記事を Org で書くメリットはあまり無いですが、練習にはちょうど良いかと思います。

僕は Emacs リテラシーが高くないので、 AI に聞きながら path を整理しました:

```elisp:elisp/export-org-article.el
(with-temp-buffer
  (insert-file-contents (car argv))
  (let* (;; */zenn/org-articles/my-org-article.md
         (src-file (expand-file-name (car argv)))
         ;; */zenn/org-articles
         (org-dir (file-name-directory src-file))
         ;; */zenn
         (zenn-dir (file-name-directory (directory-file-name org-dir)))
         ;; */zenn/articles/
         (article-dir (concat zenn-dir "articles/"))
         ;; my-org-article
         (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md"))
         ;; */zenn/articles/my-org-article.md
         (dst-path (concat article-dir file-name)))
    ;; (org-export-to-file 'zennmd outfile)
    (print src-file)
    (print org-dir)
    (print zenn-dir)
    (print article-dir)
    (print file-name)
    (print dst-path)))
```

`print` 出力を見るため、実行してみます:

```elisp
$ emacs -Q --script export-org-article.el org-articles/org-article.org
```

```txt
"/home/tbm/dev/hs/org/org-zenn-example/org-articles/my-org-article.org"

"/home/tbm/dev/hs/org/org-zenn-example/org-articles/"

"/home/tbm/dev/hs/org/org-zenn-example/"

"/home/tbm/dev/hs/org/org-zenn-example/articles/"

"my-org-article.md"

"/home/tbm/dev/hs/org/org-zenn-example/articles/my-org-article.md"
```

先頭、末尾で変換元、変換先のファイル名が取れています:

-   `"/home/tbm/dev/hs/org/org-zenn-example/org-articles/my-org-article.org"`
-   `"/home/tbm/dev/hs/org/org-zenn-example/article/my-org-article.md"`

後は `org-export-to-file` コマンドを呼び出しすれば OK です:

```elisp
;; パッケージのセットアップ
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org" . "https://orgmode.org/elpa/")))
  (package-initialize))

(use-package org
  :ensure t)

(use-package ox-zenn
   :ensure t)

(require 'org)
(require 'ox-zenn)

;; パスの整理
(with-temp-buffer
  (insert-file-contents (car argv))
  (let* (;; */zenn/org-articles/my-org-article.md
         (src-file (expand-file-name (car argv)))
         ;; */zenn/org-articles
         (org-dir (file-name-directory src-file))
         ;; */zenn
         (zenn-dir (file-name-directory (directory-file-name org-dir)))
         ;; */zenn/articles/
         (article-dir (concat zenn-dir "articles/"))
         ;; my-org-article
         (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md"))
         ;; */zenn/articles/my-org-article.md
         (dst-path (concat article-dir file-name)))
    ;; (print src-file)
    ;; (print org-dir)
    ;; (print zenn-dir)
    ;; (print article-dir)
    ;; (print file-name)
    ;; 出力先のパスを表示
    (print dst-path)
    ;; export 実行
    (org-export-to-file 'zennmd dst-path)))
```

これを実行すると:

```sh
$ emacs -Q --script export-org-article.el org-articles/org-article.org
```

無事に `articles/my-org-article.md` が生成されました。 `export-org-article.el` 完成です。後は [`watchexec`](https://github.com/watchexec/watchexec) 等で `.org`, `.md` 間を同期すれば、 Zenn の記事が Org で書けるようになります。

> 注意点として、 Org ファイルにリンクミス等があると、膨大なエラー出力が出ます:
> 
> ```txt
> <中略>
> (file-name-nondirectory src-file)) ".md")) (dst-path (concat article-dir file-name))) (print dst-path) (org-export-to-file 'zennmd dst-path))
>   (progn (insert-file-contents (car argv)) (let* ((src-file (expand-file-name (car argv))) (org-dir (file-name-directory src-file)) (zenn-dir (file-name-directory (directory-file-name org-dir))) (article-dir (concat zenn-dir "articles/")) (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md")) (dst-path (concat article-dir file-name))) (print dst-path) (org-export-to-file 'zennmd dst-path)))
>   (unwind-protect (progn (insert-file-contents (car argv)) (let* ((src-file (expand-file-name (car argv))) (org-dir (file-name-directory src-file)) (zenn-dir (file-name-directory (directory-file-name org-dir))) (article-dir (concat zenn-dir "articles/")) (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md")) (dst-path (concat article-dir file-name))) (print dst-path) (org-export-to-file 'zennmd dst-path))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))
>   (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert-file-contents (car argv)) (let* ((src-file (expand-file-name (car argv))) (org-dir (file-name-directory src-file)) (zenn-dir (file-name-directory (directory-file-name org-dir))) (article-dir (concat zenn-dir "articles/")) (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md")) (dst-path (concat article-dir file-name))) (print dst-path) (org-export-to-file 'zennmd dst-path))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer))))
>   (let ((temp-buffer (generate-new-buffer " *temp*" t))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert-file-contents (car argv)) (let* ((src-file (expand-file-name (car argv))) (org-dir (file-name-directory src-file)) (zenn-dir (file-name-directory (directory-file-name org-dir))) (article-dir (concat zenn-dir "articles/")) (file-name (concat (file-name-sans-extension (file-name-nondirectory src-file)) ".md")) (dst-path (concat article-dir file-name))) (print dst-path) (org-export-to-file 'zennmd dst-path))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))
>   #<subr F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_101>(#<buffer  *load*> "/home/tbm/write/zenn/elisp/export-org-article.el")
>   load-with-code-conversion("/home/tbm/write/zenn/elisp/export-org-article.el" "/home/tbm/write/zenn/elisp/export-org-article.el" nil t #<subr F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_101>)
>   command-line--load-script("/home/tbm/write/zenn/elisp/export-org-article.el")
>   command-line-1(("-scriptload" "elisp/export-org-article.el" "org/write-org-for-great-good.org"))
>   command-line()
>   normal-top-level()
> Wrong type argument: char-or-string-p, (verbatim (:standard-properties [1510 nil nil nil 1518 1 nil nil nil nil ...] :value "emacs"))
> ```
> 
> ログを見るのも大変なので、二分探索による解決が速いと思います。 Org ファイルを半分ずつ消して原因を絞り込むことになります。ちょっと悲しい作業ですが……。


## 本を export する

本題です。 1 つの Org ファイルから Zenn Book 全体を展開するスクリプトを作ります。


### パスの整理

こちらもパスの整理から入ります。引数として `org-book/my-book.org` (例) を受け取った時に、 `books/my-book/` パスを作成します:

```elisp:elisp/export-book.el
(with-temp-buffer
  (insert-file-contents (car argv))
  (let* (;; */zenn/org-books/my-org-book.el
         (src-file (expand-file-name (car argv)))
         ;; */zenn/org-books
         (org-dir (file-name-directory src-file))
         ;; */zenn
         (zenn-dir (file-name-directory (directory-file-name org-dir)))
         ;; my-org-book
         (book-name (file-name-sans-extension (file-name-nondirectory src-file)))
         ;; */zenn/books/my-org-book/
         (book-dir (concat zenn-dir "books/" book-name "/")))
    (print book-dir)))
```

実行すると、変換先の book ディレクトリが取れています:

```sh
$ emacs -q --script elisp/export-org-book.el ./org-books/my-org-book.org
"/home/tbm/dev/hs/org/org-zenn-example/books/my-org-book/"
```

-   `"/home/tbm/dev/hs/org/org-zenn-example/article/my-org-article/"`


### レベル 1 見出しを走査する

今回対象の Org ファイルは、次のような内容です:

```org
#+TITLE: Zenn Book in Org

* Heading 1

* TODO Heading 2

* Heading 3
```

[`org-map-entries`](https://orgmode.org/manual/Using-the-Mapping-API.html#index-org_002dmap_002dentries) を使うと、 Org ドキュメント中の要素を走査できます。

```elisp
(with-temp-buffer
  (insert-file-contents (car argv))
  (org-mode)
  (org-map-entries
   (lambda ()
     (let ((title (org-entry-get nil "ITEM")))
       (message title)))
   "LEVEL=1")))
```

実行結果は以下の通りでした:

```sh
$ emacs -q --script elisp/export-org-book.el ./org-books/my-org-book.org
Heading 1
TODO Heading 2
Heading 3
```

Org には見出しに [TODO](https://orgmode.org/manual/TODO-Basics.html) をつける機能があります。 `TODO` 付きの見出しは下書きとみなし、スキップすることにすると、 `Heading 2` を下書きとして省略されます:

```elisp
(org-map-entries
 (lambda ()
   (let ((title (org-entry-get nil "ITEM")))
     (unless (string= (org-get-todo-state) "TODO")
       (message title))))
 "LEVEL=1")
```

```sh
$ emacs -q --script elisp/export-org-book.el ./org-books/my-org-book.org
Heading 1
Heading 3
```


### レベル 1 見出しを `.md` ファイルに変換する

Org ファイルに `EXPORT_FILE_NAME` *property* を記載します。たとえば `chapter-1` を指定すると、 `chapter-1.md` が生成されるようにします:

```nil
#+TITLE: Org Book

* Heading 1
:PROPERTIES:
:EXPORT_FILE_NAME: chapter-1
:END:

テスト

* TODO Heading 2
:PROPERTIES:
:EXPORT_FILE_NAME: chapter-2
:END:

テスト

* Heading 3
:PROPERTIES:
:EXPORT_FILE_NAME: chapter-3
:END:

テスト
```

`org-map-entries` でレベル 1 見出しを走査するとき、 `lambda` 中で `org-zenn-export-to-markdown` すると、相対パスへ `.md` ファイルが生成されます。これが `book` ファイル中に出力されるよう `cd` しておけば、目的の場所に `.md` ファイルが生成されます:

```elisp
(with-temp-buffer
  (insert-file-contents (car argv))
  (org-mode)
  (let* (;; */zenn/org-books/my-org-book.el
         (src-file (expand-file-name (car argv)))
         ;; */zenn/org-books
         (org-dir (file-name-directory src-file))
         ;; */zenn
         (zenn-dir (file-name-directory (directory-file-name org-dir)))
         ;; my-org-book
         (book-name (file-name-sans-extension (file-name-nondirectory src-file)))
         ;; */zenn/books/my-org-book/
         (book-dir (concat zenn-dir "books/" book-name "/")))
    (print book-dir)

    (unless (file-directory-p book-dir)
      (make-directory book-dir t))
    (cd book-dir)

    (org-map-entries
     (lambda ()
       (let ((title (org-entry-get nil "ITEM")))
         (unless (string= (org-get-todo-state) "TODO")
           ;; (message title)
           (org-zenn-export-to-markdown nil t))))
     "LEVEL=1")))
```

```sh
$ emacs -q --script elisp/export-org-book.el ./org-books/my-org-book.org

$ fd . books/my-org-book | as-tree
books/my-org-book
├── chapter-1.md
└── chapter-3.md
```

成功です！　後は `config.yaml` を自動生成できれば、 Zenn Book ファイル全体を Org ファイルから展開できます。


### `config.yaml` を自動生成する

最後に `config.yaml` の生成部分です。 Org ファイル中に `config.yaml` を埋め込んでおきます:

```org
* TODO メタデータ

** =config.yaml=

#+NAME: config.yaml
#+BEGIN_SRC yaml
title: "My Org Book"
summary: "Greate Org"
topics: ["org"]
published: true
price: 0
chapters:
#+END_SRC
```

`chapters` 以下をスクリプト内で生成すると、次のような `config.yaml` が出力されます:

```yaml:books/my-org-book/config.yaml
title: "My Org Book"
summary: "Greate Org"
topics: ["org"]
published: true
price: 0
chapters:
- chapter-1
- chapter-3
```

スクリプトは `org-map-entries` 等でゴリゴリ実装します。詳細は [リポジトリ](https://github.com/toyboot4e/org-zenn-example)= の [該当スクリプト](https://github.com/toyboot4e/org-zenn-example/blob/main/elisp/export-org-book.el) をご覧ください。

![cover](/images/org-zenn-headings.png)
*環境構築できれば、こんな感じになります*


# 終わりに

Zenn Book を Org で書くためのスクリプトを作成しました。お使いのエディタが何であれ、 Org ファイルがご利用可能です。快適な執筆生活をお送りください！

CLI ツールとしての Emacs の可能性にも注目頂けると幸いです。何てことのないスクリプトで結構便利になったと思います。

Emacs プロの方がいらっしゃれば、あちゃーという箇所もあったと思いますが、ご容赦ください。それではご一読頂きありがとうございました！


## 参考

https://zenn.dev/conao3/articles/ox-zenn-usage
