---
title: "[1-1] Hello, world!"
---

Haskell をインストールし、スクリプト実行と REPL からの実行を確認します。初めて Haskell に触るという人はご覧ください。

# インストール

[ghcup](https://www.haskell.org/ghcup/) で以下のツールをインストールします:

```sh
$ ghcup install ghc 9.4.5     # コンパイラ
$ ghcup install cabal latest  # ビルドツール (1)
$ ghcup install stack latest  # ビルドツール (2)
$ ghcup install hls 2.2.0     # 言語サーバ (セットアップは後ほど)
```

GHC, HLS のバージョンの根拠は以下です:

- GHC: [2023 年 12 月における AtCoder のジャッジ環境](https://img.atcoder.jp/file/language-update/language-list.html)
- HLS: [GHC version support - haskell-language-server documentation](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)

インストール後は、 `PATH` が通っていることを確認します:

```sh
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.4.5
```

# スクリプト実行

まず `Main.hs` を作成します:

```hs:Main.hs
main :: IO ()
main = putStrLn "Hello, world!"
```

`runghc` コマンドで `Main.hs` をスクリプト実行できます:

```sh
$ runghc Main.hs
Hello, world!
```

スクリプト実行は 1 つのファイルで完結するお手軽な実行方法です。試し書きに良いと思います。

# REPL 実行

`ghc` には、 Haskell をインタラクティブに実行する `ghci` コマンドが付属します。いわゆる REPL (read-evaluate-print loop) です。

早速 `ghci` を起動してみます:

```sh:bash から ghci を起動する
$ ghci
ghci>
```

`ghci` の中で式を入力すると、計算結果を表示してくれます:

```hs:ghci で式を評価する
ghci> 1 + 2
3
ghci> map (+ 1) [1, 2, 3]
[2,3,4]
ghci> -- (((0 + 1) + 2) + 3)
ghci> foldl (+) 0 [1, 2, 3]
6
```

止めたくなったら `:quit` (`:q`) を実行します:

```sh:終了
ghci> :quit
$
```

> `ghci` を電卓として使うこともあります。

# REPL からの `main` 関数起動

今度は `Main.hs` を読み込んで `ghci` を起動し、REPL から `main` 関数を呼び出してみます:

```sh
$ ghci Main.hs
ghci> main
Hello, world!
```

`Main.hs` を書き換えた後は、 `:reload` (`:r`) コマンドでソースファイルを再読み込みできます。今度はドイツ語で `Hello, world!` してみます:

```hs:Main.hs (編集後)
main :: IO ()
main = putStrLn "Hallo, welt!"
```

```sh:ドイツ語で Hello, world!
ghci> :r
Ok, one module loaded.
ghci> main
Hallo, welt!
```

このように、逐次プログラムを修正しながら REPL で動作確認できます。

# 以上

簡易環境構築を行い、 Haskell のスクリプト実行と REPL を確認しました。スクリプト実行においては `main` 関数が呼び出しされるのに対し、 REPL からは任意の関数を呼び出しできます。 `main` 以外の関数の動作を見るために、しばしば REPL も利用するかもしれません。

次章から問題を解いていきます。より本格的な環境構築、実行ファイルのビルドや言語サーバのセットアップは、 [1-3] 章で紹介します。

