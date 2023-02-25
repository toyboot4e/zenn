---
title: "AtCoder 用 Haskell 逆引き (環境構築編)"
emoji: "⛳"
type: "idea" # tech: 技術記事 / idea: アイデア
topics: ["haskell", "atcoder"]
published: true
---

# この記事は

AtCoder ([Language Test 202001](https://atcoder.jp/contests/language-test-202001) 環境) の環境構築逆引きです。とにかく動かしたい人向けとなります。最低限動作しますが、 **正しい方法ではない** と思いますのでご留意ください。指摘を頂けると助かります 🙏

# 環境構築 逆引き

## 1. Haskell をインストールしたい

[GHCup](https://www.haskell.org/ghcup/) でインストールします。 GHCup 以外のパッケージマネジャでインストールした Haskell は、干渉するためアンインストールします。

AtCoder 環境の Haskell は古いため気をつけます。

- GHC 8.8.3
  [Language Test 202001](https://atcoder.jp/contests/language-test-202001) によると、 GHC 8.8.3 を使用します。
- `stack`, `cabal`  最新版
  ビルドツールは最新版が利用できます。
- HLS (haskell language server)
  [バージョン表](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html) を見るに、 HLS 1.5.1 が GHC 8.8.3  に対応します。バージョンが古いためか変数のリネームはできません。

> ローカルでは GHC 8.8.4, HLS 1.8.0.0, lts-16.31 を使っても良いかもしれません。 HLS 1.8.0.0 では変数のりネームができますし、特に Windows では GHC 8.8.3 に致命的なバグがあるそうです。

## 2. ライブラリのドキュメントが見たい

`repa` 以外は [lts-16.11](https://www.stackage.org/lts-16.11) に入っているのでそちらを参照します。

> [lts-16.31](https://www.stackage.org/lts-16.31)

## 3. HLS を動かしたい

HLS 1.5.1 を動かすためには、プロジェクトを作る必要がある……と思います。

### ファイル構成

Haskell のビルドツールには `stack` と `cabal` があります。ここでは (特に理由なく) stack プロジェクトを作ります。

```sh
abc256/ # AtCoder Beginner Contest 256 のためのプロジェクト
├── abc256.cabal    # (Stack が自動生成するファイル)
├── a # A 問題のデータが入ったディレクトリ
│   ├── Main.hs
│   └── test-cases
├── b # B 問題のデータが入ったディレクトリ
# ~~
├── hie.yaml          # HLS を動かすために必要なファイル
├── package.yaml      # Stack project の設定ファイル
├── stack.yaml        # Stack project の設定ファイル
└── stack.yaml.lock # (Stack が自動生成するファイル)
```

### ファイル内容

:::details stack.yaml
```hs:stack.yaml
# `PATH` 中の GHC を使用する
system-ghc: true
# resolver: lts-16.31
resolver: lts-16.11
packages:
- .
```
:::

:::details package.yaml
```hs:package.yaml
dependencies:
   - base >= 4.7 && < 5

   - QuickCheck
   - array
   - attoparsec
   - bytestring
   - containers
   - deepseq
   - extra
   - fgl
   - hashable
   - heaps
   - integer-logarithms
   - lens
   - massiv
   - mono-traversable
   - mtl
   - mutable-containers
   - mwc-random
   - parallel
   - parsec
   - primitive
   - psqueues
   - random
   - reflection
   - template-haskell
   - text
   - tf-random
   - transformers
   - unboxing-vector
   - unordered-containers
   - utility-ht
   - vector
   - vector-algorithms
   - vector-th-unbox

# DRY for package.yaml executables:
# <https://www.reddit.com/r/haskell/comments/haeqin/dry_for_packageyaml_executables/>
_exe-defs: &exe-defaults
  # dependencies:
  # - abs
  ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -Wall # all warnings
  other-modules: []

executables:
  a-exe:
    <<: *exe-defaults
    source-dirs: a
    main:                Main.hs

  b-exe:
    <<: *exe-defaults
    source-dirs: b
    main:                Main.hs

  c-exe:
    <<: *exe-defaults
    source-dirs: c
    main:                Main.hs

  d-exe:
    <<: *exe-defaults
    source-dirs: d
    main:                Main.hs

  e-exe:
    <<: *exe-defaults
    source-dirs: e
    main:                Main.hs

  f-exe:
    <<: *exe-defaults
    source-dirs: f
    main:                Main.hs
```
:::

:::details hie.yaml
```hs:hie.yaml
cradle:
  stack:
    - path: "./a/Main.hs"
      component: "abc256:exe:a-exe"

    - path: "./b/Main.hs"
      component: "abc256:exe:b-exe"

    - path: "./c/Main.hs"
      component: "abc256:exe:c-exe"

    - path: "./d/Main.hs"
      component: "abc256:exe:d-exe"

    - path: "./e/Main.hs"
      component: "abc256:exe:e-exe"

    - [ ] path: "./f/Main.hs"
      component: "abc256:exe:f-exe"
```
:::

`hie.yaml` はビルドには不要ですが、 HLS を動かすためには必要です。 `hie.yaml` は [implicit-hie](https://hackage.haskell.org/package/implicit-hie) を使って生成できます。

### それでも動かない場合は

- プロジェクトの `.stack-work/`, `package.lock` や `~/.stack` を吹き飛ばしてみます。
- LSP のルートディレクトリが `abc256` のようなプロジェクトのディレクトリと一致するかを確かめます。不一致の場合はなんとかします。

## 4. 個別のファイルをビルドしたい (プロジェクト全体をビルドしたくない)

`stack run a-exe` を実行すると、全問題の実行ファイルがビルドされてしまいます。 A 問題のプログラムの動きを見たいのに、 B 問題のエラーが表示されたりします。

対策としては、 `stack run` を使わなれば良いと思います。

### `ghc` でビルドする

公式環境では `ghc -o a.out -O2 {dirname}/{filename}` でビルドしています。

Stack ユーザとしては、 `stack ghc -- a/Main.hs` のような形で stack 指定のパッケージを取り込んだ GHC でビルドできます:

```sh
$ ls a67/
Main.hs*  test-cases/

$ stack ghc -- a67/Main.hs
[1 of 1] Compiling Main             ( a67/Main.hs, a67/Main.o )
Linking a67/Main ...

$ cat a67/test-cases/sample-1.in | ./a67/ghc/Main
55
```

ただし `ghc` は `Main.hs` と同じ階層に中間ファイルと実行ファイルを生成します:

```sh
$ ls a67
Main*  Main.dyn_hi  Main.dyn_o  Main.hi  Main.hs*  Main.o  test-cases/
```

GHC のオプションを使うと、生成ファイルをコントロールできます:

- `-outputdir`: 中間ファイルを出力するディレクトリを設定できます。
- `-o`: 実行ファイルの出力先を設定できます。

```sh
$ mkdir a67/ghc

$ stack ghc -- a67/Main.hs -outputdir a67/ghc -o a67/ghc/Main
[1 of 1] Compiling Main             ( a67/Main.hs, a67/ghc/Main.o )
Linking a67/ghc/Main ...

$ ls a67/
ghc/  Main.hs*  test-cases/

$ ls a67/ghc
Main*  Main.dyn_hi  Main.dyn_o  Main.hi  Main.o
```

### Stack script として実行する、 REPL で読み込む

僕は `Main.hs` を [stack script](https://zenn.dev/mod_poppo/articles/haskell-script) として実行しています。 Stack script は質問にも便利なのでおすすめです:

```hs:Main.hs
#!/usr/bin/env stack
{- stack script --resolver lts-16.11
--package array --package bytestring --package containers --package vector --package vector-algorithms --package primitive --package transformers
-}

{-# OPTIONS_GHC -O2 #-}

main = putStrLn "Hello, world!"
```

欠点はインタープリタで動作することで、問題によってはプログラムの動作が極端に遅くなることがあります (ローカルで 3 秒、ジャッジでは 100ms など) 。

Stack script は `stack repl <file>` で読み込むこともできます。 REPL の中でテストケースを実行したり、 `:r` でリロードもできるようです。

## 5. ディレクトリ毎に Haskell 環境を分けたい

[direnv](https://direnv.net/) で `PATH` に入る `ghc`, `stack`, `cabal`, `haskell-language-server` を切り替えられます。

公式の方法は存じ上げません……。

## 6. テストケースをローカルで実行したい

コマンドラインツールを使用します:

- [acc](https://github.com/Tatamo/atcoder-cli) (`atcoder-cli`)
  - テストケースのダウンロード
  - テンプレートを元にプロジェクトを作成
  - 解答の提出
- [oj](https://github.com/online-judge-tools/oj) (`online-judge-tools`)
  - テストケースの実行

## 7. 提出結果のハイライトを正したい

Haskell の変数名には `x'` のように `'` 記号を含むものが現れますが、これが文字のクオートと勘違いされてハイライトが崩れます。

ユーザースクリプトを拝借して直せます:

https://qiita.com/mod_poppo/items/af11f07169fa9bdab844

ブラウザ拡張の中には、ユーザーのレートや問題の難度を表示してくれるものもあるので入れておきましょう:

https://scrapbox.io/magurofly/AtCoder%E3%82%92%E3%81%99%E3%82%8B%E3%81%A8%E3%81%8D%E3%80%81%E5%85%A5%E3%82%8C%E3%81%A6%E3%81%8A%E3%81%8F%E3%81%A8%E3%81%84%E3%81%84%E6%8B%A1%E5%BC%B5%E6%A9%9F%E8%83%BD%E3%81%AA%E3%81%A9

## 8. 人の動向が知りたい

### 人の成績が見たい

順位表にある「⚙️カスタマイズ」のアイコンをクリックして、お気に入りユーザーの順位を確認できます:

![](/images/atcoder/atcoder-favorites.png)
*Haskell 界で A 問題最速を目指す図*

`[お気に入り管理]` をクリックすると、他のブラウザで設定したお気に入りユーザの情報を同期できます。 (自動で同期してほしい気はします) 。

### 人の AC 状況を見たい

また [AtCoder Problems](https://kenkoooo.com/atcoder/#/table/) で人の解答状況を自分の解答状況と比較することができます。

# 終わりに

快適な AC を！

## おまけ

- [Haskellで戦う競技プログラミング 第2版](https://booth.pm/ja/items/1577541) および [著者ブログ](https://blog.miz-ar.info/2019/09/techbookfest7-announce/)
- [The Haskell Cast #13](https://podcasts.apple.com/no/podcast/episode-13-john-wiegley-on-categories-and-compilers/id694047404?i=1000385334618&l=nb)
- [Rebuild FM #352](https://rebuild.fm/352/)
- [Misleading Chat #88](https://misreading.chat/2020/10/27/88-a-history-of-haskell-being-lazy-with-class/)

