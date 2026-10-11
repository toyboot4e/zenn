---
title: "bundler-hs で自作ライブラリを qualified import する！"
topics: [haskell, atcoder]
emoji: 📦
type: idea
published: true
---



# 背景

> Haskell で [AtCoder](https://atcoder.jp/) コンテストに参加する人のための記事です。

Haskell の名前空間は物理的なファイルに対応します。言語によってはファイル内の `namespace` や `mod` 宣言により名前空間を拡張できますが、 Haskell にそのような抜け道は存在しません。

競技プログラミングでは `Main.hs` に相当する ****単一のファイル**** を提出します。したがって `Main.hs` 以下に各モジュールの内容を展開したものでした:

```haskell
newQ :: Int -> IO Queue
newQ capacity = do {- ... -}
```

もちろん名前が衝突すればコンパイルエラーとなるため、 `new` ではなく `newQ` とするように、ユニークな名前を設定します。これが Haskell 本来の書き味を損なっており、体験が悪かったです。


# bundler-hs

AI で [bundler-hs](https://github.com/toyboot4e/bundler-hs) を作成しました。 [bundler-hs](https://github.com/toyboot4e/bundler-hs) は名前空間を考慮して提出ファイルを生成するため、ある程度は元の問題を回避できます。


## インストール

Nix ユーザの方は、 `nix run` により `bundler-hs` を実行できます:

```sh
alias bundler-hs='nix run github:toyboot4e/bundler-hs --'
```

それ以外の方は、お手数ですが `cabal install` するなどのセットアップが必要です:

```sh
git clone https://github.com/toyboot4e/bundler-hs # or: ghq get
cd bundler-hs
cabal update
cabal install exe:bundler-hs # ~/.local/bin/bundler-hs
```


## 例: `A.f` と `B.f`

ライブラリ側に同名の関数 `A.f` と `B.f` があるとします:

```hs
-- ~/MyLibrary/A.hs
f :: Int
f = 10
```

```hs
-- ~/MyLibrary/B.hs
f :: Int
f = 77
```

これらを `Main.hs` から使用します:

```hs
-- Main.hs
import A qualified as A
import B qualified as B

main :: IO ()
main = print $ A.f + B.f
```

`Main.hs` を AtCoder 提出用の `Main-Bundled.hs` ファイルに変換します:

```console
$ bundler-hs --lib ~/MyLibrary Main.hs > Main-Bundled.hs
```

```hs
main :: IO ()
main = print $ fA + fB

-- ### A
fA :: Int
fA = 10

-- ### B
fB :: Int
fB = 77
```

無事、名前衝突を避けた形に展開されました。このファイルはそのまま AtCoder に提出できます。 ****ヤッター！****


## Minify, tree shaking

ライブラリが増えると、提出ファイルが 1,000 行以上のテンプレートを含む場合もあります。問題の解答部分を見やすくするため、 `--minify` 等のオプションを用意しています:

```sh
bundler-hs \
  --lib ~/MyLibrary \
  --minify-lib --minify-import --minify-language-extensions \
  --tree-shake \
  --no-format \
  --embed-position before \
  Main.hs > Main-Bundled.hs
```

この設定では、以下のような `Main-Bundled.hs` が生成されます:

```hs
module Main (main) where
fA :: Int ; fA = 10 ; fB :: Int ; fB = 77
main :: IO ()
main = print $ fA + fB
```

巨大なライブラリを使った場合でも、ファイルの 1 行目にすべてのモジュールを埋め込むことができます。

> より大きなコードの例として、 [僕の提出 (例)](https://atcoder.jp/contests/abc478/submissions/79697747) は常に `minify` の設定をしています。 AtCoder アカウントをお持ちの方はご覧いただけます。


## 注意点


### フォーマットが保たれない

[bundler-hs](https://github.com/toyboot4e/bundler-hs) は [ghc-lib-parser](https://hackage.haskell.org/package/ghc-lib-parser) でパースした後の AST に対して、リネーム等の操作をします。 AST から元のソースコードを復元するため、ライブラリ部分のコメントが消え、フォーマットも保たれません。


### Bundle 後に名前衝突が起きることがある

Import の衝突は自動的に回避されません。たとえば `import Data.Map` のような open import は、 Prelude の `filter` 関数などと名前が衝突します。ライブラリ側の調整が必要になるため、動作確認した方が良いです。


### その他

-   [bundler-hs](https://github.com/toyboot4e/bundler-hs) はノールックの AI 生成で運用しています。色々と至らない点もあると思いますが、ご容赦ください。
-   [bundler-hs](https://github.com/toyboot4e/bundler-hs) は ****限定的なユースケースでのみ動作します**** 。使うと致命的な問題が見つかると思います (おそらく Windows など……) 。 AI ポン出しの issue で問題ありませんので、見つけた場合はぜひご共有ください。


# まとめ

[bundler-hs](https://github.com/toyboot4e/bundler-hs) のおかげでローカルライブラリの qualified import が捌けるようになりました。僕自身は [ghc-lib-parser](https://hackage.haskell.org/package/ghc-lib-parser) を乗りこなすことができませんでしたが、 AI は難なく成し遂げてしまいました。

ご自身でも [bundler-hs](https://github.com/toyboot4e/bundler-hs) 相当のツールが手軽に生成できる時代ですので、快適な環境でプログラミングして行きましょう！
