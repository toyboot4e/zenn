---
title: "Assert 文の共通化"
---


# `check` シリーズ

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) では、添字境界チェックの関数を作成しました:

```haskell
{-# INLINE checkIndex #-}
checkIndex :: (HasCallStack) => String -> Int -> Int -> ()
checkIndex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid index `" ++ show i ++ "` over length `" ++ show n ++ "`"
```

[Haskellのassertを文っぽく使う](https://qiita.com/mod_poppo/items/b3b415ea72eee210d222) の精神で、次のように使用できます:

```haskell
merge :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Int
merge dsu@Dsu {..} a b = stToPrim $ do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.merge" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.merge" b nDsu
  -- ~~
```

これで多くのコードを集約し、エラー文の品質も担保できました。実行効率もそんなに悪くありません。良いことづくめ……と、本当にこれで良かったでしょうか。


## 部分関数を許容すべきか

最近試した [PureScript](https://www.purescript.org/) では、大まかに言って部分関数が禁止されていました。部分関数を書いた場合は [`unsafePartial`](https://pursuit.purescript.org/packages/purescript-partial/1.2.0/docs/Partial.Unsafe#v:unsafePartial) 越しに呼ぶことになるため、 ~~屈辱です~~ 代わりに `Maybe` や `Either` を使った全域関数が主になります。

一方 Haskell では部分関数が書き放題です。 [`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) も、 *比較的* ACL に忠実なライブラリであるとして、部分関数を主に据えてリリース予定でした。しかし今一度全域関数を検討したくなりました。お手本として `vector` パッケージを参照します。


## test, error, assert を分ける

やはり [`readMaybe`](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic-Mutable.html#v:readMaybe) のように、全域関数も用意するのが無難な線です。上の `merge` の例では、次のように 3 つの関数に分けてしまいたいです:

```haskell
-- Int を返す
merge :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Int
merge dsu a b
  | not (testIndex a nDsu) = errorIndex a nDsu
  | not (testIndex b nDsu) = errorIndex b nDsu
  | otherwise = unsafeMerge dsu a b

-- Maybe Int を返す
mergeMaybe :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m (Maybe Int)
mergeMaybe dsu a b
  | not (testIndex a nDsu) = pure Nothing
  | not (testIndex b nDsu) = pure Nothing
  | otherwise = Just <$> unsafeMerge dsu a b

-- Int を返す (境界チェック無し)
mergeMaybe :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Int
mergeMaybe dsu a b = { .. }
```

必然的に `checkIndex` (assertion) は `checkIndex`, `testIndex`, `errorIndex` に分かれます。単純な例ではありますが、こうした構造の整理が盆栽の醍醐味だと思います:

```haskell
{-# INLINE checkIndex #-}
checkIndex :: (HasCallStack) => String -> Int -> Int -> ()
checkIndex funcName i n
  | testIndex i n = ()
  | otherwise = errorIndex funcName i n

{-# INLINE testIndex #-}
testIndex :: (HasCallStack) => Int -> Int -> Bool
testIndex i n = 0 <= i && i < n

{-# INLINE errorIndex #-}
errorIndex :: (HasCallStack) => String -> Int -> Int -> ()
errorIndex funcName i n =
  error $ funcName ++ ": given invalid index `" ++ show i ++ "` over length `" ++ show n ++ "`"
```

> 競プロ縛りだと普通の Haskell プログラミングに飢えてきて、普通のプログラミングを『盆栽』と称している可能性はあります。


## 本当に全域関数を追加するのか

部分関数を 3 つの関数 (部分関数、全域関数、 unsafe 関数) に分けて行くと、ドキュメントが分厚くなるのが嫌です。 `vector` パッケージにおいても、 `exchange` や `modify` などには全域関数がありませんから、用途が少ない関数は省略して良さそうです。

重要な全域関数のみを追加してみます。経験上、区間取得がよくエラーになるため、セグメント木の `prod` 関数などを対象に `*Maybe` シリーズを作成します。他は随時検討します。

それにしても、自分の作ったライブラリが PureScript 的に出来損ないだと思うのは悲しいです。いずれ全域関数縛りのお作法も学ばなければなりませんね。


# [`doctest`](https://github.com/sol/doctest) の実施

統一したエラー文に間違いが無いか確認します。 Haskell には builtin の string interpolation が無く、空白の有無などでミスが起きやすいです。

[doctest](https://github.com/sol/doctest) を書いてメッセージ内容を確認します。

```haskell
-- >>> let !_ = checkIndex "AtCoder.Internal.Assert.doctest" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid index `-1` over length `3`
-- ...
```

実際のエラー文にはスタックトレースが続きますが、 `...` とマッチさせることで [`doctest`](https://github.com/sol/doctest) を通過します ([Matching arbitrary input](https://github.com/sol/doctest?tab=readme-ov-file#matching-arbitrary-output)) 。こうしてエラーケースも [`doctest`](https://github.com/sol/doctest) できました。


## [`doctest`](https://github.com/sol/doctest) の実行方法


### 1. REPL からの実行

プロジェクト全体の [`doctest`](https://github.com/sol/doctest) は REPL から実施するのが無難です。

```sh
$ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault'
```

> [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) 付きの `doctest` ([`prop>`](https://github.com/sol/doctest?tab=readme-ov-file#quickcheck-properties)) を使う場合は、依存指定が必要かもしれません。


### 2. `stack test` からの実行

`stack test` からも [`doctest`](https://github.com/sol/doctest) を実施できたはずです。

```haskell
main :: IO ()
main = do
  -- Run `doctest` over all the source files:
  doctest ["-isrc", "src/"]
```

`cabal test` からの実行は、色々大変そうで諦めました ([#19](https://github.com/toyboot4e/ac-library-hs/issues/19)) 。どうも `Setup.hs` が必要な雰囲気です。雰囲気とか言って流しても良いのがブログ……！


### 3. HLS による実行

[Haskeller の異常な愛情](https://zenn.dev/jij_inc/articles/2024-12-18-pure-haskeller-writing-rust) を読んで知ったことですが、 [HLS の eval plugin](https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin) によって [`doctest`](https://github.com/sol/doctest) を評価できます。たとえば次の `doctest` を作成します。

```haskell
-- | My function.
--
-- >>> let g = (* 3)
-- >>> 1
--
-- >>> g $ f 1
f :: Int -> Int
f = (* 2)
```

僕のエディタ上では以下のように code lens が表示します。現状、 [eval plugin の code action が無い](https://github.com/haskell/haskell-language-server/issues/496) そうなので、 code lens を有効化しましょう:

![Emacs](/images/kyopro-bonsai-hs/hls-eval-1.png =269x)
*Evaluate が表示しました。僕は code lens を無効化していて気づきませんでした*

Evaluate をクリックすると、次のように `doctest` の評価結果が表示します。

```haskell
-- | My function.
--
-- >>> let g = (* 3)
-- >>> 1
-- 1
--
-- >>> g $ f 1
-- 6
f :: Int -> Int
f = (* 2)
```

初回の `doctest` は HLS の eval plugin で結果を生成し、以降は `cabal repl` から実施すれば良いと思います。


## 備考: `.cabal/bin/` の自動切り替え

`cabal install` したコマンドは、デフォルトだと `~/.cabal/bin` 以下に保存されます。おそらく GHC のバージョン切り替えが考慮されていません。特に `doctest` は GHC のバージョンごとにビルドする必要があるため、 GHC と共に自動で切り替わってほしいと思います。

[`ghcup`](https://github.com/haskell/ghcup-hs) ユーザは、 GHC のバージョン毎に `bin` フォルダが自動で切り替わると思います (未確認) 。たぶん問題ありません。

[Nix](https://nixos.org/) では `cabal install` を手動実行する必要は無く、 `devShell` でバージョン指定すれば [`nix-direnv`](https://github.com/nix-community/nix-direnv) により自動で `PATH` に入ります。恐ろしく手間のかかったエコシステムです。

```nix
(haskell.compiler.ghc983.override { useLLVM = true; })
(haskell-language-server.override { supportedGhcVersions = [ "983" ]; })
haskell.packages.ghc983.cabal-fmt
haskell.packages.ghc983.cabal-plan
haskell.packages.ghc983.doctest
haskell.packages.ghc983.implicit-hie
```

これで GHC と共にコマンドのバージョンが切り替わるようになりました。入れたコマンドは以下の 4 つです:

1. [cabal-fmt](https://github.com/phadej/cabal-fmt)
  `cabal-fmt -i ac-library-hs.cabal` のように `.cabal` を整形できます。
2. [cabal-plan](https://github.com/haskell-hvr/cabal-plan)
  `cabal-plan info` で今使っているパッケージのバージョン確認などができます。
3. [`doctest`](https://github.com/sol/doctest)
  `cabal repl --with-ghc=doctest` で `doctest` を実施できます。
4. [`implicit-hie`](https://github.com/Avi-D-coder/implicit-hie)
  `gen-hie > hie.yaml` を実行して初めて HLS が動作する場合があります。


# Goldten test (snapshots test)

Rust の [insta](https://docs.rs/insta/latest/insta/) クレートは、テストプログラムの標準出力をファイル保存しておき、再実行時に diff が見れるライブラリです。 Snapshots test を名乗っています。自作言語の作成時には、構文木の pretty printer などと組み合わせて重宝するようです。

Haskell 界隈で snapshots test は [golden test](https://ro-che.info/articles/2017-12-04-golden-tests) として知られています。今回の `checkIndex` 関数のテストは [`doctest`](https://github.com/sol/doctest) で十分だったため試していません ([#17](https://github.com/toyboot4e/ac-library-hs/issues/17)) が、利用機会が楽しみです。


# おまけ: [すべての警告を表示する](https://github.com/haskell/cabal/issues/1179) には

以下のコマンドが (比較的) 速いようです。

```sh
$ cabal build --ghc-options="-fforce-recomp -fno-code"
```


# まとめ

添字外チェックの assert 文を共通化し、エラーメッセージの `doctest` を実施しました。 HLS の eval plugin は code lens として表示されるので、これを使ってどんどんテストして行きましょう。インクリメンタルな開発にも貢献します。
