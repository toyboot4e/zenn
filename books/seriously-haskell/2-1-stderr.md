---
title: "[2-1] stderr にデバッグ出力"
---

複雑な問題を解く際には、プログラムの計算過程を目視したい場合が増えます。

この時 `print` を使うと、標準出力にデバッグ文が混じってしまい、確認が難しくなります。さらにデバッグ対象の関数に `IO` モナドが必要となり、コード編集も大変になります。

そこで [`Debug.Trace`] の関数を使用すると、標準エラー出力にデバッグ文を出力します。しかも通常の式として使用できるため、 `IO` モナドの無い文脈でも使用できます。

![stderr をターミナルに繋ぐ](/images/seriously-haskell/stderr.png)
*stderr をデバッグ出力に使おう*

以降では [`Debug.Trace`] モジュールの関数を使って print デバッグする方法と、 `#ifdef` により AtCoder 環境ではデバッグ出力を off にする方法を紹介します。

# [`Debug.Trace`]

## 1. [`traceShow :: Show a => a -> b -> b `][`traceShow`]

[`traceShow`] のシグネチャは不思議です。第 1 引数が `stderr` に表示され、第 2 引数はそのまま返ってきます:

```hs
ghci> let !x = traceShow 123 ()
123
ghci> x
()
```

これは `traceShow` が通常の式として表現されるためであり、 `IO` モナドの無い文脈でも使用できるように作られています。

Haskell の関数の例にもれず、 `traceShow` も遅延評価されます。 `traceShow` の計算結果を評価するまで、 stderr への出力はありません:

```hs
ghci> let y = traceShow 123 ()
ghci> y
123
()
```

[『Haskellのassertを文っぽく使う』](https://qiita.com/mod_poppo/items/b3b415ea72eee210d222) という記事では、このように副作用を持つ式を逐次評価するために、 `BangPatterns` (`!`) を使用する方法が示されています。同様に、ここでは `traceShow` を文のように使う例を紹介します。

> ここで Haskell の遅延評価・正格評価が初耳である方は『[Haskellで戦う競技プログラミング](https://booth.pm/ja/items/1577541)』などをご参照ください。

### 1-1. `let` の中で使う

`let` 式の中で `traceShow` を使用する場合は、 `!` を付けると確実に評価されます:

```hs:TraceShow.hs (1/3)
import Debug.Trace

-- | 例 1. @let@ 式で @traceShow@ を使用する
traceShowInLet :: Int -> Int
traceShowInLet x =
  let !_ = traceShow x ()
   in x + 1
```

### 1-2. `where` 句の中で使う

`where` 句の中に `traceShow` を書いた場合も、 `!` を付ければ確実に評価されます:

```hs:TraceShow.hs (2/3)
-- | 例 2. @where@ 句で @traceShow@ を使用する
traceShowInWhere :: Int -> Int
traceShowInWhere x = x + 2
  where
    !_ = traceShow x ()
```

### 1-3. `do` の中で使う

以上の関数を使用してみます:

```hs:TraceShow.hs (3/3)
main :: IO ()
main = do
  let !x1 = traceShowInLet 1
  let !x2 = traceShowInWhere 2
  let !_ = trace "print" ()
  print $ x1 + x2
```

実行結果は次のとおりです ([playground](https://play.haskell.org/saved/XDNGZPdl)):

```sh
$ # stdout (1) のみを表示した場合:
$ runghc TraceShow.hs 2>/dev/null
6
$ # stderr (2) のみを表示した場合:
$ runghc TraceShow.hs
$ trace.hs 1>/dev/null
1
2
$ # stdout (1), stderr (2) を両方表示した場合:
$ runghc TraceShow.hs
1
2
6
```

特に [`online-judge-tools`][`oj`] でテスト実行した場合、ローカルジャッジへの出力を変更せずに、標準エラー出力をデバッグに利用できます:

![筆者のターミナル画像](/images/seriously-haskell/oj-with-stderr.png)
*online-judge-tools でテストを実行する様子*

なおここで `main` 関数中の `!` を外すと、出力の順番が変わってしまうのではないでしょうか ([playground](https://play.haskell.org/saved/jcnth5RV)) 。出力の順番を保つという意味でも、デバッグ出力を伴った式は正格評価するべきです。

## 2. [`traceShowId :: Show a => a -> a`][`traceShowId`]

[`traceShowId`] は [`traceShow`] よりも素直なシグネチャを持っています。これも便利です ([playground](https://play.haskell.org/saved/GtMOfdjx)):

```hs:TraceShowId.hs
import Debug.Trace
import Data.List qualified as L

-- | 例 1. 計算結果に @traceShow@ を適用する
traceShowIdExample1 :: Int -> Int
traceShowIdExample1 x = traceShowId $ x + 1

-- | 例 2. 畳み込みの計算過程を表示する
traceShowIdExample2 :: [Int] -> Int
traceShowIdExample2 xs = L.foldl' step 0 xs
  where
    step !acc x = traceShowId $ acc + x

main :: IO ()
main = do
  let !_ = trace "example 1:" ()
  let !x1 = traceShowIdExample1 1

  let !_ = trace "example 2:" ()
  let !x2 = traceShowIdExample2 [1, 2, 3]

  let !_ = trace "print:" ()
  print $ x1 + x2
```

```sh
$ runghc TraceShowId.hs
example 1:
2
example 2:
1
3
6
print:
8
```

# `traceShow` をさらに便利にする

デバッグ出力は、時計と同じくらい気軽に利用できる存在であるべきです。そのため `traceShow` をラップした `dbg` 関数を定義してみます。

## 1. タイプ数を減らす

`traceShow` 関数のエイリアスとして `dbg` 関数を作成します。 `dbg` 関数を文のように使う想定のため、 `traceShow` の第 2 引数を `()` で固定します:

```hs
import Debug.Trace

dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())
```

`dbg` 関数の使い方は次の通りです:

```hs
main :: IO ()
main = do
  xs <- ints
  let !_ = dbg xs
  print "TODO"
```

## 2. AtCoder 環境では空の関数にする

AtCoder サーバー上でのテストでは、巨大な入力が与えられる問題も多いです。これを標準エラー出力に流してしまうと、まず間違いなく TLE を起こします。

そこで `CPP` 言語拡張を使用し、 `#ifdef`, `#ifndef` を使った条件コンパイルを実施するのがおすすめです。特に AtCoder のジャッジ環境では `ATCODER` マクロが定義されるため、ローカル環境と AtCoder 環境を区別して実装を切り替えできます:

```hs
{-# LANGUAGE CPP #-}

import Debug.Trace

#ifndef ATCODER
-- ローカルではデバッグ出力を実施する
dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

#elif
-- AtCoder 環境では空の関数にする
dbg :: (Show a) => a -> ()
dbg = const ()

#endif
```

### 注. `#ifdef` は code action で消える

HLS の code action を実行すると、 `#ifdef` が消えて無くなるという現象があります。 `#ifdef` を使用する場合は、 code action を実行しない運用になります。変数のリネームなどは問題無く使用できます。

## おまけ: その他のデバッグ出力関数

`dbg` の他、デバッグ用の関数を [playground](https://play.haskell.org/saved/pefpeuV1) にまとめました。以下のように使用できます。特にグリッドの表示のようなデバッグの関数は、早めに作っておくと活躍の機会が多いと思います:

```hs
main :: IO ()
main = do
  -- [2,3,4]
  let !xs = dbgId $ map (+ 1) [1 .. 3]

  -- ys: [2,4,6]
  let !ys = note "ys" $ map (* 2) [1 .. 3]

  let bnd = ((0, 0), (2, 4))
  let arr = listArray @UArray bnd [0 :: Int .. rangeSize bnd - 1]

  --     0     1     2
  --     3     4     5
  --     6     7     8
  --     9    10    11
  --    12    13    14
  let !_ = dbgGrid arr

  return ()
```

# 以上

実践的なデバッグ出力についてお伝えしました。 [`Debug.Trace`] モジュールの関数は純粋な式のような見た目をしており、 `IO` モナドの無い文脈でも使用できます。 BangPatterns を用いて正格評価すれば、文のような使い心地となりました。

`#ifndef` で分岐したことにより、 TLE の心配なくデバッグ出力できるようになりました。 `Trace` モジュールの関数が式であることを活かすと、計算過程に `traceShowId` を挟み込むなど、 `print` 文以上に便利に活用できることもあるでしょう。

今はまだデバッグ出力の必要性を感じていないかもしれませんが、いずれ余裕ができたときに試してみてください。今よりも速く正確に回答できるよう時もあるかもしれません。

## 備考. その他のデバッグ手法

[Haskellでのデバッグ手法｜Haskellの森](https://zenn.dev/mod_poppo/books/haskell-forest/viewer/debug) では、 [`HasCallStack` 制約](https://downloads.haskell.org/ghc/9.4.5/docs/users_guide/exts/callstack.html) や [`ghci` のデバッガ](https://downloads.haskell.org/ghc/9.4.5/docs/users_guide/ghci.html?highlight=debugger#the-ghci-debugger) について記載があります。 [GHC のユーザーガイド](https://downloads.haskell.org/ghc/9.4.5/docs/users_guide/index.html) なども余裕があれば目を通しておくと役に立つかもしれません。

[`Debug.Trace`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Debug-Trace.html
[`traceShow`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Debug-Trace.html#v:traceShow

[`oj`]: https://github.com/online-judge-tools/oj
[`traceShowId`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Debug-Trace.html#v:traceShowId

