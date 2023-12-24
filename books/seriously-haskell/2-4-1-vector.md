---
title: "[2-4] 1. Vector の使い方"
---

[`vector`] はストリームと配列の役割を果たす強力なパッケージです。リストの主な用途を代替し、 [`array`] をほぼ完全に置換します。 `ModInt` を始めとしたユーザー定義型を格納できる他、端的に計算を表現できる気の効いた関数も多く、以前よりも余裕を持って解答できるようになるでしょう。

しかし [`vector`] はリストや [`array`] をそっくりそのまま置換するものではなく、リストや [`array`] にも適した用途が残っています。具体的には、列挙やスタックにはリストを、多次元配列には [`array`] を使い、それ以外には [`vector`] を使うのが一般的です。

このような具体的な計算を確認し、 [`vector`] の立ち位置をどのようなものとするか考えていきます。

# [`vector`] によるストリーム処理

[`vector`] はストリームとしてのリストをほぼ完全に置換します。 [`vector`] をストリームとして使った場合、処理が高速になる上に、 `ifilter` や `unfoldrExactN` のような便利な関数も使えるようになります。

しかしリストに慣れていると [`vector`] が使いにくいと感じる場合もあります。代表的な例を確認し、対策を考えてみます。

## パターンマッチ

標準入力をリストとしてパースし、パターンマッチで 1 つずつ処理する場合を考えます:

```hs
solve :: Int -> [Int] -> Int
solve k xs = inner xs
  where
    inner [] = 0
    inner (x : xs) = {- .. -}
```

このような場合はリストを使って問題無いのですが、選択肢を広げるために、あえて `vector` へ置き換える方法を考えてみます。

まず再帰処理を畳み込み (`foldl'`) や展開 (`unfoldr`) で表現できないかを考えてみます。もしも当てはまる関数があれば、きっと [`vector`] パッケージにも同名の関数がありますから、コードの形を保ったまま [`vector`] へ移行できます。

どうしても再帰関数が書きたいという場合は、 `(x : xs)` のようなパターンマッチを `uncons` に置き換えることも可能です。上記のパターンマッチを `uncons` を使って書き表すと、以下の通りです。 [`vector`] においても同名の関数があり、同様に書けます:

```hs
solve :: Int -> [Int] -> Int
solve k xs = inner xs
  where
    inner xs = case uncons xs of
      Nothing -> 0
      Just (x, xs') -> {- .. -}
```

このように、意外とリストの API を知らないからこそ、 [`vector`] への移行が大変に見える可能性があります。 [`vector`] を使うときにこそ、 [`Prelude`] や [`Data.List`] を眺めてみるのも良いと思います。

## ソート

[`vector`] のソート関数は [`vector-algorithms`] パッケージに分かれています。 [`vector-algorithms`] の関数は [`MVector`] を引数に取り、ソート済み配列に書き換えます。よって純粋な文脈でソートするためには `modify` 関数と組み合わせて使います:

```hs
ghci> import Data.Vector.Unboxed qualified as U
ghci> import Data.Vector.Algorithms.Intro qualified as VAI
ghci> let xs = U.fromList [1 :: Int, 3, 2, 4]
ghci> U.modify VAI.sort xs
[1,2,3,4]
ghci> U.modify (VAI.sortBy (comparing Down)) xs
[4,3,2,1]
```

この辺りも『Haskell で挑む競技プログラミング』に言及があります:

https://booth.pm/ja/items/1577541

AtCoder 環境の Haskell においては `VAI.sort` が低速であるという情報が共有されています。実際、使ってみると TLE することも多いです。

`VAI.sort` を `(VAI.sortBy compare)` に書き換えると高速になります。以下の書き換え規則 (rewrite rules) によって、 `VAI.sort` を自動的に `VAI.sortBy compare` に置き換えることができると情報共有がありました:

```hs
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
```

https://haskell.jp/slack-log/html/C4M4TT8JJ/104.html#message-1698841366.259449

書き換え規則は、ソースファイル内のアイテムとしては関数と同じレベルの扱いに見えます。ソースファイルの上の方に書くとコンパイルエラーになりますが、以下のような `Main.hs` ならばコンパイル可能です:

```hs:Main.hs
-- GHC のオプション
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}

-- 言語拡張
{-# LANGUAGE CPP #-}

-- import
import Control.DeepSeq

-- 関数および rewrite rules
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

main :: IO ()
main = do {- .. -}
```

## リスト内包表記

複数の `map` 関数をフラットに書けるというモチベーションでリスト内包表記／リストモナドを使っている場合は、 `generate` 関数などで代替できるかもしれません。

行列の各列の和を求める計算を例に取ります:

```hs
[sum [arr ! (y, x) | x <- [0 . .w - 1]] | y <- [0 .. h - 1]]
```

```hs
U.generate w $ \x -> U.sum (U.generate h $ \y -> arr ! (y, x))
```

# `vector` による `array` の置換

[`vector`] はほぼ完全に [`array`] を置換します。 API の違いは改良であり、多次元配列でさえもその気になれば `vector` で書けます。

## 型クラス

`array` においては型クラス [`IArray`] を使用しました。 [`vector`] においては [`Data.Vector.Generic`] モジュールが boxed/unboxed vector 共通の API であり、 [`Data.Vector`] と [`Data.Vector.Unboxed`] は [`Data.Vector.Generic`] の API の型を限定したものとなっています。また型クラス [`Unbox`] が配列の型クラスから分離されています。

## `accumulate`

`accumulate` を多次元畳み込みであると見ると、 `foldl'` とシグネチャの対応が取れ、 `accumArray` よりも分かりやすいと思います。

```
foldl'      step s0     input
accumArray  op   e0 bnd input
accumulate  op   vec0   input
            ~~~~ ~~~~~~ ~~~~~
             |     |      |
             |     |      +-- 入力
             |     +-- 初期値
             +-- 演算子
```

## 多次元配列

`vector` をラップして [`Ix`] クラス越しにアクセスすれば、 `vector` を使いつつ快適な多次元配列の API を提供できます。僕以外にやっている人を見たことがありませんが、 `constructN` との組み合わせなどで活躍するため、アリかと思います。

https://toyboot4e.github.io/toy-lib/Data-Vector-IxVector.html

# `vector` ならではの機能・利点

`vector` ならではの決定的な利点と細かな良さを紹介します。

## ユーザー定義型の unbox 化

`vector` においては型クラスの [`Unbox`] が分かれており、ユーザー定義型を unboxed vector に保存するのが比較的簡単です。たとえば自動的に mod 計算を行う `ModInt` 型を unboxed vector に保存できます。

## 部分列へのアクセス

`vector` には部分列を取る API があり、リストを経由せずイテレートできます。 CSR 形式のグラフの実装などで大活躍します。

## `ifilter`, `unfoldrExactN` などの便利関数

偶数版目の要素や奇数番目の要素を抜き出したり:

```hs
ghci> U.ifilter (const . even) [0, 2, 1, 3]
[0,1]
ghci> U.ifilter (const . odd) [0, 2, 1, 3]
[2,3]
```

長さが分かっている `unfoldr` の実装が簡単になったり:

```hs
ghci> U.unfoldr (\case 0 -> Nothing; x -> Just . swap . (`divMod` 10) $ x) 123
[3,2,1]
ghci> U.unfoldrExactN 3 (swap . (`divMod` 10)) 123
[3,2,1]
```

そういった便利な関数があります。

# `vector` で置換しないもの

最後に `vector` で置換しないものを検討します。

## `fromList`

`containers` パッケージのデータ型は `fromList` で生成できました。 vector から生成する場合は、一旦 `toList` でリストに変換してしまえば良いと思います。

## `HT.groupBy`

`groupBy` は (隣接要素ではなく) グループの先頭要素と後続の要素を比較してグループ分けを行います。

```hs
ghci> U.groupBy (\a b -> abs (a - b) <= 1) [0, 1, 2, 10, 0, 1]
[[0,1],[2],[10],[0,1]]
```

リストにおいては `utility-ht` パッケージの `groupBy` を使うと、隣接要素の比較によってグループ分けを実施できました:

```hs
ghci> groupBy (\a b -> abs (a - b) <= 1) [0, 1, 2, 10, 0, 1]
[[0,1],[2],[10],[0,1]]
ghci> HT.groupBy (\a b -> abs (a - b) <= 1) [0, 1, 2, 10, 0, 1]
[[0,1,2],[10],[0,1]]
```

`vector` において隣接要素の比較によってグループ分けを実施したいなら、やはり一旦 `toList` でリストに変換し、 `HT.groupBy` を使うと良いでしょう。

## 順列・組み合わせ・直積・素数列挙などの列挙

列挙を行う関数は `vector` パッケージでは提供されていません。順列 ([`permutations`]), 直積 ([`sequence`]), 組み合わせ, 素数列挙といった関数は、そのままリスト版を使えば良いと思います。

組み合わせ関数 (`combinations`) のおすすめは:
https://zenn.dev/osushi0x/scraps/51ff0594a1e863#comment-e6b0af9b61c54c

素数列挙 (`primes`) のおすすめは:
https://zenn.dev/link/comments/1022553732563c

# まとめ

大雑把に [`vector`] の API を確認しました。リストを [`vector`] で置換する際には、そもそも [`Data.List`] の API に詳しくなると [`vector`] への移行が簡単になります。 [`array`] から [`vector`] への移行の際にはユーザー定義型を [`Unbox`] にできることが決定的なメリットとなり、他の API も改善されます。リストに依存する強力な関数は、引き続き使用すれば良いでしょう。

[`sortOn`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:sortOn
[`sortBy`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:sortBy
[`comparing`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ord.html#v:comparing
[`on`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ord.html#v:comparing

[`permutations`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:permutations
[`sequence`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:sequence

[`vector`]: https://www.stackage.org/lts-21.7/package/vector-0.13.0.0
[`MVector`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Mutable.html
[`Data.Vector`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector.html
[`Data.Vector.Unboxed`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Unboxed.html
[`Data.Vector.Generic`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Generic.html
[`Unbox`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Unboxed.html#t:Unbox

[`vector-algorithms`]: https://www.stackage.org/lts-21.7/package/vector-algorithms-0.9.0.1

[`array`]: https://www.stackage.org/lts-21.7/package/array-0.5.4.0
[`IArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html

[`Data.Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html
[`Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#t:Ix

[`Prelude`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html
[`Data.List`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html

