---
title: "[2-3] 2. IArray の使い方"
---

[`IArray`] の主な API は [`!` 演算子][`!`] および [`accumArray`] です。それぞれ多次元配列への 1 点アクセスと多次元畳み込みの関数です。配列全体をイテレートする際にはリストに変換します。配列の一部をイテレートする際には、まず添字を作り、添字を配列の要素に写します。

# [`IArray`] への 1 点アクセス

型クラス [`Ix`] が分かっていれば、 [`IArray`] を半分理解したようなものです。いきなりですが、典型 90 問の [004 - Cross Sum（★2）](https://atcoder.jp/contests/typical90/tasks/typical90_d) を解いてみます。

> AI に聞けば十分に実装が見れますが、以下では考え方を重視して解説します。

## Import

以降では、以下の `import` を使用します:

```hs
import Data.Ix
import Data.Array.IArray
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
```

[`IArray`] を [`UArray`] および [`Array`] 共通の API とします。 [`Data.Array`] および [`Data.UArray`] からは配列のデータ型のみを import します。

- [`Data.UArray`] からは関数のエクスポートがありません。 [`UArray`] にアクセスするときは、必然的に型クラス [`IArray`] の関数を使用します。
- [`Data.Array`] からは関数のエクスポートがありますが、 [`IArray`] の関数名と衝突します。したがって [`IArray`] の関数を使います ([`UArray`] と使い方を統一するという意味もあります) 。

## 行列のパース

[問題](https://atcoder.jp/contests/typical90/tasks/typical90_d) における入力例 2 を例とします:

```hs:入力
4 4
3 1 4 1
5 9 2 6
5 3 5 8
9 7 9 3
```

`getMatInt` 関数は行列を 1 次元リストとして読み出した上で、 [`listArray`] で配列に変換します:

```hs:Main.hs (import は省略しています)
ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints
```

生成された配列を元に、 `bounds`, `elems`, `assocs` といった [`IArray`] の関数を使って配列の中身を表示してみます:

```hs:Main.hs
main :: IO ()
main = do
  [h, w] <- ints
  mat <- getMatInt h w

  -- 配列の添字範囲
  print $ bounds mat

  -- 配列中の値の一覧 (@listArray@ への引数を表示するとも言えます)
  print $ elems mat

  -- 配列中の (添字, 値) の一覧
  print $ assocs mat

  -- show <array> は bounds および assocs の組み合わせを表示する
  print mat
```

```hs:出力
((0,0),(3,3))
[3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3]
[((0,0),3),((0,1),1),((0,2),4),((0,3),1),((1,0),5),((1,1),9),((1,2),2),((1,3),6),((2,0),5),((2,1),3),((2,2),5),((2,3),8),((3,0),9),((3,1),7),((3,2),9),((3,3),3)]
array ((0,0),(3,3)) [((0,0),3),((0,1),1),((0,2),4),((0,3),1),((1,0),5),((1,1),9),((1,2),2),((1,3),6),((2,0),5),((2,1),3),((2,2),5),((2,3),8),((3,0),9),((3,1),7),((3,2),9),((3,3),3)]
```

Row-major 行列を作成したことが確認できました。 `elems` の出力が見やすいと思うので、以降では `elems` で配列の中身を確認することにします。

## 解く

後は行・列ごとに累積和を用意して解くことができます。解答例にリンクしておきます。

- リスト内包表記を使って走査しました。 `map` を使っても良いと思います。
- [`listArray`] は型クラス [`IArray`] の関数であるため、型推論のため `@UArray` を書きました。

https://atcoder.jp/contests/typical90/submissions/48521367

# [`accumArray`]

最後に、 [`accumArray`] が強力な関数です。これが使えると [関数プログラミング 珠玉のアルゴリズムデザイン](https://shop.ohmsha.co.jp/shopdetail/000000004066/) の第一章が読める他、コンテストにおいても序盤から終盤まで大活躍するでしょう。

## [`accumArray`] のメンタルモデル

[`accumArray`] とは、データスロットを複数持った畳み込みです。引数は、演算子、初期値、添字範囲、入力です。これを 演算子、初期配列 (各スロットの初期値および添字範囲) 、入力 と見れば、 [`foldl'`] と共通したシグネチャを持つ関数であると言えます。

そのため僕が引数に名前を付けるなら、次のようになります:

```
accumArray  op   e0 bnd input
foldl'      step s0     input
            ~~~~ ~~~~~~ ~~~~~
             |     |      |
             |     |      +-- 入力
             |     +-- 初期値
             +-- 演算子
```

> なお [`vector`] パッケージにおける `accumArray` とは [`accumulate`] 関数ですが、 [`foldl'`] と完全に同じシグネチャを持っています。やはり後発のパッケージのため API が良いのだと思います。お楽しみに！
> ```
> accumulate op vec0 input
> ```

## [`accumArray`] の使い方

まずはコンパイル可能なコード例を作成します:

```hs
ghci> import Data.Array.IArray
ghci> import Data.Array.Unboxed (UArray)
ghci> accumArray @UArray (+) (0 :: Int) (0, 3) []
array (0,3) [(0,0),(1,0),(2,0),(3,0)]
```

`elems` で配列中の値を抽出すれば、以下の通りです:

```hs
ghci> elems $ accumArray @UArray (+) (0 :: Int) (0, 3) []
[0,0,0,0]
```

入力は空の配列 `[]` にしましたから、初期配列の作成結果が確認できました。

入力は `(index, value)` ペアの列です。 `index` が畳み込み先のスロットを指定し、 `value` が畳み込み演算子への第二項となります。

```hs
ghci> elems $ accumArray @UArray (+) (0 :: Int) (0, 3) [(0, 1)]
[1,0,0,0]
ghci> elems $ accumArray @UArray (+) (0 :: Int) (0, 3) [(0, 1), (3, 2)]
[1,0,0,2]
ghci> elems $ accumArray @UArray (+) (0 :: Int) (0, 3) [(0, 1), (3, 2), (0, 3)]
[4,0,0,2]
```

なお演算子のシグネチャは `配列中の値 -> 入力 -> 配列中の値` です。非可換な演算子を使用する場合は注意しましょう:

```hs
ghci> elems $ accumArray @UArray (-) (0 :: Int) (0, 3) [(0, 1), (3, 2), (0, 3)]
[-4,0,0,-2]
ghci> elems $ accumArray @UArray (flip (-)) (0 :: Int) (0, 3) [(0, 1), (3, 1), (0, 2)]
[4,0,0,2]
```

## [`accumArray`] の内部実装

[`accumArray`] が多次元畳み込みであると表現するならば、その実装は `foldl'` の wrapper なのでしょうか？　もちろん配列全体をコピーすると計算量が膨れ上がりますから、 [`accumArray`] の内部では可変配列が使用されています。

[`accumArray`] の内部実装をたどっていくと、 [`GHC.Arr`] の [`unsafeAccumArray'`] が見つかります。 [`unsafeAccumArray'`] は `adjust'` を呼び、 `adjust'` は [prim-ops] の `writeArray#` を呼び出しています。これは GHC に組み込まれた真の可変操作です。

内部実装を聞くと、結局手続き的プログラミングと変わり無いのではないかと落胆しませんか。 [4-1] RealWorld の章では、 Haskell には可変配列など存在しないという解釈を紹介します。

# Tips

## `Array` の使いどころ

上記までは [`UArray`] を使用しましたが、リストのような boxed なデータ型 [^1] を使用する場合は [`Array`] を指定する必要があります:

```hs
ghci> elems $ accumArray @Array (flip (:)) [] (0, 3) [(0, 1), (3, 2), (0, 3)]
[[3,1],[],[],[2]]
```

> 一応 `(:)` 関数の型を確認すると:
>
> ```hs
> ghci> ghci> :t (:)
> (:) :: a -> [a] -> [a]
> ghci> ghci> :t flip (:)
> flip (:) :: [a] -> a -> [a]
> ```

もしも `UArray` を指定した場合は、次のようなエラーが出ます:

```hs
ghci> elems $ accumArray @UArray (flip (:)) [] (0, 3) [(0, 1), (3, 2), (0, 3)]

<interactive>:28:1: error:
    • No instance for (IArray UArray [Integer])
        arising from a use of ‘it’
    • In the first argument of ‘print’, namely ‘it’
      In a stmt of an interactive GHCi command: print it
```

`IArray UArray [Integer]` の実装は無いとのことです。確かにその通りですが、当然 `Array` を指定したつもりで `UArray` を指定していたり、たまにやってしまうエラーです。

## 配列を引数に取る関数を作るには

[`UArray`], [`Array`] を問わず引数に取る関数を作りたいとします。 [`IArray`] の出番です。

例として、リストを元に 1 次元累積和 (cummulative sum) を作る関数を作ってみましょう。添字型 `i` は `Int` で固定しました:

```hs
-- | 1 次元の累積和配列を作成する。
{-# INLINE csum1D #-}
csum1D :: (IArray a e, Num e) => Int -> [e] -> a Int e
csum1D n = listArray (0, n - 1) . L.scanl' (+) 0
```

ついでに累積和へのアクセサも定義してみます:

```hs
-- | 1 次元の累積和配列を元に区間和を求める。
-- >>> csum1D [1, 2, 3, 4] +! (1, 2)
-- 5
{-# INLINE (+!) #-}
(+!) :: (IArray a e, Num e) => a Int e -> (Int, Int) -> e
(+!) arr (!l, !r) = arr ! succ r - arr ! l
```

使ってみると、以下のように `@UArray` を指定する必要があります ([playground](https://play.haskell.org/saved/0c8OUR1C)):

```hs
main :: IO ()
main = do
  let csum = csum1D @UArray 4 [1 :: Int, 2, 3, 4]
  print $ csum +! (1, 2)
```

```txt:出力
5
```

この例ならば、むしろ配列を抽象化せず、常に `UArray` を引数に取った方が良いかもしれません。

実は型制約において `IArray UArray e` のように具体的な型を型パラメータを当てはめることができます。 `IArray UArray e` の形で制約を書けば、返値は常に `UArray` となり、 `@UArray` の記述を省くことができます ([playground](https://play.haskell.org/saved/bsEpHMy8)):

```hs
main :: IO ()
main = do
  let csum = csum1D 4 [1 :: Int, 2, 3, 4]
  print $ csum +! (1, 2)
```

## ユーザー定義型を array に保存するには？

よく配列に入れたくなるデータ型としては [`ModInt`](https://atcoder.github.io/ac-library/production/document_ja/modint.html) があります。 `ModInt` のようなユーザー定義型を配列に保存するためには、 `IArray` や `MArray` といった型クラスを実装する必要があるようです。僕は `array` から逃げて `vector` を使っているため助言できませんが、実装方法を探してみてください。もしくは、都度手動で mod 計算を挿入すれば良いでしょう。

# まとめ

[`Data.Ix`] および [`IArray`] の使い方を確認しました。 [`UArray`] と [`Array`] の使い分けにも軽く言及しました。 API ドキュメントこそ短いものの、 array は重めのモジュールです。その他ハマり所などあればコメントください。

[`array`]: https://www.stackage.org/lts-21.7/package/array-0.5.4.0
[`IArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html
[`UArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-Unboxed.html#t:UArray
[`Data.Array`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-Array.html
[`Data.UArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-UArray.html
[`Data.Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html
[`Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#t:Ix

[`error`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/GHC-Err.html#v:error
[`!`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html#v:-33-
[`accumArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html#v:accumArray
[`listArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html#v:listArray
[`index`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#v:index
[`inRange`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#v:inRange
[`rangeSize`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#v:rangeSize
[HasCallStack]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/callstack.html

[`unsafeAccumArray'`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/GHC-Arr.html#v:unsafeAccumArray-39-
[`newArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-MArray.html#v:newArray

[`foldl'`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:foldl-39-

[`GHC.Arr`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/GHC-Arr.html
[`vector`]: https://www.stackage.org/lts-21.7/package/vector-0.13.0.0
[`accumulate`]: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Generic.html#v:accumulate
[prim-ops]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/prim-ops

[^1]: 正確には、 `IArray UArray e` が実装されないデータ型 `e` がタプルやリストです。 `vector` パッケージの場合は、配列とは独立してデータ型に対して `Unbox` を実装しますから、 `IArray` はちょっとヘンテコな型クラスだと思います。
[^2]: 計算結果が非常に大きくなる問題などでは、オーバーフローの防止のためか、素数 `998244353` を法とした答えが問われる場合が多いです。
[^3]: フェルマーの小定理については [Haskellで戦う競技プログラミング 第2版](https://booth.pm/ja/items/1577541) などをご覧ください。 `ModInt` の実装例、すなわち型クラス `Num` の実装例は AI に聞いてみてください。高度に高速化された実装としては [cojna/iota] の [IntMod] をご覧ください。

[cojna/iota]: https://github.com/cojna/iota
[IntMod]: https://github.com/cojna/iota/blob/master/src/Data/IntMod.hs

