---
title: "[3] 1. 入出力の典型をライブラリ化する"
---

まずは入出力でウォーミングアップしていきます。入出力も、ある種の典型問題であると言えるでしょう。手持ちの道具で対応できるよう整備していきます。

# 出力

出力は入力よりも簡単なため、出力のパターンから押さえていきます。

出力は短い場合が多く、低速な `String` 型を使って問題ありません。

| 出力          | 使用関数             |
|---------------|----------------------|
| 1. Yes / No   | `yn`                 |
| 2. 空白区切り | `unwords`            |
| 3. 改行区切り | `unlines` or `forM_` |

## 1. Yes / No

Yes / No を出力する問題は多いです。ライブラリ化しておくと手間なく書けて良いと思います:

```hs
import Data.Bool (bool)

yn :: Bool -> String
yn = bool "No" "Yes"

printYn :: Bool -> IO ()
printYn = putStrLn . yn
```

> [`bool :: a -> a -> Bool -> a`][`bool`] を使うと `yn` をポイントフリースタイルで書くことができます。ポイントフリースタイルができると……たぶん自己満足できます。

## 2. 空白区切りの出力

文字列リストに [`unwords`] を適用すると、空白区切りの文字列になります:

```hs
ghci> putStrLn $ unwords . map show $ [1, 2, 3]
1 2 3
```

[`intercalate`] を使うことでも、文字列の間に空白文字を挿入できます。他言語の `join` のようなものでしょう:

```hs
ghci> import Data.List qualified as L
ghci> putStrLn $ L.intercalate " " . map show $ [1, 2, 3]
1 2 3
```

`vector`, `array` を空白区切りで出力する際は、一度リストに変換すれば良いと思います:

```hs
ghci> import Data.Array
ghci> putStrLn $ unwords . map show . elems $ listArray (1, 3) [1 .. 3]
1 2 3
```

## 3. 改行区切りの出力

文字列のリストに [`unlines`] を適用すると、改行区切りの文字列にできます。ただし `unlines` の出力は末尾に改行を含むため、 [`putStrLn`] ではなく [`putStr`] を使って表示しましょう:

```hs
ghci> putStr $ unlines . map show $ [1, 2, 3]
1
2
3
```

あるいは [`forM_`] / [`for_`] か [`mapM_`] / [`traverse_`] で n 回 `print` しても良いです。短く便利です:

```hs
ghci> import Control.Monad
ghci> forM_ [1, 2, 3] print
1
2
3
```

なお AtCoder のジャッジは寛容であるため、出力の末尾に改行文字が含まれていても受理されます。

# 入力

入力は巨大な場合が多く、高速な [`ByteString`] の使用をお勧めします。

ここでは [`bytestring`] パッケージの [`getLine`] 関数をベースに、行単位で入力を処理します。複数行をひとまとめに読み込む際は、 `forM` や `replicateM` で行の読み込みを繰り返します。

出力するデータ型としては `[Int]` や `UArray` を使います。 `vector` を使う場合も、やり方は大体同じです。

| 入力          | 行数 | データ型                    | 作成関数名              |
|---------------|------|-----------------------------|-------------------------|
| 1. 整数列     | 1    | `[Int]`                     | `ints`                  |
| 2. 2 個の整数 | 1    | `(Int, Int)`                | `ints2` (, `ints3`, ..) |
| 3. 巨大整数   | 1    | `[Int]`                     | `digitsL`               |
| 4. グリッド   | `h`  | `UArray (Int, Int) Char`    | `getGrid`               |
| 5. 行列       | `h`  | `UArray (Int, Int) Int`     | `getMatInt`             |
| 6. 異種混合   | 1    | `(Int, String, [Int])` など | `auto`                  |
| 7. 対称行列   | `h`  | `UArray (Int, Int) Int`     | `getDiagMat`            |

## 1. 整数列

任意の数の整数列は、いつもの `ints` で読み取れます:

```txt:入力例
A_1 A_2 .. A_N
```

```hs
ints :: IO [Int]
ints = map read . words <$> getLine
```

## 2. 2 個の整数

2 個の整数をタプルとしてパースします:

```txt:入力例 (2 個の整数)
N M
```

```hs
{-# LANGUAGE LambdaCase #-}

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"
```

`ints2` はタプルを返すため、グラフの構築と相性が良いです。たとえば [典型 90 問 078 - Easy Graph Problem（★2）](https://atcoder.jp/contests/typical90/tasks/typical90_bz) で与えられるグラフを読み出してみます:

[`replicateM`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Control-Monad.html#v:replicateM

```txt:入力の形式
N M
a1​ b1​
⋮
aM​ bM​
```

```hs:Main.hs
swapDupe :: (Int, Int) -> [(Int, Int)]
swapDupe (!v1, !v2) = [(v1, v2), (v2, v1)]

main :: IO ()
main = do
  (!nVerts, !nEdges) <- ints2
  edges <- concatMap swapDupe <$> replicateM nEdges ints2
  let gr = accumArray (flip (:)) [] (1, n) edges
  return ()
```

[`Data.Tuple.Extra`] の `both` 関数と組み合わせると、 `(both pred <$> ints2)` のように 1-based index を 0-based index に変換することもできます。 [`Data.Bifunctor`] の `first` / `second` を使うとタプルの左側／右側のみの値を更新できます。

## 3. 巨大整数

`Int` に収まらないような巨大な整数が与えられた場合は `[Int]` 列として保存します:

```hs
digitsL :: IO [Int]
digitsL = L.unfoldr (fmap (first digitToInt) . BS.uncons) <$> BS.getLine
```

## 4. グリッド

`.` と `#` のグリッドなど、 HxW 文字のグリッドを読み込む場合は `getLine` を H 回繰り返します:

```txt:入力例
H W
a1,1​ … a1,W​
:
aH,1​ … aH,W​
```

```txt:サンプル
3 4
...#
.#..
....
```

```hs
getGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
getGrid h w = listArray ((0, 0), (h - 1, w - 1)) . concatMap BS.unpack <$> replicateM h BS.getLine
```

`Char` から `Int` に変換するには [`amap`] を使うと良いでしょう:

```hs
main :: IO ()
main = do
  (!h, !w) <- ints2
  grid <- amap (bool (0 :: Int) 1 . (== '.')) <$> getGrid h w
  print $ elems grid
```

```txt:出力
[1,1,0,1,1,1,1,0,1,1]
```

## 5. 行列

HxW 行列の各行が空白区切りで与えられる場合は、 `ints` を `h` 回繰り返します:

```txt:入力例
H W
a1,1​ … a1,W​
:
aH,1​ … aH,W​
```

```txt:サンプル
3 4
1 2 3 4
5 6 7 8
9 10 11 12
```

```hs
getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints
```

## 6. 複数のデータ型が混じった行

稀に、 1 行に異なるデータ型が複数混じっている場合があります。都度手動でパースすれば良いと思います:

```hs
-- @n@ 行を @[(Int, String, [Int])]@ として読み取る:
queries <- replicateM n $ do
  xs <- ints
  return (read @Int (xs !! 0), xs !! 1, map (read @Int) (drop 2 xs))
```

あるいは `Readable` のような型クラスを作れば、任意の `Readble` 型のタプルを `Readble` とすることで、任意のタプルへとパースできます:

```hs
queries <- replicateM n (auto @(Int, String, [Int]))
```

僕の時間の都合で省略します……

## 7. 対称行列

稀に、対角成分が未定義の対称行列を扱う際は、次のような入力が与えられます:

```txt:入力例
N
A_12 A_12 .. A_1N
A_23 A_24 .. A_2N
..
A_(N-1)N
```

これも僕の時間の都合で省略します……

# モナドの選択

ここでは割り切って `IO` モナドを使用しましたが、 `IO` モナドを使った関数は単体テストができない点が問題です。こだわるなら、パーサを純粋にすることもできます。

パーサの入力を `State` モナドにする例が、 [Haskellで戦う競技プログラミング 第2版](https://booth.pm/ja/items/1577541) でさらっと触れられていたと思います。 `State` モナドと組み合わせると、パーサはたぶんこんな感じになります:

```hs
lineN n st = unfoldrExactN n (runStateT st) <$> BS.getLine
```

あるいは `stdin` 全体を `ByteString` に保存しておき、 `State` モナドの中で入力処理する形もあります。 [`cojna/iota`] の `runSolver` と `PrimParser` がまさにそれです。美しいと思います。

`stdin` 全体を `ByteString` に読み込んでおくと、改行区切りに囚われず、入力をデータストリームとみなすことができます。 C++ で `cin` を使うのと同じ感覚で入力データを読み取っていけるのではないでしょうか。

さらに [`cojna/iota`] の `runSolver` は出力も `ByteString` 型としており、解答プログラムは `IO` モナドに依存しません。最後に 1 度だけ `IO` モナドを使って print するようです。出力を含めて純粋な関数となるのは興味深いですし、都度 `stdout` に書き込むよりも効率が良いのではないかと思います。

# まとめ

AtCoder 典型の入出力を Haskell で書く方法を確認しました。意外と工夫の余地があるため、事前に入出力の方法を練っておくとコンテストでも活躍してくれると思います。モナドを上手く使えば、より美しいコードにもできそうです。

[`bool`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Bool.html#v:bool
[`unwords`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:unwords
[`intercalate`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:intercalate

[`unlines`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:unlines
[`putStrLn`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:putStrLn
[`putStr`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:putStr

[`forM_`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Control-Monad.html#v:forM_
[`for_`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Foldable.html#v:for_
[`mapM_`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Control-Monad.html#v:mapM_
[`traverse_`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Foldable.html#v:traverse_

[`ByteString`]: https://www.stackage.org/haddock/lts-21.7/bytestring-0.11.4.0/Data-ByteString-Char8.html#t:ByteString
[`amap`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html#v:amap

[`bytestring`]: https://www.stackage.org/lts-21.7/package/bytestring-0.11.4.0
[`getLine`]: https://www.stackage.org/haddock/lts-21.7/bytestring-0.11.4.0/Data-ByteString-Char8.html#v:getLine

[`Data.Tuple.Extra`]: https://www.stackage.org/haddock/lts-21.7/extra-1.7.14/Data-Tuple-Extra.html
[`Data.Bifunctor`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Bifunctor.html
[`both`]: https://www.stackage.org/haddock/lts-21.7/extra-1.7.14/Data-Tuple-Extra.html#v:both
[`pred`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:pred

[`cojna/iota`]: https://github.com/cojna/iota

