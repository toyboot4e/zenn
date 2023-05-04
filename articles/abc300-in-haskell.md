---
title: "ABC 300 解説 (Haskell)"
emoji: "🐙"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: []
published: true
---

# 概要

この記事では [ユニークビジョンプログラミングコンテスト2023 春 (AtCoder Beginner Contest 300)](https://atcoder.jp/contests/abc300/tasks) の A ~ E 問題を Haskell で解説します。

## 感想

ABC 300 は連休前のコンテストでした。 DDoS 攻撃の影響で unrated が続く中、今回はサイトが落ちることもなく、無事レーティングが更新されて良かったです。

## 外部リンク

:::details 得点・時間とパフォーマンスのグラフ (ABC 300)
https://twitter.com/kiri8128/status/1652318155509989377
:::

:::details 解説放送: 未公開
未公開
:::

:::details 【競技プログラミング】ABC300【実況】
https://www.youtube.com/watch?v=qePaukrGrTI
:::

# [A - N-choice question](https://atcoder.jp/contests/abc300/tasks/abc300_a)

## 入力処理

まずは入力をパースします:

```hs:入力処理
main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  xs <- map read . words <$> getLine
```

## 考察・解答

配列 `xs` の中から、値が `a + b` に等しいものの添字を求めます。これは [findIndex](http://zvon.org/other/haskell/Outputlist/findIndex_f.html) を使うのが素直だと思います:

```hs
  let i = succ . fromJust $ findIndex (== (a + b)) xs
  print i
```

:::details 解答全体
[提出](https://atcoder.jp/contests/abc300/submissions/41156812)

```hs:解答全体
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  xs <- map read . words <$> getLine

  let i = succ . fromJust $ findIndex (== (a + b)) xs
  print i
```
:::

# [B - Same Map in the RPG World ](https://atcoder.jp/contests/abc300/tasks/abc300_b)

## 入力処理

まず入力をパースします。行列が出て来るためやや手強いです:

```hs:入力処理
import Control.Monad
import Data.Array.Unboxed

getInts :: IO [Int]
getInts = map read . words <$> getLine

getMat :: Int -> Int -> IO (UArray (Int, Int) Char)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

main :: IO ()
main = do
  [h, w] <- getInts
  gridA <- getMat h w
  gridB <- getMat h w
```

- [replicateM](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad.html#v:replicateM) で `IO` アクションを n 回実行できます。 `getLine` を n 回繰り返すことで n 行分のデータを `[[Char]]` として読み込み、 `concat` で `[Char]` に変換します。

- [listArray](https://hackage.haskell.org/package/array-0.5.5.0/docs/Data-Array-IArray.html#v:listArray) で `UArray` を作ります。 Haskell の array は中々クセがありますが、 [Data.Ix](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Ix.html) を活かした作りになっており、多次元配列の作成に適しています。

## 考察・解答

さて問題文中の『シフト』というのは、ループしたグリッドを上下左右に動かす操作にあたります。このようなグリッドのループや回転は、 [ixmap](http://zvon.org/other/haskell/Outputarray/ixmap_f.html) を使って表現できます。

```hs
-- | 行列中のすべての添字 (y, x) に (dy, dx) を加算する。行列をはみ出た添字はループさせる。
shiftMat :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Char
shiftMat mat (dy, dx) = ixmap (bounds mat) f mat
  where
    f (y, x) = ((y + dy) `mod` h, (x + dx) `mod` w)
    (h, w) = both succ . snd $ bounds mat
```

- `bounds mat` は `((0, 0), (w - 1, h - 1))` を返します。
- [both](https://www.stackage.org/haddock/lts-20.19/extra-1.7.13/Data-Tuple-Extra.html#v:both) は外部パッケージ `extra` の関数です。 `(w - 1, h - 1)` に `both succ` を適用すると、 `(w, h)` が返ります。

またシフト量 `(dy, dx)` はリスト内包表記を使って簡潔に生成できます:

```hs
  let dyxs = [(dy, dx) | dy <- [0 .. h - 1], dx <- [0 .. w - 1]]
```

したがって全探索で回答できました。

:::details 解答全体
[提出](https://atcoder.jp/contests/abc300/submissions/41156812)

```hs
import Control.Monad
import Data.Array.Unboxed
import Data.Tuple.Extra (both)

-- | 1 行読んで `[Int]` を作る。
getInts :: IO [Int]
getInts = map read . words <$> getLine

-- | h 行読んで `UArray (Int, Int) Char` を作る。
getMat :: Int -> Int -> IO (UArray (Int, Int) Char)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

-- | 真なら `Yes`, 偽なら `No` を返す。
yn :: Bool -> String
yn True = "Yes"
yn False = "No"

-- | 行列中のすべての添字 (y, x) に (dy, dx) を加算する。行列をはみ出た添字はループさせる。
shiftMat :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Char
shiftMat mat (dy, dx) = ixmap (bounds mat) f mat
  where
    f (y, x) = ((y + dy) `mod` h, (x + dx) `mod` w)
    (h, w) = both succ . snd $ bounds mat

main :: IO ()
main = do
  [h, w] <- getInts
  matA <- getMat h w
  matB <- getMat h w

  let dyxs = [(dy, dx) | dy <- [0 .. h - 1], dx <- [0 .. w - 1]]
  putStrLn $ yn . any (== matB) $ map (shiftMat matA) dyxs
```
:::

# [C - Cross](https://atcoder.jp/contests/abc300/tasks/abc300_c)

x 印の数をカウントせよという問題です。難し目の問題が来ました。

## 考察 (※ 公式解説と異なる)

グリッド中の正方形を切り出して、 n = 1, 2, .. の x 印の数を数えることにします。

たとえば以下の入力例では、 n = 1 の x 印が 1 つ、 n = 2 の x 印が 1 つ出てきます。この際、 **n = 2 の x 印も n = 1 の x 印としてカウント** してしまいます:

```txt:入力例
#.#.#...#
.#...#.#.
#.#...#..
.....#.#.
....#...#
```

| n | カウント |
|---|----------|
| 1 | 2        |
| 2 | 1        |

ただし正しい解答は、 n = 1 の x 印が **1** 個、 n = 2 の x 印が 1 個です。この差の原因は、 n = 2 の x 印を 2 回カウントしたことです。正しい解答を得るためには、 n = 1 のカウントから n = 2 のカウントを引かなければなりません。

多重カウントに規則性がありそうです。もう 1 つ x 印を増やした例を見てみます:

```txt:入力例
#.#.#...#.#.....#
.#...#.#...#...#.
#.#...#.....#.#..
.....#.#.....#...
....#...#...#.#..
...........#...#.
..........#.....#
```

| n | カウント | 解答 |
|---|----------|------|
| 1 | 3        | 1    |
| 2 | 2        | 1    |
| 3 | 1        | 1    |

n = 3, n = 2, n = 1 の順で見ると、正しい解答の累積和がカウントになっていることが分かります。したがって、すべての $n$ についてカウントを求めた後に、逆累積和を計算すれば答えが求まります。

## 解答

:::details 解答全体
[提出](https://atcoder.jp/contests/abc300/submissions/41160133)

```hs
import Control.Monad
import Data.Array.Unboxed
import Data.List (mapAccumL)
import Data.Tuple.Extra (both)

-- | 1 行読んで `[Int]` を作る。
getInts :: IO [Int]
getInts = map read . words <$> getLine

-- | h 行読んで `UArray (Int, Int) Char` を作る。
getMat :: Int -> Int -> IO (UArray (Int, Int) Char)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

-- | 1 辺の長さが `(2n + 1)` の正方形の中心座標を列挙する。
squareCenters :: Int -> (Int, Int) -> [(Int, Int)]
squareCenters n (h, w) = [(y, x) | y <- [n .. (h - 1 - n)], x <- [n .. (w - 1 - n)]]

-- | (y0, x0) を中心座標とする正方形に、大きさ n の x 印が存在するかを答える。
isCrossMark :: UArray (Int, Int) Char -> Int -> (Int, Int) -> Bool
isCrossMark mat n (y0, x0) = all isSharp crossPoints
  where
    -- その座標が `#` であるか
    isSharp (y, x) = mat ! (y, x) == '#'
    -- x 印を構成する (y, x) 座標一覧
    crossPoints = map (add2 (y0, x0)) dyxs
      where
        dyxs = [(dy, dx) | dy <- [-n .. n], dx <- [-n .. n], abs dy == abs dx]
        add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

-- | `n` に対応する x 印の数を数える。
-- | このとき n より大きな x 印も、大きさ n の x 印としてカウントしてしまう。
count :: UArray (Int, Int) Char -> Int -> Int
count mat n = length $ filter (isCrossMark mat n) centers
  where
    (h, w) = both succ . snd $ bounds mat
    -- 正方形の中心座標の一覧
    centers = squareCenters n (h, w)

-- | 累積和を元の数列に戻す
invCSum :: [Int] -> [Int]
invCSum = snd . mapAccumL step s0
  where
    s0 = 0 :: Int
    step lastX x = (x, x - lastX)

main :: IO ()
main = do
  [h, w] <- getInts
  mat <- getMat h w

  let nMax = min h w
  let counts = map (count mat) [1 .. nMax]
  let result = reverse . invCSum . reverse $ counts

  forM_ result print
```
:::

# [D - AABCC](https://atcoder.jp/contests/abc300/tasks/abc300_d)

## 考察

この問題ではオーバーフローへの対策が必要です。ざっくり見積もって、

- `Int` 型が取れる最大値は $2^{63}$ 程度、すなわち $9 \cdot 10^{18}$ 程度
- `a * a * b * c * c` の最大値は $n^{\frac 5 2}$ 程度、すなわち $10^{30}$ 程度

したがって、 a, b, c を 3 回かけた時点、 4 回かけた時点でも、都度積が $n$ 以上になっているかの判定が必要です。また $a$, $b$, $c$ を全列挙すると場合の数が増えすぎるため、 $a$, $b$ を決めた後は、 $c$ が取れる範囲を 2 分探索で求めてしまいます。

> もっとシビアな制約下では、 2 分探索では通らないかもしれません。

## 解答

いい加減な見積もりでしたが、通ってくれました。

:::details 解答全体
[解答](https://atcoder.jp/contests/abc300/submissions/41160618)

```hs:解答
{-# LANGUAGE BangPatterns #-}

import Data.Ix (inRange)
import Data.Maybe
import Data.Tuple.Extra (both)
import qualified Data.Vector.Unboxed as VU

-- | 素数の無限リスト。
-- | <https://zenn.dev/osushi0x/scraps/51ff0594a1e863#comment-1022553732563c>
primes :: [Int]
primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])
  where
    minus (x : xs) (y : ys) = case (compare x y) of
      LT -> x : minus xs (y : ys)
      EQ -> minus xs ys
      GT -> minus (x : xs) ys
    minus xs _ = xs

    union (x : xs) (y : ys) = case (compare x y) of
      LT -> x : union xs (y : ys)
      EQ -> x : union xs ys
      GT -> y : union (x : xs) ys
    union xs [] = xs
    union [] ys = ys

    unionAll :: Ord a => [[a]] -> [a]
    unionAll ((x : xs) : t) = x : union xs (unionAll $ pairs t)
      where
        pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t

-- | 境界の添字を返す 2 分探索。
-- |
-- | # 例
-- |
-- | `(<= 5)` という `isOk` 関数が与えられた場合、リスト `[0..9]` は以下のように見られる:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | この場合、 `(Just 5, Just 6)` というペアが返却される。
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (!low, !high) !isOk = both wrap (inner (low - 1, high + 1))
  where
    inner :: (Int, Int) -> (Int, Int)
    inner (!ok, !ng)
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

-- | `Int` 型の平方根 (切り落とし)
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO ()
main = do
  n <- readLn :: IO Int

  let pMax = isqrt (n `div` 12) + 1
  let ps = VU.fromList $ takeWhile (<= pMax) primes
  let nps = VU.length ps

  -- Int 型の取れる範囲は +- 2^63 ~ 9 * 10^18 程度。
  -- p <= sqrt 10^12 より p <= 10^6 であることから、オーバーフローに注意する必要がある。

  let result =
        [ count
          | ia <- [0 .. nps - 3],
            let a = ps VU.! ia,
            ib <- [ia + 1 .. nps - 2],
            let b = ps VU.! ib,
            let aab = a * a * b,
            aab <= n,
            let icMax = fromMaybe (-1) . fst $ bsearch (ib + 1, VU.length ps - 1) $ \ic ->
                  let c = ps VU.! ic
                   in aab * c <= n && aab * c * c <= n,
            let count = max 0 (icMax - ib)
        ]

  print $ sum result
```
:::

# [E - Dice Product 3](https://atcoder.jp/contests/abc300/tasks/abc300_e)

これは素因数分解と `ModInt` のテンプレートが無いと回答できないと思います。解法自体は素因数分解して配る DP なので、テンプレートを用意した人なら自力で解けると思います。

素因数分解や `ModInt` の解説は大変過ぎるので省略させていただきます。

https://atcoder.jp/contests/abc300/submissions/41161500

# 感想

模範的な記事は書けませんでした。あまり大変だったので、解説を書くのは最初で最後にしようと思います。

