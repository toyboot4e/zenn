---
title: "[3] 3. グラフ探索を得意技にする"
---

グラフ探索は茶〜緑レーティングを目指す際の難関です。ここでは惜しみなく手続き的プログラミングをするのがおすすめです。

:::message
なお以降の方法が優れているかより、自身が納得してプログラムを書けるかが大切だと思います。
:::

# DFS / BFS

深さ優先探索 (DFS) と幅優先探索 (BFS) を例に、実装のおすすめを紹介します。

## グラフの表現

Haskell では `Array Int [Int]` をグラフとすることが多いです。隣接頂点にアクセスするときには `!` 演算子を使います。理解が簡単なため、一旦この形式を採用します。

## 可変データ型の使用がおすすめ

探索済みフラグなど、頂点毎にデータを記録する際には可変配列を使うのがおすすめです。 `IntSet` や `IntMap` のような永続データ型を使った場合、パフォーマンスが大幅に低下します。コードも難しくなると思います。

BFS などでキューが必要なときは、 [`cojna/iota`] の [`Data.Buffer`] を使うのがおすすめです。永続データ型を使う場合はリストや `Seq` を使うことになりますが、やはり苦労する割にパフォーマンスが悪いと思います。

具体例は下の方で改めて紹介します。

## 永続データ型はおすすめしません

おすすめしませんが、グラフ探索のお供に `IntSet` のような永続データ型を使う場合は、 `State` モナドを使うと認知的負荷が減るかもしれません。

たとえばモナド無しで DFS を書けば次の通りです (※ バグってないといいのですが):

```hs
import Control.Monad.State.Strict
import Data.Array (Array)
import Data.Array.IArray
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L

dfsIM :: Array Int [Int] -> Int -> IM.IntMap Int
dfsIM gr vStart = inner (0 :: Int) IM.empty vStart
  where
    inner :: Int -> IM.IntMap Int -> Int -> IM.IntMap Int
    inner d !im v1
      | IM.member v1 im = im
      | otherwise = L.foldl' (inner d') im' $ gr ! v1
      where
        d' = d + 1
        im' = IM.insert v1 d im
```

ここで `State` モナドを使用した場合、 `forM` で書けるため単純になります:

```hs
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Strict
import Data.Array (Array)
import Data.Array.IArray
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L

dfsIM' :: Array Int [Int] -> Int -> IM.IntMap Int
dfsIM' gr vStart = execState (inner (0 :: Int) vStart) IM.empty
  where
    inner :: Int -> Int -> State (IM.IntMap Int) ()
    inner d v1 = do
      modify $ IM.insert v1 d
      forM_ (gr ! v1) $ \v2 -> do
        unlessM (gets (IM.member v2)) $ do
          inner (d + 1) v2
```

> 単純になってないかも……

なお `containers` パッケージの関数のシグネチャは `State` モナドと相性が良いように思います。たとえば `modify $ IM.insert v1 d` のようにポイントフリースタイルができます。

# CSR 形式のグラフのおすすめ

先程は `Array Int [Int]` でグラフを表しましたが、グラフのデータ型としては [`cojna/iota`] の [`SparseGraph`] がおすすめです。効率が良い上に API も統一的になります。

## `Array Int [Int]` に関して

`Array Int [Int]` にはパフォーマンス上の問題があります:

- Array が boxed 型である  
  Boxed な配列はとにかく遅いという経験則があって不安です。

- リストをコンテナとして使っている  
  リストの替わりに array や vector を使った方が効率は良くなりそうです。リストをストリームとして使うのは良いですが、コンテナとして使うことには疑問があります。

API の問題もあります:

- 辺に重みがある場合に `!` 演算子が返すデータ型が変わる  
  たとえば重み付きグラフを `Array Int [(Int, Weight)]` とする場合、連結成分を集める関数を重みと無関係にすることができません。型パラメータを追加するなどライブラリを作り込む際は、 `SparseGraph` に乗り換えれば良いと思います。

## `SparseGraph` に関して

グラフのデータ表現としては [`cojna/iota`] の [`SparseGraph`] をおすすめします。これは隣接リスト形式のグラフを [CSR (compressed sparse row)](https://qiita.com/AkariLuminous/items/31faea745b5dd4fb9edc) として保存したデータ型であり、 unboxed vector を使ってパフォーマンス上の問題を解決しています。

API も良いと思います。重みが型パラメータ `w` で抽象化されており、重みなしの辺を取得する関数が `adj`, 重みありの辺を取得する関数が `adjW` と分かれています。そのため `w` に制約を付けず `adj` 関数のみを使う場合は、重みの有無に関わらず使用できる関数となります。 `Num w` のように制約を設けたり `Int` のような具体的な型を `w` として指定すれば、辺に重みのあるグラフを指定できます。

> なお頂点に重みがある場合は、グラフとは別に配列を持つことになると思います。たぶん。

## 頂点の次元に関して

さて `SparseGraph` のように頂点を 1 次元 (整数) で表してしまって良いものでしょうか。問題無いと思います。たとえば以下のケースを考えてみます。

1. 各頂点が 1, 2, .. の通し番号として与えられる問題
`SparseGraph` がそのまま使えます。

2. 各頂点を `(i, j)` のような 2 成分で表したい問題
`SparseGraph` の辺を作る際に `(i, j)` 成分を 1 次元の通し番号に変換します。グラフ探索の際には 1 次元の通し番号をそのまま使えば良いです。

3. 2 次元グリッドの問題
これには、次項の通り `SparseGraph` を作らない (辺データを生成しない) のがおすすめです。

# グラフの一般的な表現

グラフ探索を実装する際は、グラフを関数で表現すると実装も簡単になることが多いです。 (辺の) 重み無しグラフは `頂点 -> U.Vector 頂点`, 重み付きグラフは `頂点 -> U.Vector (頂点, 重み)` で表します。そして必要ならばグラフを表す関数の中で添字成分を扱えば良いです。

たとえば周囲 4 方向に移動できる 2 次元グリッドの問題において、以下の `gridFn` によって `Vertex -> U.Vector Vertex` というグラフの表現を生み出せます:

```hs
type Vertex = Int

-- | . と # で構成されるグリッドにおいて、ある点と隣接する . を返す関数を作る
gridFn :: UArray (Int, Int) Char -> Vertex -> U.Vector Vertex
gridFn grid = index bnd . filter p . around . unindex
  where
    unindex = (`divMod`) w
    around yx@(!_, !_) = U.filter (inRange bnd) $ U.map (add2 yx) dir4
    bnd = bounds grid
    w = snd (snd bnd) - snd (fst bnd) + 1
    p = (== '.') . (grid !)

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (!y1, !x1) (!y2, !x2) = (y1 + y2, x1 + x2)

dir4 :: U.Vector (Int, Int)
dir4 = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]
```

グラフ探索の実装の際は、 2 次元グリッドという具体的な対象を忘れ、 `頂点 -> U.Vector 頂点` というより抽象的な、グラフと呼ぶにふさわしい表現に専念できます。頂点が単純な `Int` であるために、単純な配列に距離を記録できる点もメリットです:

```hs
-- | 幅優先探索
genericBfs :: (Vertex -> U.Vector Vertex) -> Int -> Vertex -> U.Vector Int
genericBfs gr nVerts start = U.create $ do
  let !undef = -1 :: Int

  -- 始点と各頂点間の最短距離 (@genericBfs@ の戻値)
  !dist <- UM.replicate nVerts undef
  UM.unsafeWrite dist start (0 :: Int)

  -- @cojna/iota@ の @Data.Buffer@ をキューとして使う
  !queue <- newBufferAsQueue nVerts
  pushBack queue start

  -- 再帰ループ
  fix $ \loop -> do
    -- キューから 1 つ要素を取り出す
    popFront queue >>= \case
      Nothing -> return ()
      Just !v1 -> do
        !d1 <- UM.unsafeRead dist v1
        -- @v1@ の隣接頂点 @v2@ を調べる
        U.forM_ (gr v1) $ \v2 -> do
          -- @v2@ は訪問済み？
          !visited <- (== undef) <$> UM.unsafeRead dist v2
          unless visited $ do
            -- 未訪問であれば距離を保存してキューに追加する
            UM.unsafeWrite dist v2 $! d1 + 1
            pushBack queue v2

        -- ループする
        loop

  return dist
```

# まとめ

グラフ探索におけるおすすめのスタイルを紹介しました。可変データ型を使うこと、 CSR 形式の [`SparseGraph`] 、関数をグラフとみなすことがおすすめです。あまりにもグラフ問題が苦手だという人には、特に手続き的なスタイルもアリなのだとご認識頂ければ幸いです。

[`cojna/iota`]: https://github.com/cojna/iota
[`Data.Buffer`]: https://cojna.github.io/iota/Data-Buffer.html
[`SparseGraph`]: https://github.com/cojna/iota/blob/master/src/Data/Graph/Sparse.hs

