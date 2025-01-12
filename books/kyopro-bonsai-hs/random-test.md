---
title: "ランダムテストの盾"
---


# 今しかバグを修正できない

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) の `Extra` モジュールでは、主に `HashMap` や `IntMap` などの可変データ構造を提供しています。これらのソースファイルは、一度 AtCoder 環境に取り込まれたら **更新不可能** となりますから、事前にバグを洗い出す必要があります。

以前の章までに書いたテストは、開発を進めるための攻めのテストでした。今回のテストは、バグを予防するための守りのテストです。ここでは全ての関数をテストするため、 QuickCheck でランダムテストを書いてみます。


# ランダムテストを作成する


## 例 1: `HashMap`

リファレンス実装として、 [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers-0.2.20) の [`HashMap`](https://hackage.haskell.org/package/unordered-containers/docs/Data-HashMap-Strict.html) と動作を比較します。


### 雛形

初期状態を `Init` で、操作を `Query` で、操作結果を `Result` で表すことにしました。テスト自体は大まかに以下の形です:

```haskell
prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  -- モナディックな初期化を実施
  myImplementation <- myM
  -- クエリを生成
  q <- QCM.pick $ QC.chooseInt (1, 5 * capacity)
  qs <- QCM.pick $ QC.vectorOf q (QC.arbitrary @Query)
  foldM_
    ( \ref query -> do
        -- リファレンス実装と自作実装の結果を比較
        let (!expected, !ref') = handleRef ref query
        actual <- handleAcl myImplementation query
        QCM.assertWith (expected == actual) $ show query
        pure ref
    )
    ref0
    qs
```

より優れた定型的な書き方がありそうですが、『正解』を探すとキリがありませんから、ここは先に進みます。

:::details import
```hs
{-# LANGUAGE RecordWildCards #-}

import AtCoder.Extra.HashMap qualified as HM
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.HashMap.Strict qualified as HMR -- R: referencial implementation
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
```
:::

1. 初期状態 `Init`:

```haskell
data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    -- リファレンス実装
    ref0 :: !(HMR.HashMap Int Int),
    -- 自作
    hmM :: !(IO (HM.HashMap RealWorld Int))
  }

instance Show Init where
  show Init {..} = show (capacity, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    capacity <- QC.chooseInt (1, 10)
    pure $ Init capacity HMR.empty (HM.new capacity)
```

2. 操作 `Query`:

```haskell
data Query
  = Size
  | Member Int
  | NotMember Int
  | Lookup Int
  | Insert Int Int
  | InsertWithAdd Int Int
  | Exchange Int Int
  | ModifyAdd Int Int
  | Clear
  deriving (Show)

instance QC.Arbitrary Query where
  arbitrary = do
    -- クエリをランダム生成する
    QC.frequency
      [ -- 全状態を破棄するクリア処理は極稀に実施する:
        (rare, pure Clear),
        -- 他の処理は均等な確率で実施する:
        (often, pure Size),
        (often, Member <$> keyGen),
        (often, NotMember <$> keyGen),
        (often, Lookup <$> keyGen),
        (often, Insert <$> keyGen <*> valGen),
        (often, InsertWithAdd <$> keyGen <*> valGen),
        (often, Exchange <$> keyGen <*> valGen),
        (often, ModifyAdd <$> keyGen <*> valGen)
      ]
    where
      rare = 1
      often = 10
      keyGen = QC.chooseInt (-5, 5)
      valGen = QC.arbitrary @Int
```

3. 操作結果 `Result`:

```haskell
-- 結果比較用 (本当は Eq を upcast したい)
data Result
  = None
  | B Bool
  | I Int
  | M (Maybe Int)
  deriving (Show, Eq)
```

後はリファレンス実装と自作実装のクエリ処理関数を作成し、実行結果を比較します。実は **要素削除できない仕様の `HashMap` に `delete` 関数を生やしていたり** と、ヤバいバグが見つかりました。

これに懲りて本格的にテストを書き始めました。他の mutable 構造に対しても、大体同じ雛形に沿ってランダムテストを実施しています。


## 例 2: 辞書順最小のトポロジカルソート

まず DAG をランダム生成します。面白かったのですが、すべての辺を $u \rightarrow v (u \lt v)$ とすると、明らかに DAG になります。この DAG に大してランダムに頂点番号を振り直すことで、完全ランダムな DAG を生成することができます。

```haskell
genDag :: Int -> QC.Gen (Gr.Csr ())
genDag n = do
  edges <- VU.fromList <$> QC.sublistOf [(u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1]]
  verts <- VU.fromList <$> QC.shuffle [0 .. n - 1]
  pure $ Gr.build n $ VU.map (\(!u, !v) -> (verts VG.! u, verts VG.! v, ())) edges
```

後は愚直解を作成し、高速解と比較します。愚直解としては、ランダムな順列であってトポロジカル順になっているものを列挙し、辞書順最小のものを見つけます。テストを支えるのは、こうした地道なコーディングスキルなんですね。


## 例 3. 行列の逆元のテスト

何気なく生やした 2x2 行列の `zero`, `ident`, `inv` 関数をテストします。ランダムテストに比べ、こうした性質テストは最小限の労力で書けます:

```haskell
-- ゼロ写像 (?)
prop_mat2x2Zero :: Mat2x2 Int -> QC.Property
prop_mat2x2Zero a =
  QC.conjoin
    [ M.zero <> a QC.=== M.zero,
      a <> M.zero QC.=== M.zero
    ]

-- 恒等写像
prop_mat2x2Ident :: Mat2x2 Int -> QC.Property
prop_mat2x2Ident a =
  QC.conjoin
    [ M.ident <> a QC.=== a,
      a <> M.ident QC.=== a
    ]

-- 逆元
prop_mat2x2Inv :: Mat2x2 ModInt.ModInt998244353 -> QC.Property
prop_mat2x2Inv a =
  QC.conjoin
    [ M.inv a <> a QC.=== M.ident,
      a <> M.inv a QC.=== M.ident
    ]
```

お気づきでしょうか、逆元のテストに問題があり、ゼロ除算が発生します。恥ずかしながら、既にその直感を失っていますが、正方行列 $A$ が [逆行列](https://manabitimes.jp/math/1153) を持つ (正則である) ことは $\mathrm{det} A \neq 0$ と同値なのでした。テストの条件に追加します:

```haskell
-- 逆元
prop_mat2x2Inv :: Mat2x2 ModInt.ModInt998244353 -> QC.Property
prop_mat2x2Inv a =
  -- 固有値が 0 でない場合
  (M.det a /= 0 QC.==>) $
    QC.conjoin
      [ M.inv a <> a QC.=== M.ident,
        a <> M.inv a QC.=== M.ident
      ]
```

こうして `Mat2x2` モジュールに `det` 関数が追加され、少し使い勝手の良いモジュールになったと思います。


## 例 4: 異常な入力値のテスト

一番しんどい部分です。たとえば $[0, n)$ 内の $[l, r)$ 区間を受け取る関数において、 $r > l$ であったり、 $[0, n)$ 以外の値が区間の範囲として与えられた時にどうするか。雑に `assert` するか、 `Nothing` を返すか、実は `[0, n)` 区間に clamp しなければならないか、などよく考える必要があります。

苦手分野です。正直、いい加減にやりました。決めた設計はドキュメントに反映します。


# まとめ

`Extra` モジュールにテストを追加すると、設計レベルのバグが多数露見しました。他にも様々な問題が眠っている可能性はありますが、 9 割方正しいコードになったのではないかと思います。

`HashMap` の例のように、クエリ処理のランダムテストを書くのはしんどかった一方で、性質テストは最小限の労力で作ることができ、割が良いと感じました。読み返してみても、テストの意味やコードの内容が明確であり、ほとんど一瞬で把握できます。単体テストの手続きを 1 つ 1 つ追っていくことに比べ、なんて楽かと思います。少なくとも、競プロと QuickCheck の相性は最高です。
