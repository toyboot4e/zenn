---
title: "AtCoder 用 Haskell 逆引き (失敗編)"
emoji: "🌵"
type: "idea" # tech: 技術記事 / idea: アイデア
topics: ["haskell", "atcoder"]
published: true
---

# この記事は

Haskell 入門者あるあるをまとめたものです。

## 予備知識

- TLE: time limit exceed (時間制限超過)
- MLE: memory limit exceed (メモリ制限超過)

# 芝刈り・しばかれ

前提の `import` です:

```hs
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
```

それではあるあるを挙げていきます。

## 1. HLS が動かない

エラー表示も、定義ジャンプも、型名表示すら無いままコンテストに挑みます。ひもじい気分を味わいます。

## 2. リストへのランダムアクセスで TLE

連結リストへのランダムアクセスは $O(N)$ です。 B 問題の TLE が H 本を制覇した Haskller の心を折ります。

```hs:問題のコード
xs !! i
```

```hs:対策後
xs VU.! i
```

## 3. 永遠に処理が戻ってこない (タイポ再帰)

Haskell では遅延評価ができます。再帰的な変数を作ってしまうと、制御が戻らなくなることがあります。

```hs:問題のコード
let xs = 3 : xs
 in 0 : xs
```

```hs:対策後
let ys = 3 : xs
 in 0 : ys
```

## 4. 永遠に処理が戻ってこない (無限リスト)

無限リストをフィルタリングすると無限リストが返ってきます。どこかで切り落としましょう。

```hs:問題のコード
filter (<= boundary) [1..]
```

```hs:対策後
takeWhile (<= boundary) [1..]
```

## 5. `Vector.Unboxed` で 2 次元配列が持てない

型が合わないときは謎のコンパイルエラーを食らいます。エラーメッセージの癖を掴みましょう。

```hs:問題のコード
mat <- VU.replicateM n getLineIntVec
```

```hs:対策後
mat <- V.replicateM n getLineIntVec
```

## 6. `UArray` でタプルが持てない

同上です。逆に `Vector.Unboxed.Vector` でタプルが持てるのは、内部的に `Vector` 2 本になるためです。

```hs:問題のコード
table <- newArray @IOUArray bounds_ (0, 0)
```

```hs:対策後
table <- newArray @IOArray bounds_ (0, 0)
```

## 7. Boxing により MLE

テーブルサイズの大きな DP で起きました。 Unboxed なデータ型を使いましょう。また DP では遅延評価に頼らないようにしましょう。

```hs:問題のコード
table <- newArray @IOArray bounds_ e0
```

```hs:対策後
table <- newArray @IOUArray bounds_ e0
```

## 8. 言語拡張 `NPlusKPatterns` でエラー

言語拡張を有効にして `ghci` を起動します:

```sh
$ ghci -XNPlusKPatterns
Prelude> 
```

一見 N + K パタンは問題なく動きます:

```hs
Prelude> let x = 1
Prelude> let (n + 1) = x
Prelude> n
0
```

しかし `n` が負の数になるとエラーが発生します:

```hs
Prelude> let x = 0
Prelude> let (n + 1) = x
Prelude> n
*** Exception: <interactive>:2:5-15: Non-exhaustive patterns in n+1
```

使わないほうが無難かもしれません。

## 9. `runST` の中から複数の値を返せない

ST (state thread) モナドの範囲で別の変数を書き換えようとして失敗しました。モナドをほぼ理解していなかった頃ですね。

対策としては、

- モナドを合成する
- すべての可変変数を IO モナドに入れる
- 『別の変数』は `foldM` のループで更新する

## 10. `Unbox` を実装できない

昔の `vector` は `Unbox` の実装が異様に難しいです。そのため他のパッケージを頼るのが無難です。

- `vector` の代わりに [unboxing-vector](https://www.stackage.org/lts-16.11/package/unboxing-vector-0.1.1.0) を使う
  [Sum type に対しては `Unboxable` を実装できない](https://github.com/minoki/unboxing-vector/commit/889462f6a69a6be8f117748da6fe22263aac6f8e) 点は留意します。
- [vector-th-unbox](https://www.stackage.org/lts-16.11/package/vector-th-unbox-0.2.1.7) を使う
  Template Haskell でコード生成します。

```hs
-- vector-th-unbox: https://www.stackage.org/lts-16.11/package/vector-th-unbox-0.2.1.7
import Data.Vector.Unboxed.Deriving (derivingUnbox)

newtype UnionFind = UnionFind (VU.Vector UFNode)

-- | `Child parent | Root size`
data UFNode = UFChild {-# UNPACK #-} !Int | UFRoot {-# UNPACK #-} !Int

_ufrepr1 :: UFNode -> (Bool, Int)
_ufrepr1 (UFChild x) = (True, x)
_ufrepr1 (UFRoot x) = (False, x)

_ufrepr2 :: (Bool, Int) -> UFNode
_ufrepr2 (True, x) = UFChild x
_ufrepr2 (False, x) = UFRoot x

derivingUnbox "UFNode" [t|UFNode -> (Bool, Int)|] [|_ufrepr1|] [|_ufrepr2|]
```

# 終わりに

当時は大変で、自力で Haskell が書けないと挫折感を味わいました。今でも型関連のエラーが直せないときは質問します。 AtCoder に絞っても相当難しい言語なので、完全な習得には年単位で時間が必要なのではないかと思います。

その分だけ得るものは多いと感じています。同様の方々へ、快適な AC を！

