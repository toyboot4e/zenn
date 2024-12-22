---
title: "全てからの脱出"
---


# ああああ

もう、止めたい。この世の全てから逃れたい。

そんなとき、我々は大域脱出します。


# 継続モナド (`Cont` モナド)

[`transformers`](https://hackage.haskell.org/package/transformers) の 継続モナド ([`Control.Monad.Trans.Cont`](https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-Cont.html)) は僅か 250 行。リッチなソース解説も、ございます！　ありがとうございます！

https://qiita.com/sparklingbaby/items/2eacabb4be93b9b64755


## `callCC` の使い方

仕組みは理解していませんが、 `callCC` を使ってみます。たとえば $2^n \ge x$ を満たす $2^n$ を求める関数があります:

```hs
-- >>> calc1 14
-- 16
calc1 :: Int -> Int
calc1 x0 = until (>= x0) (* 2) (1 :: Int)
```

これを Rust で手続き的に実装すればこんな形で:

```rust
fn calc_2(x0: usize) -> usize {
    let mut x = x0;
    while x < x0 {
        x *= 2;
    }
    x
}
```

`Cont` を使って手続き的な実装にすれば以下の通り。 `exit` を呼べば脱出できます:

```hs
calc2 :: Int -> Int
calc2 x0 = evalCont $ callCC $ \exit ->
  flip fix (1 :: Int) $ \loop acc -> do
    when (acc >= x0) $
      exit acc
    loop (acc * 2)
```

無駄に `ContT` を使って `Stete` モナドに返値を載せると、次のようになります:

```hs
calc2' :: Int -> Int
calc2' x0 = (`execState` (1 :: Int)) $ evalContT $ callCC $ \exit -> do
  fix $ \loop -> do
    acc <- get
    when (acc >= x0) $
      exit ()
    put (2 * acc)
    loop
```

なお `lift` は基本必要ありません。

-   [`MonadCont`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Cont.html#t:MonadCont) は `StateT` に対して実装されています。
-   [`PrimMonad`](https://hackage.haskell.org/package/primitive-0.9.0.0/docs/Control-Monad-Primitive.html) でも `PrimMonad m => PrimMonad (ContT r m)` が提供されており、たとえば `ContT () (ST s)` は `PrimMonad` を実装します。


## `callCC` が活きる問題

[ABC 345 - D](https://atcoder.jp/contests/abc345/tasks/abc345_d) は枝刈りが本質的な計算量の改善に繋がる問題で、大域脱出が大活躍します。継続モナドを使用した場合、コードは *比較的* 整理がつきましたが、実行速度は約 3 倍になりました:

-   [継続モナド使用前 (91 ms)](https://atcoder.jp/contests/abc345/submissions/51371574)
-   [継続モナド使用時 (281 ms)](https://atcoder.jp/contests/abc345/submissions/51521918)

`callCC` は `ST` モナドと併せて使用しています:

```haskell
solve :: (HasCallStack) => Int -> Int -> U.Vector (Int, Int) -> Bool
solve h w tiles = runST $ evalContT $ callCC $ \exit -> do
  {- ゴチャゴチャな実装は省略…… -}
```

[cojna さんの提出](https://atcoder.jp/contests/abc345/submissions/51403780) は 60 ms です。 `callCC` を使っていますが、めちゃめちゃ速い！　この提出から `ContT` の使い方を学ばせてもらいました。非常に助かっています。


# まとめ

Haskell では関数から `return` できませんが、大域脱出はできます。この辺りは Lisp とも類似しており、他言語習得にも役立ちそうです。

また `callCC` に限らず継続モナドは大活躍するようです。まだまだ遊べます。

https://takoeight0821.hatenablog.jp/entry/2024/03/12/150448
