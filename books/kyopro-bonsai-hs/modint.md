---
title: "遅くない ModInt"
---


# `ModInt` 型の定義

`ModInt` 型の実装は、畳み込み (`convolution`) の実行速度に直に影響しますから重要です。遅過ぎない程度の高速化を目指しました。

内部データ型は本家 ACL に合わせて `Word32` にしました。 `Int` や `Word64` でも問題ありませんが、長い配列を作る場合は `Word32` の方が高速なのではないかと思います。

```haskell
newtype ModInt a = ModInt {unModInt :: Word32}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Read, Show)
```

早速『思います』と口にしましたが、厳密な確認ができておりません。遅過ぎることはないと思いますが、重要な最適化を逃している可能性はあります。


# 型から整数を復元する


## `KnownNat` 型

`ModInt a` の型パラメータ `a` は法の値を表します。 `KnownNat` 型にすると `ModInt 998244353` のように型を書けて良さそうです。

`KnownNat` 型は 2 種類あり、以下のモジュールで定義されています:

-   [`GHC.TypeNats`](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-TypeNats.html): `natVal` 関数で `Nat` 型に復元できます
-   [`GHC.TypeLits`](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-TypeLits.html): `natVal` 関数で `Integer` 型に復元できます

いずれも `KnownNat (n :: Nat)` が内部的に `Nat` 型で定義されていることから、 [`GHC.TypeNats`](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-TypeNats.html) を使った方がパフォーマンス面では無難なようです。参考:

https://zenn.dev/mod_poppo/articles/playing-with-visible-forall#%E5%9E%8B%E3%82%AF%E3%83%A9%E3%82%B9


## `AllowAmbiguousTypes` vs `Tagged` vs `Proxy#`

上のリンク先では、 (主に) 以下の 3 つの方法で型変数 `a` から `Int` を復元する方法が検討されています:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

newtype Tagged t a = MkTagged { unTagged :: a }

class Foo a where
  -- 1. AllowAmbiguousTypes
  someValueAmb :: Int
  -- 2. Tagged
  someValueTagged :: Tagged a Int
  someValueProxy :: Proxy a -> Int
```

追加するとしたら `Proyx#` で、 `Proxy` よりは速いはずです:

```haskell
-- 3. Proxy#
someValueProxy' :: Proxy# a -> Int
```

軽くベンチマークした限りでは、 `AllowAmbiguousTypes`, `Tagged`, `Proxy#` 間で `ModInt` のパフォーマンスに変化はありませんでした。そのため比較的一般的な `Proxy#` を採用しました。

> より複雑なケースが心配で、正直確認まで手が回っていませんが、遅過ぎるということはないでしょう。


# 遅過ぎない四則演算 + `pow`

`ModInt` の実装では、とにかく剰余の計算 (`mod`) をしないことが重要です。 ACL を写経して実装しました。


## `(+)`

`x1 + x2` が法 `m` を超えていたら `m` を引きます:

```haskell
{-# INLINE (+) #-}
(ModInt !x1) + (ModInt !x2)
  | x' >= m = ModInt $! x' - m
  | otherwise = ModInt x'
  where
    !x' = x1 + x2
    !m = fromIntegral (natVal' (proxy# @p))
```


## `(-)`

`x1 - x2` がアンダーフローした場合に法 `m` を足し直してオーバーフローさせて戻します。ちょっと驚きました:

```haskell
{-# INLINE (-) #-}
(ModInt !x1) - (ModInt !x2)
  | x' >= m = ModInt $! x' + m
  | otherwise = ModInt x'
  where
    !x' = x1 - x2
    !m = fromIntegral (natVal' (proxy# @p))
```


## `(*)`

`Word64` で計算して `Word32` に戻します。 `mod` 計算を実施しますが、仕方ありません:

```haskell
{-# INLINE (*) #-}
(ModInt !x1) * (ModInt !x2) = ModInt $! fromIntegral (x' `rem` m)
  where
    !x' :: Word64 = fromIntegral x1 * fromIntegral x2
    !m :: Word64 = fromIntegral (natVal' (proxy# @p))
```


## `(/)`

`Fractional` クラスの `recip` を通して実装します:

```haskell
instance (Modulus p) => Fractional (ModInt p) where
  {-# INLINE recip #-}
  recip = inv
  {-# INLINE fromRational #-}
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
```

`recip` の実装は、 ACL の実装に合わせて法が素数かで分岐します。 ACL はコンパイル時に分岐していますが、 `ac-library-hs` の `ModInt` では実行時に分岐しています。法が素数の場合は [フェルマーの小定理](https://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A7%E3%83%AB%E3%83%9E%E3%83%BC%E3%81%AE%E5%B0%8F%E5%AE%9A%E7%90%86) により $x^{-1} \equiv x^{p-2} \mod m$ です:

```haskell
{-# INLINE inv #-}
inv :: forall a. (HasCallStack, Modulus a) => ModInt a -> ModInt a
inv self@(ModInt x)
  | isPrimeModulus (proxy# @a) =
      -- 法が素数の場合
      let !_ = ACIA.runtimeAssert (x /= 0) "AtCoder.ModInt.inv: tried to perform zero division"
       in pow self (fromIntegral (natVal' (proxy# @a)) - 2)
  | otherwise =
      let (!eg1, !eg2) = ACIM.invGcd (fromIntegral x) $ fromIntegral (natVal' (proxy# @a))
          !_ = ACIA.runtimeAssert (eg1 == 1) "AtCoder.ModInt.inv: `x^(-1) mod m` cannot be calculated when `gcd x modulus /= 1`"
       in fromIntegral eg2
```

高速な素数判定のため、型クラス `Modulus` を定義しました:

```haskell
class (KnownNat a) => Modulus a where
  -- 素数判定
  isPrimeModulus :: Proxy# a -> Bool
  -- 原子根を取得 (convolution で使用)
  primitiveRootModulus :: Proxy# a -> Int

instance Modulus 998244353 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3
```

新しい `Modulus` のインスタンスは orphan instance になりますが、ほぼ定義することは無いため問題無いでしょう。


## `pow` ($x^n \bmod m$)

`pow` は繰り返し二乗法により計算します。このように何度も $x \cdot y \bmod m$ を計算する場合は、 Barrrtt reduction や Montgomery 乗算が高速であると知られています。 ACL では Barrett reduction を実施しています:

```haskell
{-# INLINE pow #-}
pow :: forall a. (HasCallStack, KnownNat a) => ModInt a -> Int -> ModInt a
pow (ModInt x0) n0 = ModInt . fromIntegral $ inner n0 1 (fromIntegral x0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.ModInt.pow: given negative exponential `n`: " ++ show n0 ++ show "`"
    bt = ACIBT.new64 $ fromIntegral (natVal' (proxy# @a))
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then ACIBT.mulMod bt r y else r
              y' = ACIBT.mulMod bt y y
           in inner (n !>>. 1) r' y'
```

> Barrett reduction の実装は省略しますが、 128 bit 整数の実装に [`wide-word`](https://hackage.haskell.org/package/wide-word) パッケージを利用しています。

一応、次の記事にある 3 つの mod 演算のベンチマークを取ってみましたが、なぜか Barrett reduction が最も高速でした。

https://natsugiri.hatenablog.com/entry/2020/04/06/030559


# 簡易ベンチマーク結果

[`cojna/iota`](https://github.com/cojna/iota) を参考に、 [`criterion`](https://hackage.haskell.org/package/criterion) で各演算の実行速度を比較しました。入力値はランダムです。結果の雰囲気だけお伝えします。


## `(+)`

![cover](/images/kyopro-bonsai-hs/bench-addMod.png)

addMod を 10,000 回計算しました。

-   mod 演算を避けたことで 6 倍以上高速化されました。
-   `MagicHash` を使った実装は高速化されていませんでした。


## `(*)`

![cover](/images/kyopro-bonsai-hs/bench-mulMod.png)

Barrett reduction 等の準備計算の時間を除外して 10,000 回 mulMod を計算しました。

-   Barrett reduction > Montgomery 乗算 > Barredd reduction (64) の順で速かったです。
-   Barrett reduction は `rem` の 1.5 倍ほど高速化されました。


## `pow`

![cover](/images/kyopro-bonsai-hs/bench-powMod.png)

10,000 回 powMod (繰り返し二乗法) を計算しました。

-   Barrett reduction > Montgomery 乗算 > Barredd reduction (64) の順で速かったです。
    -   いずれも下準備のコストがあるため、 `powMod` ほど `rem` を圧倒しません。
-   [`(^)`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:-94-) が普通の `powerRem` よりも速かったです。今気づきましたが、 `(^)` の繰り返し二乗法の実装をコピーすれば、今より `pow` を高速化できる可能性があります。

> なお本家 ACL の `modint.hpp` の [`pow`](https://github.com/atcoder/ac-library/blob/fe9b6fca9ab4e1be946ea23a4e6a2a751cf4aaa2/atcoder/modint.hpp#L95) ではなぜか Barrett reduction を実施していません。報告・確認した方が良いでしょうか……？


## まとめ

ベンチマークテストを確認しつつ、遅過ぎない程度に `ModInt` を高速化したつもりです。本当は `convolution` 全体の速度を比較する必要があり、特に Haskell では特殊化うんぬんで速度が変わります。
