---
title: "モノイドのテスト"
---


# 頻出バグ

遅延伝播セグメント木の問題では、以下の性質を満たす 2 種類のモノイド $F, X$ を定義します。

1.  モノイド作用: $(f_2 \circ f1) * x = f_2 * (f1 * x)$
2.  恒等写像: $f^0 * x = x$
3.  自己同型写像: $f (x1 \cdot x2) = (f * x1) \cdot (f * x2)$

> [半群作用の wiki](https://en.wikipedia.org/wiki/Semigroup_action#Formal_definitions) を参考に見様見真似で書いてみました。不正確でしたらすみません。


## Haskell における表記法

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) では以下の形に落とし込みました。

| 数式    | Haskell      |
| ------- | ------------ |
| $\circ$ | `(<>) @f`    |
| $\cdot$ | `(<>) @a`    |
| $f^0$   | `mempty @f`  |
| $*$     | 以下の `segAct` |

```haskell
class (Monoid f) => SegAct f a where
  segAct :: f -> a -> a
```

上記の 3 つの性質をコードにすれば、以下の性質を満たす必要があります:

1.  ``(f2 <> f1) `segAct` x == f2 `segAct` (f1 `segAct` x)``
2.  ``(mempty @f) `segAct` x == x``
3.  ``f `segAct` (x2 <> x1) == (f `segAct` x2) <> (f `segAct` x1)``

この性質を破ってしまったがために問題が解けないことは珍しくありません。超重要！！


## 追加の API

[maspy さん](https://atcoder.jp/users/maspy) の [遅延伝播セグメント木](https://github.com/maspypy/library/blob/fb8c2faf726e432b0d6c976ebb739cf2f040f553/ds/segtree/lazy_segtree.hpp#L157) では、作用の関数が区間長を受け取ります。

```cpp
void apply_at(int k, A a) {
  ll sz = 1 << (log - topbit(k));
  dat[k] = AM::act(dat[k], a, sz);
  if (k < size) laz[k] = MA::op(laz[k], a);
}
```

これが非常に上手く働きます:

-   $X$ に区間長を埋め込まなくて済みます。
-   $F$ から $X$ への作用は、 $X$ に対して制約を設けずに区間長を得ることができます。

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) では真似して `segActWithLength` も用意しています。

```haskell
class (Monoid f) => SegAct f a where
  {-# INLINE segAct #-}
  segAct :: f -> a -> a
  segAct = segActWithLength 1

  segActWithLength :: Int -> f -> a -> a
```

`segActWithLength` に対しては、以下の等式が成り立つ必要があります:

4. `segActWithLength len f a == times len (segAct f a) a`

以上の性質をテストして行きましょう。


## テストすべきモノイド

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) では、 `SegAct` のサンプルを `Extra` モジュールで提供します。これらのモノイドをテストして行きましょう。

-   [`Affine1.hs`](https://github.com/toyboot4e/ac-library-hs/blob/2a5083aeca24896b9fe595edc0eb7f9e4cc6d8fd/src/AtCoder/Extra/Monoid/Affine1.hs) ($f: x \rightarrow a \dot x + b$)
-   [`RangeSet`](https://github.com/toyboot4e/ac-library-hs/blob/2a5083aeca24896b9fe595edc0eb7f9e4cc6d8fd/src/AtCoder/Extra/Monoid/RangeSetId.hs) ($f: x \rightarrow x + a$)


# 型クラスをテストする

[`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) が良さそうです。モノイド則などのテストが用意されているため、拡張して使います。


## `tasty` との互換レイヤの作成

[`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) は `tasty` との互換コードを提供していませんが、テストコードの [`Advanced.hs`](https://github.com/andrewthad/quickcheck-classes/blob/6713b482d6b823dce9cc90c48f770b1de98a007d/quickcheck-classes/test/Advanced.hs) がそのまま参考になります:

```haskell
tests :: TestTree
tests = testGroup "universe"
  [ testGroup "deriving"
    [ testGroup "strict"
      [ laws @A [eqLaws,ordLaws]
      , laws @B [eqLaws,ordLaws]
      , laws @F [eqLaws,ordLaws]
      -- ..
```

この `laws` 関数と、 `myForAllShrink` を拝借して `Util.hs` を作ります:

```haskell
module Tests.Util (myForAllShrink, laws) where

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

myForAllShrink ::
  (QC.Arbitrary a, Show b, Eq b) =>
  Bool -> -- Should we show the RHS. It's better not to show it if the RHS is equal to the input.
  (a -> Bool) -> -- is the value a valid input
  (a -> [String]) -> -- show the 'a' values
  String -> -- show the LHS
  (a -> b) -> -- the function that makes the LHS
  String -> -- show the RHS
  (a -> b) -> -- the function that makes the RHS
  QC.Property
myForAllShrink displayRhs isValid showInputs name1 calc1 name2 calc2 =
  QC.MkProperty $
    QC.arbitrary >>= \x ->
      QC.unProperty $
        QC.shrinking QC.shrink x $ \x' ->
          let b1 = calc1 x'
              b2 = calc2 x'
              sb1 = show b1
              sb2 = show b2
              description = "  Description: " ++ name1 ++ " = " ++ name2
              err = description ++ "\n" ++ unlines (map ("  " ++) (showInputs x')) ++ "  " ++ name1 ++ " = " ++ sb1 ++ (if displayRhs then "\n  " ++ name2 ++ " = " ++ sb2 else "")
           in isValid x' QC.==> QC.counterexample err (b1 == b2)

laws :: forall a. (Typeable a) => [Proxy a -> QCC.Laws] -> TestTree
laws =
  testGroup (show (typeRep (Proxy @a)))
    . map
      ( \f ->
          let QCC.Laws name pairs = f (Proxy @a)
           in testGroup name (map (uncurry QC.testProperty) pairs)
      )
```


## モノイド則を破っていた

まずは既存の関数で [モノイド則](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Monoid.html#t:Monoid) をテストしました。

```haskell
module Tests.Extra.Monoid (tests) where

import AtCoder.Extra.Monoid
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..), stimes)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (laws, myForAllShrink)

tests :: [TestTree]
tests =
  [ testGroup
      "Affine1"
      [ laws @(Affine1 (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ]
      ],
    testGroup
      "RangeSet"
      [ laws @(RangeSet (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ]
      ]
  ]
```

`RangeSet` のモノイド則がバグっていました。これが修正前です (`SegAct` の実装は省略します):

```haskell
newtype RangeSet a = RangeSet a
  deriving newtype (Eq, Ord, Show)

instance Semigroup (RangeSet a) where
  new <> _ = new

instance (Monoid a) => Monoid (RangeSet a) where
  {-# INLINE mempty #-}
  mempty = RangeSet mempty
  {-# INLINE mconcat #-}
  mconcat [] = RangeSet mempty
  mconcat (a : _) = a
```

これが修正後です。 `Bool` のフィールドが `False` の場合は恒等写像 (作用無し) になります:

```haskell
newtype RangeSet a = RangeSet (RangeSetRepr a)
  deriving newtype (Eq, Ord, Show)

type RangeSetRepr a = (Bool, a)

new :: a -> RangeSet a
new = RangeSet . (True,)

instance Semigroup (RangeSet a) where
  RangeSet (False, !_) <> old = old
  new_ <> _ = new_

instance (Monoid a) => Monoid (RangeSet a) where
  mempty = RangeSet (False, mempty)
  mconcat [] = mempty
  mconcat (RangeSet (False, !_) : as) = mconcat as
  mconcat (a : _) = a
```

これもよく盲点になります。


## `SegAct` 則をテストする

上記の 4 つの性質をコードで表現します。

```haskell
module Tests.Extra.Monoid (tests) where

import AtCoder.Extra.Monoid
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..), stimes)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (laws, myForAllShrink)

segActLaw :: (Monoid a, Eq a) => (SegAct f a, QC.Arbitrary f, Eq f, Show f, QC.Arbitrary f, QC.Arbitrary a, Show a) => Proxy (f, a) -> QCC.Laws
segActLaw p =
  QCC.Laws
    "SegAct"
    [ ("Monoid Action", segActMonoidAction p),
      ("Identity map", segActIdentity p),
      ("Endomorphism", segActEndomorphism p),
      ("Linear Monoid Action", segActLinearMonoidAction p)
    ]

segActIdentity :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActIdentity _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: a -> [String]
    desc (a :: a) = ["a = " ++ show a]
    lhsS = "segAct mempty a"
    lhs = segAct (mempty @f)
    rhsS = "a"
    rhs = id

segActMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActMonoidAction _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, f, a) -> [String]
    desc (!f2, !f1, !a) = ["f2 = " ++ show f2 ++ ", f1 = " ++ show f1 ++ ", a = " ++ show a]
    lhsS = "(f_2 <> f_1) a"
    lhs (!f2, !f1, !a) = (f2 <> f1) `segAct` a
    rhsS = "f_2 (f_1 a)"
    rhs (!f2, !f1, !a) = f2 `segAct` (f1 `segAct` a)

segActEndomorphism :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActEndomorphism _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, a, a) -> [String]
    desc (!f, !a1, !a2) = ["f = " ++ show f ++ ", a1 = " ++ show a1 ++ ", a2 = " ++ show a2]
    lhsS = "f (a1 <> a2)"
    lhs (!f, !a1, !a2) = segActWithLength 2 f (a1 <> a2)
    rhsS = "(f a1) <> (f a2)"
    rhs (!f, !a1, !a2) = (f `segAct` a1) <> (f `segAct` a2)

segActLinearMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActLinearMonoidAction _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.Positive Int, f, a) -> [String]
    desc (QC.Positive !len, !f, !a) = ["len = " ++ show len ++ ", f = " ++ show f ++ ", a = " ++ show a]
    lhsS = "segActWithLength len f (a^len)"
    lhs (QC.Positive !len, !f, !a) = segActWithLength len f $! stimes len a
    rhsS = "(f a)^len"
    rhs (QC.Positive !len, !f, !a) = stimes len (segAct f a)
```

これがやりたかったんですねー。やりました。


# まとめ

念願のモノイドのテストを作成しました。 [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) の仕組みに乗っかって、半群やモノイドの性質に加えて遅延伝播セグメント木の作用 (`SegAct`) の性質もチェックできました。

モノイド則や恒等写像が間違っていたり、作用の対象のインスタンスを `Num a` のように広く取り過ぎていたことに気付きました。非常に有意義なテストになりました。

コンテストの本番では、もう少しポータブルな関数で PBT できると良さそうです。
