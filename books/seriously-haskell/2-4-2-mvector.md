---
title: "[2-4] 2. MVector の使い方"
---

この章をご覧になってしまわれましたか。私、力尽きております。

[`MVector`] の場合は型クラスが複数に分かれています。 Boxed / unboxed は [`Unbox`] 型クラスが担当し、 `IO` / `ST` モナドは [`PrimMonad`] によって抽象化できます。

[`MVector`] のコンパイルエラーは大体 `MArray` と同様ですから、 `MArray` の章をご覧ください。 `MArray` ができるなら [`MVector`] もできます！

[`MVector`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Mutable.html
[`PrimMonad`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Mutable.html#t:PrimMonad
[`Unbox`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Unboxed.html#t:Unbox

