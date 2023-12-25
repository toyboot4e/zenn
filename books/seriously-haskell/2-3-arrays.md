---
title: "[2-3] 📜 array の使い方"
---

配列を使えば、大抵の問題は解けるようになります。ついに B 問題で TLE しなくていい時代がやってくるのです (!?) 。 [^1] 長い道のりですが進んでいきましょう。

# 概観

この章では [`array`] を初めて使う人に向けて、主な API を紹介します。 [`Ix`] [^2], [`IArray`] および [`MArray`] という 3 つの型クラスです。

## 1. [`Ix`]

[`array`] パッケージは型クラス [`Ix`] を使って添字を抽象化しています。 [`Ix`] は n 次元添字を一般化するものであり、 [`Ix`] を実装する主なデータ型は `Int` および `Int` のタプルです。 [`Ix`] クラスの主な機能は、添字の成分から 0-based index (0, 1, 2 ..) への変換です。 [`index`] 関数は添字の範囲と添字の成分を引数に取り、 0-based index の通し番号を返します。これが内部的な 1 次元配列の添え字に使われるものと思います。

## 2. [`IArray`] (Immutable Array)

不変配列の API が型クラス [`IArray`] です。 [`IArray`] を実装するデータ型は、主に boxed/unboxed な 2 種類の配列です。パフォーマンスのため、可能な限り unboxed な配列 ([`UArray`]) を使います [^3] 。

| インスタンス | Boxed / Unboxed | 効率 | 保存できるデータ型                   |
|--------------|-----------------|------|--------------------------------------|
| [`Array`]    | Boxed           | x    | 任意のデータ型、特にタプルやリスト   |
| [`UArray`]   | Unboxed         | o    | `Int`, `Double`, `Bool`, `Char` など |

## 3. [`MArray`] (Mutable Arra)

可変配列の API が型クラス [`MArray`] です。 [`MArray`] を実装するデータ型は、モナドの種類 (IO/ST モナド) と boxed/unboxed かで 4 種類に分けられます。 `IO` モナドは `main` 関数の直下で使用できますから、横着したい時に便利です。 `ST` (state thread) モナドを使うと、区切られた一部のコードで可変変数を使用できます (純粋な文脈でも使用できて便利です) 。 やはりパフォーマンスのため、可能な限り unboxed なデータ型 ([`IOUArray`] および [`STUArray`]) を使います。

| インスタンス | Monad | Boxed / Unboxed |
|--------------|-------|-----------------|
| [`IOArray`]  | `IO`  | Boxed           |
| [`IOUArray`] | `IO`  | Unboxed         |
| [`STArray`]  | `ST`  | Boxed           |
| [`STUArray`] | `ST`  | Unboxed         |

# どの配列型を使うべきなのか

[`array`] パッケージだけで主な配列の型が 6 つもありました。概観で触れた通り、基本は unboxed な array ([`UArray`], [`IOUArray`], [`STUArray`]) を使います。可変性が必要無いならば [`UArray`] を使い、可変性が必要なら [`STUArray`] を使います。可変性は必要だが横着したいとき、使い捨てのコードを書くときは [`IOUArray`] を使います。

Unboxed な array に保存して使用できるデータ型は非常に限られており、タプルやリストなどを格納したい時は boxed な array ([`Array`], [`STArray`], [`IOArray`]) を使います。ここは Haskell のちょっと面倒くさいところだなと思います。

## [`array`] vs [`vector`] パッケージ

[`array`] 以外の配列のパッケージとしては [`vector`] があります。 [`array`] と比べて [`vector`] はパフォーマンスが良く、連続部分列のイテレートにも強いなど、取り回しの良い面があります。正直僕は [`array`] よりも [`vector`] がおすすめです。

しかし [`array`] はプレインストールされることもあって Haskeller の共通言語と言えます。また [`array`] における多次元の添字の扱い ([`Ix`] 型クラス) は [`vector`] にも転用できて役に立ちます。したがって [`vector`] を主に使う場合であっても、 [`array`] と [`vector`] を両方習得するのがおすすめです。

[`array`]: https://www.stackage.org/lts-21.7/package/array-0.5.4.0
[`IArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html
[`MArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-MArray.html
[`Array`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array.html
[`UArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-Unboxed.html
[`STUArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-ST.html#t:STUArray
[`IOUArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IO.html#t:IOUArray
[`STArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-ST.html#t:STArray

[`IOArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IO.html#t:IOArray
[`Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#t:Ix
[`Data.Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html
[`index`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#v:index

[`vector`]: https://www.stackage.org/lts-21.7/package/vector-0.13.0.0
<!-- [`massiv`]: https://github.com/lehins/massiv -->

[^1]: (T^T) (悲しい思い出)
[^2]: [`Ix`] は正確には [`array`] パッケージには属しません。 `base` パッケージの [`Data.Ix`] モジュールにあります。
[^3]: たとえばナップサック問題では boxed array を使うと TLE になるため、 unboxed な array を使うほかありません。

