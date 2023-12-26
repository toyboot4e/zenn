---
title: "[4] 1. RealWorld"
---

# 注. ネタバレ

本節は以下の記事のネタバレとなり得ます。とても貴重な体験ができるので、先に記事をご覧ください。

https://zenn.dev/mod_poppo/articles/unsafeperformio

## 以降、ネタバレを含みます

---
---
---
---
---
---
---
---
---
---

---
---
---
---
---
---
---
---
---
---

---
---
---
---
---
---
---
---
---
---

『[すごい Haskell](https://booth.pm/ja/items/1577541)』 P333 `NOTE` に本節で伝えたい内容の要約が載っています。すなわち `IO` モナドは `State` モナドにおいて現実世界を状態とし、 `RealWorld -> RealWorld` のように現実世界を写し取るという幻想を副作用の表現としています。

言い換えると、本節では [IO モナドと副作用 - Haskell-jp #おまけ: IOモナドの実装](https://haskell.jp/blog/posts/2020/io-monad-and-sideeffect.html#%E3%81%8A%E3%81%BE%E3%81%91-io%E3%83%A2%E3%83%8A%E3%83%89%E3%81%AE%E5%AE%9F%E8%A3%85) で触れられる「『Haskell の IO モナドは、現実世界を状態にする State モナドだ』という主張」を直感的に理解するための確認をします。

:::message alert
本節も憶測混じりで書いてあります。ご指摘は大歓迎です。
:::

# 可変操作の表現

## `State` モナド

`State` モナドが『状態付き計算』であるとは、ユーザ目線で考えるといい当て妙です。 `State` モナドが持つ文脈に不変データ型を乗せると、可変データのように扱えます:

```hs
example1 :: Int -> (Double, Int)
example1 s0 = (`runState` s0) $ do
  -- ----------------> ここから
  modify' (+ 1)
  modify' (+ 1)
  modify' (+ 1)
  return 0.0
  -- <---------------- ここまで全体で @State Int Double@
  --                   (@Int -> (Double, Int)@ の wrapper)
  -- @runState@ によって @Int@ を与えて実行する
```

```
ghci> example1 39
(0.0,42)
```

> 上記の例を、以下 playground にて desugaring してみました: https://play.haskell.org/saved/WtZhkdQ0 。 `do` 記法の各行はポイントフリースタイルで書かれたモナディック関数であること、 `State` モナドは `s -> (a, s)` の wrapper であること、 `modify` は引き継がれる状態の加工を目的とした関数であり `a` = `()` であることなどが確認できます。

## `ST` モナド

`ST` モナドの表現は `State` モナドの表現とほぼ同一である、と言うと違和感を覚えるでしょうか。その原因をいくつか取り払ってみたいと思います。

まずはデータ表現に関して、 [GHC.ST] を覗いてみると、 `ST` モナドの実装はほぼ `State` モナドと同一です:

```hs
newtype ST s a = ST (STRep s a)
type STRep s a = State# s -> (# State# s, a #)
```

内部実装に関して、 `ST` モナドに関連した操作の背後には、 `writeMutVar#` のような真に mutable なネイティブ実装がある点が `State` モナドと異なります。しかしそれらは [prim-ops] の中で実装されており、表面部分の Haskell は関知しません。除外して考えます。

`ST` モナドと `State` モナドの API を比較してみます。 `State` モナドの例は以下とします:

```hs
stateExample :: Int -> Int
stateExample s0 = (`execState` s0) $ do
  -- ----------------> ここから
  modify' (+ 1)
  modify' (+ 1)
  modify' (+ 1)
  return ()
  -- <---------------- ここまで全体で @State Int ()@
  --                   (@Int -> ((), Int)@ の wrapper)
  -- @runState@ によって @Int@ を与えて実行する
```

似たコードを `ST` モナドを使って書くとしたら、次のようになるでしょう:

```hs
stExample :: Int -> Int
stExample s0 = runST $ do
  -- ----------------> ここから
  ref <- newSTRef s0
  modifySTRef' ref (+ 1)
  modifySTRef' ref (+ 1)
  modifySTRef' ref (+ 1)
  readSTRef ref
  -- <---------------- ここまで全体で @ST s Int@
  --                   (@s -> (Double, s)@ の wrapper)
  -- これに @runST@ によって @s@ を与えて実行する
```

ここで `stExample` の不思議な点は、文脈に乗せた型 `s` (state thread) とは何か、変数 `ref` とは何かということです。これらをアロケータ (コンテナ) および識別子であると捉えれば、 `State` モナドにおいても似た機能を実現できることに気付きます。

![STRef](/images/seriously-haskell/STRef.png =520x)
*`STRef` と `s` (state thread) のメンタルモデル
`s` (state thread) はコンテナであり更新されていく。 `STRef` は定数だが、参照先の値が更新される。*

たとえば `State` モナドの文脈に [`Map`] を乗せた場合、次のように `STRef` を模したコードが書けます。 `stExample` とそっくりですし、 `s` (state thread) は [`Map`] のすごいやつ (immutable で万能なアロケータ) であると解釈できます:

```hs
myArenaExample :: Int -> Int
myArenaExample s0 = (`evalState` M.empty) $ do
  ref <- newIntRef s0
  modifyIntRef' ref (+ 1)
  modifyIntRef' ref (+ 1)
  modifyIntRef' ref (+ 1)
  readIntRef ref
```

> 実装例 (playground): https://play.haskell.org/saved/YHA2il9D

以上の通りメンタルモデルを更新すると、 `ST` モナドの表現は、直感的にはほぼ `State` モナドと同一であると捉えられるのではないでしょうか。

## `IO` モナド

`IO` モナドの表現もまた、ほぼ `State` モナドです。状態のトークンは `RealWorld` 型となっています:

```hs
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

`IO` モナドの文脈の元では、 `ST` モナドのように可変変数を扱えることに加えて、標準入出力なども操作可能です。これを `State` モナドと同様に捉えるとしたら、文脈である `RealWorld` の中に標準入出力が含まれていると考えれば良いでしょう。さらに外界すべてが `RealWorld` の中にあると捉えれば、我々の `Hello, world!` は、この世のすべてを新しい世界へ写し替えた結果、ターミナルの出力を生み出すという表現であるとも解釈できます。

![IO モナドと RealWorld](/images/seriously-haskell/IOMonad.png =360x)
*Haskell の外に世界があるのではない、 RealWorld は Haskell が持つデータ領域上の 1 点に過ぎない ?!*

これならばいかなる副作用を書いたとて、関数の出力を次の関数の入力に繋いだだけのことであり、純粋性は保たれています。『ループ処理』の章で扱った状態更新と同じ表現です。

なお `IO`, `ST` は `State` モナドとは異なり、コンパイラから特別扱いされているようです。記事『[IO モナドと副作用](https://haskell.jp/blog/posts/2020/io-monad-and-sideeffect.html)』では `IO` が `State` モナドとは異なる特別な制約を課されることが説明されています。また [続くといいな日記 – GHC IO モナドの中身](https://mizunashi-mana.github.io/blog/posts/2019/05/ghc-io-inside/) においては `IO` の概要と GHC における扱いが紹介されています。

この記事の内容は、こうした投稿の 3 歩手前ぐらいの所かと思います。『すごい Haskell』でも言及されていた内容ではありますが、最近やっと理解したつもりで感動したため紹介しました。

[`State`]: https://www.stackage.org/haddock/lts-21.7/mtl-2.2.2/Control-Monad-State-Strict.html#t:StateT
[`>>=`]: https://www.stackage.org/haddock/lts-21.7/transformers-0.5.6.2/src/Control.Monad.Trans.State.Strict.html#local-6989586621679084069
[`Map`]: https://www.stackage.org/haddock/lts-21.7/containers-0.6.7/Data-Map-Strict.html
[`IntMap`]: https://www.stackage.org/haddock/lts-21.7/containers-0.6.7/Data-Map-Strict.html
[GHC.ST]: https://hackage.haskell.org/package/base-4.17.1.0/docs/GHC-ST.html
[prim-ops]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/prim-ops

