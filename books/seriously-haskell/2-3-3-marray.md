---
title: "[2-3] 3. MArray の使い方"
---

[`MArray`] は型が複雑で、エラーが出ると泥沼にはまります。エラー文が真の原因を表しているとは限らず、意外と単純な原因に起因するかもしれません。よって特に `ST` モナドに関連した代表的なエラーと原因、対処方法を紹介します。

なお型で詰まった際には、 [haskell-jp slack](https://haskell.jp/signin-slack.html) で質問するのがおすすめです。以下で紹介する例も、過去に僕が haskell-jp で質問し、手厚く助けて頂けたためにコンパイルできた関数の類型です。

# 例 1. [`runSTUArray`] で 1 次元累積和を計算する

前章で 1 次元累積和を計算する関数を作りました:

```hs
-- | 1 次元の累積和配列を作成する。
{-# INLINE csum1D #-}
csum1D :: (IArray a e, Num e) => Int -> [e] -> a Int e
csum1D n = listArray (0, n - 1) . L.scanl' (+) 0
```

同じ計算を [`runSTUArray`] によって実装してみます。 [`STUArray`] を使う意味はまったくありませんが、単純な例で型に集中しましょう。

## 1-1. `Int` 列の累積和を計算する

配列やモナドの import は以下とします:

```hs
import Control.Monad                -- `forM_` など
import Control.Monad.ST             -- `ST` モナド
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO                -- `IOArray`, `IOUArray` (`MArray` のインスタンス)
import Data.Array.MArray            -- 可変配列の API (型クラス `MArray`)
import Data.Array.ST                -- `STArray`, `STUArray` (`MArray` のインスタンス)
import Data.Array.Unboxed (UArray)
```

[`runSTUArray`] によって累積和の配列を生成します。ここで `ST` (state thread) モナドの理解としては、 `do` 以下で可変変数を扱うことができるイメージで十分だと思います:

```hs
mutCSum1 :: Int -> [Int] -> UArray Int Int
mutCSum1 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    acc <- readArray arr i
    writeArray arr (i + 1) $! acc + dx

  return arr
```

実行します ([playground](https://play.haskell.org/saved/z70DuJfe)):

```hs
main :: IO ()
main = do
  let csum1 = mutCSum1 3 [1 .. 3]
  print $ elems csum1
```

```txt:出力
[0,1,3,6]
```

無事、累積和を計算できました。

## 1-2. 任意の `Num e` に対して累積和を計算する

配列に乗せるデータ型を `Int` から任意の `Num e` に拡張してみます:

```hs
mutCSum2 :: (Num e, MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
mutCSum2 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  return arr
```

コンパイルしてみると、長いエラーが出ました:

```hs:コンパイル結果
/home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:63:13: error:
    • Could not deduce (MArray (STUArray s0) e (ST s0))
      from the context: (Num e, MArray (STUArray s) e (ST s))
        bound by the type signature for:
                   mutCSum2 :: forall e s.
                               (Num e, MArray (STUArray s) e (ST s)) =>
                               Int -> [e] -> UArray Int e
        at /home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:63:13-79
      The type variable ‘s0’ is ambiguous
      Potentially matching instances:
        instance MArray (STUArray s) Bool (ST s)
          -- Defined in ‘Data.Array.Base’
        instance MArray (STUArray s) Char (ST s)
          -- Defined in ‘Data.Array.Base’
        ...plus four others
        ...plus 11 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the ambiguity check for ‘mutCSum2’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        mutCSum2 :: (Num e, MArray (STUArray s) e (ST s)) =>
                    Int -> [e] -> UArray Int e
   |
63 | mutCSum2 :: (Num e, MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
   |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

ほげええええ (️️️ ´ᾥ` )

エラー文では [AllowAmbiguousTypes] が提案されています。制約に現れる型 `s` が関数の引数・戻値の型に現れないため、この提案が成されたようです:

https://qiita.com/aiya000/items/7e2efc378e9fabb1af32

こういった関数は Rust ではコンパイルエラーとなります。 Haskell においては非常に型が柔軟なため、言語拡張を有効にすればよしなにコンパイルしてくれるのでしょう。ひとまず言語拡張の [AllowAmbiguousTypes] を有効化した上で、再度コンパイルしてみます。引き続きエラーとなりますが、エラー文には変化がありました:

```diff hs
+ {-# LANGUAGE AllowAmbiguousTypes #-}
```

```hs
/home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:64:10: error:
    • Could not deduce (MArray (STUArray s1) e (ST s1))
        arising from a use of ‘newArray’
      from the context: (Num e, MArray (STUArray s) e (ST s))
        bound by the type signature for:
                   mutCSum2 :: forall e s.
                               (Num e, MArray (STUArray s) e (ST s)) =>
                               Int -> [e] -> UArray Int e
        at /home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:62:1-79
    • In a stmt of a 'do' block: arr <- newArray (0, n) 0
      In the second argument of ‘($)’, namely
        ‘do arr <- newArray (0, n) 0
            forM_ (zip [0 .. n - 1] xs) $ \ (!i, !dx) -> do ...
            return arr’
      In the expression:
        runSTUArray
          $ do arr <- newArray (0, n) 0
               forM_ (zip [0 .. n - 1] xs) $ \ (!i, !dx) -> do ...
               return arr
   |
64 |   arr <- newArray (0, n) 0
   |          ^^^^^^^^
```

このエラーが真の原因を表しており、型パラメータの `s` が `runSTUArray` における別の `s1` と合わないと表示されています。型制約における `s` がどの `s` であるかと言えば、任意の `s` であって欲しいわけです。 Rust でライフタイムに触れているときにもありがちなエラーです。

実際 [`runSTUArray`] の制約を確認すると、 `forall s.` が書かれています。 `ST` モナドの言葉を借りるならば、 [`runSTUArray`] とは『任意の state thread が与えられた際に `STUArray` を生み出す関数』を実行し、出来上がった `STUArray` を凍結して `UArray` にして返してくれる関数ということになります:

```hs
runSTUArray :: (forall s. ST s (STUArray s i e)) -> UArray i e
```

ただし今書いている関数においては、型制約として `forall s.` を書きたいです。この場合は言語拡張の [QuantifiedConstraints] を有効化する必要があります。またコンパイルエラーの真の原因を取り除いたため、 [AllowAmbiguousTypes] は不要になります:

```diff hs
- {-# LANGUAGE AllowAmbiguousTypes #-}
+ {-# LANGUAGE QuantifiedConstraints #-}
```

> [QuantifiedConstraints] は、ちょうど Rust の [HRTB] に対応するような機能だと思います。こんな簡単なコードで隠し機能みたいなものが必要になって良いのでしょうか。

最後に `mutCSum2` の型制約に `forall s.` を追加して、無事コンパイルできました:

```diff hs
- mutCSum2 :: (Num e, MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
+ mutCSum2 :: (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
mutCSum2 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  return arr
```

`Num e` まで拡張できたので、今度は `Int` ではなく `Double` を指定して累積和を計算してみます ([playground](https://play.haskell.org/saved/FWspimeI)):

```hs
main :: IO ()
main = do
  let csum2 = mutCSum2 3 [1.0 :: Double .. 3.0]
  print $ elems csum2
```

```hs:出力
[0.0,1.0,3.0,6.0]
```

[`MArray`], 手強くないですか。振り返ってみれば型制約に `forall s.` を書くというだけの話ですが、最初のエラー文から一発でこれを見抜くには経験が必要だと思います。

では次のエラーに向かいます。

# 例 2. `runST` で可変配列を使う際は型ヒントを与える

これまでは [`runSTUArray`] を使っていましたが、計算過程に可変配列を使いつつも、最後は配列以外のデータ型を返却したい場合があります。この場合は [`runST`] を使います。

手頃な例が思い浮かばないので、累積和を計算した後に、結局和だけを返す関数を作ってみます:

```hs
mutSum1 :: (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> e
mutSum1 n xs = runST $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  readArray arr n
```

これは派手なコンパイルエラーになります。 `newArray` は型クラス `MArray` の関数ですが、どのインスタンスを使用するか推論できなくなるためです。

対処法としては、明示的に型を書けばコンパイルできるようになります:

```hs
  arr <- newArray (0, n) 0 :: ST s (STUArray s Int e)
```

あるいは何もしない関数に通して型を付けることもできます [^1]:

```hs
asSTU :: ST s (STUArray s i e) -> ST s (STUArray s i e)
asSTU = id

-- | 累積和を計算した後に、結局和だけを返す
mutSum1 :: forall e. (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> e
mutSum1 n xs = runST $ do
  arr <- asSTU $ newArray (0, n) 0
  -- ...
```

そもそも `newSTUArray` のような関数を作っておけば、 `newArray` を `newSTUArray` に置き換えるだけで対応できます。本来は `Data.Array.ST` モジュールがこの関数をエクスポートしてくれるのが筋でしょう。

いずれかの方法を用いて、リストの和を計算できました ([playground](https://play.haskell.org/saved/of78GVrV)):

```hs
main :: IO ()
main = do
  let sum1 = mutSum1 3 [1.0 :: Double, 2.0, 3.0]
  print sum1
```

```txt:出力
6.0
```

# Tips

## 任意の `MArray a e m` を書き換える関数を作る

`MArray` に対して働きかける関数を作るのは簡単で、 [`writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m () `][`writeArray`] と同じように型を書けば良いです。ただし先の例で見たとおり、呼び出し元で適切な型制約を書かなければエラーとなります。

## `f . runST $ do ..` はコンパイルエラー

`runST` に関連して、以下のコードはコンパイル可能です:

```hs
stExample :: Int
stExample = succ $ runST $ return 0
```

しかし以下はコンパイルできません:

```hs
stExample :: Int
stExample = succ . runST $ return 0
```

```hs
/home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:87:20: error:
    • Couldn't match type: forall s. ST s Int
                     with: m0 a0
      Expected: m0 a0 -> Int
        Actual: (forall s. ST s Int) -> Int
    • In the second argument of ‘(.)’, namely ‘runST’
      In the first argument of ‘($)’, namely ‘succ . runST’
      In the expression: succ . runST $ return 0
   |
87 | stExample = succ . runST $ return 0
   |                    ^^^^^
```

実は `$` の方に特殊な推論のルールが設けられているようです。この件とも関連しているかもしれません……が、ひとまず問題が解ければ良いということで保留します。

https://stackoverflow.com/a/9469942

## クロージャ (?) には型を書かなくて良い

可変配列に対する複雑な操作を関数に分ける際に、可変配列を引数に取るとやや冗長であると思います。 `let` で関数定義すれば、 `newArray` で作った配列がスコープに入るため、引数から省略できます。型も推論されるため書かずに済みます。

## 無用なクロージャに注意

次の関数は、 `arr` をキャプチャする未使用のクロージャ `test` を定義しています。コンパイルできるでしょうか:

```hs
eg2 :: Int -> Int
eg2 n = runST $ do
  !arr <- asSTU $ newArray (0 :: Int, n - 1) (0 :: Int)
  let test i = readArray arr i
  readArray arr 0
```

できました。ところが [TypeFamilies] 言語拡張を有効にしていた場合、コンパイルエラーが発生します ([playground]()) 。

```hs
{-# LANGUAGE TypeFamilies #-}
```

```hs
/home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:111:16: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘readArray’
      prevents the constraint ‘(MArray
                                  (STUArray s) Int m0)’ from being solved.
      Relevant bindings include
        test :: Int -> m0 Int
          (bound at /home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:111:7)
        arr :: STUArray s Int Int
          (bound at /home/tbm/dev/hs/seriously-haskell/examples/MArray.hs:110:4)
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      Potentially matching instance:
        instance MArray (STUArray s) Int (ST s)
          -- Defined in ‘Data.Array.Base’
    • In the expression: readArray arr i
      In an equation for ‘test’: test i = readArray arr i
      In the second argument of ‘($)’, namely
        ‘do !arr <- asSTU $ newArray (0 :: Int, n - 1 :: Int) (0 :: Int)
            let test i = readArray arr i
            readArray arr 0’
    |
111 |   let test i = readArray arr i
    |                ^^^^^^^^^
```

```txt:解読が終わるまで、流れる SL をご観覧ください
                           (@@) (  ) (@)  ( )  @@    ()    @     O     @     O      @
                      (   )
                  (@@@@)
               (    )

             (@@@)
          ====        ________                ___________
      _D _|  |_______/        \__I_I_____===__|_________|
       |(_)---  |   H\________/ |   |        =|___ ___|      _________________
       /     |  |   H  |  |     |   |         ||_| |_||     _|                \_____A
      |      |  |   H  |__--------------------| [___] |   =|                        |
      | ________|___H__/__|_____/[][]~\_______|       |   -|                        |
      |/ |   |-----------I_____I [][] []  D   |=======|____|________________________|_
    __/ =| o |=-~~\  /~~\  /~~\  /~~\ ____Y___________|__|__________________________|_
     |/-=|___|=    ||    ||    ||    |_____/~\___/          |_D__D__D_|  |_D__D__D_|
      \_/      \O=====O=====O=====O_/      \_/               \_/   \_/    \_/   \_/
```

分かりました。 [TypeFamilies] 拡張は [`Data.Vector.Unboxed`] の型クラス `Unbox` を実装する際などに使用するため有効化していました。なぜエラーが出るのかは分かりませんが、上手く推論が働くように調整すれば良いようです。

対策としては、 `arr` を明示的に引数として受け取るか:

```hs
eg2 :: Int -> Int
eg2 n = runST $ do
  !arr <- asSTU $ newArray (0 :: Int, n - 1 :: Int) (0 :: Int)
  let test arr_ i = readArray arr_ i
  readArray arr 0
```

作ったクロージャを `runST` の文脈の中で使ってあげると正しく推論が働くようです:

```hs
eg2 :: Int -> Int
eg2 n = runST $ do
  !arr <- asSTU $ newArray (0 :: Int, n - 1 :: Int) (0 :: Int)
  let test i = readArray arr i
  test 1
  readArray arr 0
```

> あるいは正しく `s` を書ければ良いのかもしれません。

複雑な関数を書いているときには、このように真の原因がコンパイルエラーとして露出しません。ユーザーの認知を超えた謎のエラーが溢れ出るのみです。エラー再現の最小構成を探してみたり、予めエラーのパターンを認識しておくしかないかと思います。

# まとめ

[`MArray`] の主なコンパイルエラーを確認しました。型制約に `forall` を書きたいときは言語拡張の [QuantifiedConstraints] を有効化する必要があり、 [TypeFamilies] が有効な場合は推論が上手く働かない場合があることを確認しました。エラーで詰まったときには確認してみてください。

[`array`]: https://www.stackage.org/lts-21.7/package/array-0.5.4.0
[`IArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IArray.html
[`UArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-Unboxed.html#t:UArray

[`STUArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-ST.html#t:STUArray
[`IOUArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-IO.html#t:IOUArray

[`Data.Array`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-Array.html
[`Data.UArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-UArray.html

[`MArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-MArray.html
[`Data.Array.ST`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-ST.html
[`runSTUArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-ST.html#v:runSTUArray
[`writeArray`]: https://www.stackage.org/haddock/lts-21.7/array-0.5.4.0/Data-Array-MArray.html#v:writeArray

[`Data.Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html
[`Ix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Ix.html#t:Ix

[`vector`]: https://www.stackage.org/lts-21.7/package/vector-0.13.0.0
[`Data.Vector.Unboxed`]: https://www.stackage.org/haddock/lts-21.7/vector-0.13.0.0/Data-Vector-Unboxed.html

[`primitive`]: https://www.stackage.org/lts-21.7/package/primitive-0.8.0.0

[HRTB]: https://doc.rust-lang.org/nomicon/hrtb.html

[AllowAmbiguousTypes]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html
[QuantifiedConstraints]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/quantified_constraints.html
[TypeFamilies]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html

[`runST`]: https://www.stackage.org/haddock/lts-21.7/base-4.17.1.0/Control-Monad-ST-Safe.html#v:runST

[^1]: EDPC - F の提出などで見た方法です。

