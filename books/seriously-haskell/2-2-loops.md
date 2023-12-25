---
title: "[2-2] ループ処理"
---

AtCoder Beginner Contest (ABC) 序盤の問題が解けないときには、コードが煩雑になり、自分でも意味が分からなくなると思います [^1] 。いわゆる『スパゲッティ』が生まれる理由の一端は、手続き的な考えを元に Haskell を書く難しさにあります。 Haskell には可変変数が無い [^2] 上に、 `for`, `break`, `continue`, `return` といった制御構文もありません。手続き型プログラミングとは違ったパーツを使ってコードを組み立てる必要があります。

パターン認識で計算方法を思いつくようになるまでは、気長に待つ必要があります。演習あるのみですが、ときには休みつつ、気軽に誰かの認識を覗いてみたくなるものではないでしょうか。以下ではループ処理の具体例を示し、解法のパターンを増やして行きます。記憶の中の逆引き辞典を育て、自分の中の Haskell を豊かにして行きましょう。

# 1. 関数 `f` の繰り返し適用

## 1-1. 再帰による関数 `f` の `n` 回適用

再帰とはループの手動呼び出し、あるいは `goto` 文のようなものであると捉えられます。ただし再帰呼出し時に引数の値を更新できます。

:::message
いい加減な理解かもしれませんが、ここでは問題が解ければ良しとさせてください。
:::

n 回の繰り返し処理とは、手続き型言語における `for` ループそのものです。 Haskell においては再帰で表現するのが自然で、実行効率も良いです:

```hs
-- | >>> times 3 (* 2) (1 :: Int)
-- 8
times :: Int -> (a -> a) -> a -> a
times n f = inner 0
  where
    inner i !s
      | i >= n = s
      | otherwise = inner (i + 1) $! f s
```

`for` ループを再帰に置き換えて嬉しいことは、可変変数が無くなったことです。可変変数が無くなると、プログラムの基礎的な表現が変わります。たとえば `times` 関数を Rust で書き直した場合は、以下のように `x` を mutable にするのが自然です ([playground](https://play.rust-lang.org/?version=nightly&mode=debug&edition=2024&gist=752b4ab8c723de12922337969d2cdd22)) 。これは Haskell 版の `times` とは少し表現が異なります:

```rust
fn times<T>(n: usize, f: impl Fn(&T) -> T, mut x0: T) -> T {
    let mut x = x0;
    for i in 0..n {
        x = f(&x);
    }
    x
}
```

Haskell を書いていると、このような対比を観察する機会が繰り返し訪れます。手続き的な制御構文を失ったまま、関数の出力を関数の入力に繋ぐというある種原始的な表現によって、どれほど多くのことを記述できるのでしょうか。 Haskell を書いて実験して行きましょう。

## 1-2. [`foldl'`]

[`foldl'`] など『畳み込み』の関数は、逐次状態を更新しつつデータ列を処理する関数です:

[`foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b`][`foldl'`]

関数の `n` 回適用を畳み込みで書けば以下のとおりです。 `[0 .. n - 1]` というデータ列はそのまま捨てていますが、カウンタとして機能しています:

```hs
import Data.List qualified as L

times2 :: Int -> (a -> a) -> a -> a
times2 n f x0 = L.foldl' (const . f) x0 [0 :: Int .. n - 1]
             -- L.foldl' (\x _ -> f x) x0 [0 :: Int .. n - 1]
```

なお本書ではポイントフリースタイルを積極的に使います。人によってスタイルの違いがあるとは思いますが、僕にとってはこれが Haskell の一番楽しい所なので……！

## 1-3. [`until :: (a -> Bool) -> (a -> a) -> a -> a `][`until`]

不特定回数の関数適用であって、対象の値を打ち切り条件に用いる場合は [`until`] がハマります:

```hs:GHC.Base.hs
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x | p x          = x
         | otherwise    = go (f x)
```

[`until`] を使って『$2^n \ge x$ を満たす最小の $2^n$』を求める関数を作ってみます:

```hs
ghci> let f x = until (>= x) (* 2) (1 :: Int)
ghci> f 6
8
```

同じ関数を手続き的に書くと、可変変数と `while` ループの組み合わせになります。同じ計算に見えるでしょうか:

```rust
fn f(x: usize) -> usize {
    let mut s = 1;
    while s < x {
        s *= 2;
    }
    s
}
```

なお打ち切り条件に他の変数が絡む場合は、タプルに状態を載せることになります。こうした場合では、 [`until`] よりも再帰を使った方が素直になると僕は感じます:

```hs
import Data.BiFucntor (both)

-- 関数の n 回適用を @until@ で実装した場合:
times' :: Int -> (a -> a) -> a -> a
times' n f s0 = snd $ until ((== n) . fst) (bimap succ f) (0 :: Int, s0)
```

> `bimap` は [`Data.Bifunctor`] の関数です。

## 1-4. [`iterate :: (a -> a) -> a -> [a]`][`iterate`]

[`iterate`] は関数適用を繰り返し無限リストを作る関数です:

```hs:GHC.List.hs
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
```

[`iterate`] と `take` や `takeWhile` を組み合わせることで、関数の繰り返し適用の計算過程をリストとして残すことができます:

```hs
ghci> -- times と同様に:
ghci> take 4 $ iterate (* 2) (1 :: Int)
[1,2,4,8]
ghci> -- `until` 似の計算 (ちょっと違って要素が 1 つ減りますが):
ghci> takeWhile (<= 6) $ iterate (* 2) (1 :: Int)
[1,2,4]
```

<!-- Rust で言えば [`repeat_with`](https://doc.rust-lang.org/std/iter/fn.repeat_with.html) が [`iterate`] に対応すると思います。 -->

`n` 番目の要素のみが必要な場合は、 `!!` 演算子で抽出できます:

```hs
ghci> iterate (* 2) (1 :: Int) !! 3
8
```

しかし [`iterate`] は時に TLE を引き起こすほど低速です (ベンチマークの章で確認します) 。計算過程が必要無い場合は、再帰や畳み込みを使いましょう。

## 1-5 [`unfoldr :: (b -> Maybe (a, b)) -> b -> [a]`][`unfoldr`]

[`unfoldr`] 関数の定義は list fusion の最適化が絡むため難解です。振る舞いとしては、次の `myUnfoldr` と同一です:

```hs
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f = inner
  where
    inner b = case f b of
      Nothing -> []
      Just (a, b') -> a : inner b'
```

たとえば数値を 10 進数表記の各桁に分解する関数が欲しいとします:

```hs
ghci> -- 欲しい関数の振る舞い:
ghci> digits10 4321
[1,2,3,4]
```

手慣れた再帰関数で実装すれば、次の通りです:

```hs
digits10 :: Int -> [Int]
digits10 = f
  where
    f 0 = []
    f x = r : f q
      where
        -- q: quotient (商), r: remainder (あまり)
        (!q, !r) = x `divMod` 10
```

[`unfoldr`] で書き直せば以下のようになります:

```hs
import Data.List qualified as L

digits10' :: Int -> [Int]
digits10' = L.unfoldr f
  where
    f 0 = Nothing
    f x = Just (r, q)
      where
        (q, r) = x `divMod` 10

-- 関数 `f` は計算を停止する場合に `Nothing` を返し、
-- 計算を続行する場合は `Just (リストに追加する値、次の状態)` を返します
```

再帰関数を用いた実装においては、関数 `f` が `:` 演算子に依存していました。 [`unfoldr`] においては関数 `f` が構築するデータ型と無関係になるために、任意のデータ型の [`unfoldr`] を使用することができるようになります。

たとえば上記 `digits10'` を `vector` 版に書き換えてみます (`vector` は後の章で紹介します) 。 [`unfoldr`] 関数のみを置き換えれば、関数 `f` はまったく同じ定義のまま使用できます:

```hs
import Data.Vector.Unboxed qualified as U

digits10'U :: Int -> U.Vector Int
digits10'U = U.unfoldr f
  where
    f 0 = Nothing
    f x = Just (r, q)
      where
        (q, r) = x `divMod` 10
```

他の例として `bytestring` パッケージの `readInt :: ByteString -> Maybe (Int, ByteString)` は、リストの `unfoldr` にも [`vector`] の `unfoldr` にも使用できます:

```hs
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Data.Vector.Unboxed qualified as U

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsU :: IO (U.Vector Int)
intsU = U.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
```

特に [`vector`] においては `:` 演算子のように $O(1)$ でデータを追加する方法が存在しないため、 `unfoldr` による配列生成が必須です。 `unfoldr` のアイデアが実際役に立つことが確認できたと思います。

なお Rust で言えば [`successors`](https://doc.rust-lang.org/std/iter/fn.successors.html) というイテレータのメソッドが [`unfoldr`] に対応すると思います。どのデータ型にも [`collect`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect) できますね。

その他に [`unfoldr`] で表現できるものとしては、整数列から整数のチャンク列への変換、 `Int` を bit 集合とみなした場合のべき集合の列挙、尺取り法などがあります。

# 2. リストの走査

リストの走査は関数の繰り返し適用の 1 種と見れますが、観点の違いから項を分けました。

## 2.1. 再帰によるリスト走査

再び再帰の登場です。再帰とパターンマッチでリストを走査する場合、最も表現力が高くなると思います:

- 状態の更新  
  再帰関数の引数を状態の替わりにすることができます。

- 打ち切り (`break`)  
  再帰呼び出しを止めればループ処理を打ち切ることができます。

- スキップ (`continue`)  
  何もせず再帰呼び出しすれば要素をスキップできます。

再帰は表現力が高い分だけ、コードの意図が不明確になるかもしれません。 `break`, `continue` に相当する機能が必要無い場合は、畳み込みや `mapAccumL` に置き換えるのがおすすめです。 `unfoldr` に置き換えられることもあります。

## 2-2. 畳み込み (`foldl'`)

再び畳み込みの登場です。これは逐次状態を更新しつつデータ列を処理する関数でした:

[`foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b`][`foldl'`]

たとえば和の計算は畳み込みで表現できます:

```hs
sum' :: [Int] -> Int
sum' = L.foldl' step s0
  where
    s0 = 0 -- s0: initial state
    step !acc x = acc + x -- acc: accumulator.. というか accumulated value (?)
```

```hs
ghci> sum' [1, 2, 3]
6
```

畳み込みを使うと、ナップサック問題を始めとして非常に多くの問題を解くことができます。手続き型プログラミングのループが畳み込みに見えることも多いでしょう。

## 2.3. 状態付き map ([`mapAccumL`])

[`mapAccumL`] は状態付きの `map` です。稀に使うこともあるので紹介します:

[`mapAccumL :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)`][`mapAccumL`]

累積和から元の数列を復元する、逆累積和の計算を実施してみます。 (`zipWith` で同じ計算ができるため、あまり良くない例ですが):

```hs
invCSum :: [Int] -> (Int, [Int])
invCSum = mapAccumL step s0
  where
    s0 = 0 :: Int
    step lastX x = (x, x - lastX)
```

```hs
ghci> -- 累積和
ghci> L.scanl1 (+) [1, 2, 3]
[1,3,6]
ghci> -- 逆累積和
ghci> invCSum $ L.scanl1 (+) [1, 2, 3]
(6,[1,2,3])
```

状態付き計算と言えば `State` モナドですが、 `mapM` と `State` モナドを組み合わせることで状態付き `map` 関数が出来上がり、それはまるっきり `mapAccumL` です:

```hs
-- ※ mapAccumL とは若干シグネチャが異なります
mapAccumL' :: (a -> s -> (b, s)) -> s -> [a] -> ([b], s)
mapAccumL' step s0 xs = runState (mapM (state . step) xs) s0
```

```hs
ghci> -- あらためて逆累積和
ghci> mapAccumL' (\x lastX -> (x - lastX, lastX)) (0 :: Int) [1 :: Int, 3, 6]
(6,[1,2,3])
```

特に `vector` パッケージにおいては `mapAccumL` 関数が存在しないため、 `State` モナドと `mapM` の組み合わせを使うのが自然かと思います。 `State` モナドは他の章で解説予定です。

# Tips

## キューが無い件とどう向き合うか

標準的な Haskell にはキューがありません。複数のデータを出力する際は、基本的にリストを使います。たとえば:

- `map` や `mapAccumL` を使う

- `unfoldr` を使う

- 再帰関数で `:` を使って出力を作る

- リストを stack として使い、後で `reverse` する

リスト以外を使う方法としては:

- [`Data.Sequence`] をキューとして使う  
  使いづらいためおすすめできません。

- 可変配列をキューとして使う ([`cojna/iota`] の [`Data.Buffer`] など)  
  なんだかんだで可変配列を使うとコードが書きやすい上に、実行効率も良くなります。特にグラフ探索などでおすすめです。グラフ探索の章で改めて紹介します。

## `exit`

Haskell には手続き型言語における `return` に対応する制御構文がありませんが、 `exit` に対応する [`exitSuccess`] が [`System.Exit`] モジュールにあります。プロセスを終了することで強制的に脱出できます。緊急性が重要な場合、エッジケースに気付き修正を追加する場合などに切れる手札です [^3]:

```hs
main :: IO ()
main = do
  s <- getLine
  -- ..

  -- 簡単なケースでは即終了
  when isTrivialCase $ do
    print (-1 :: Int)
    exitSuccess

  -- 複雑なケース
```

ところで 2023 年の AtCoder では Emacs Lisp を使うことができます。なんてこったい。 ELisp も Haskell と同様に、手続き的な `return` がない言語です。そのためしばしば `kill-emacs` でプロセスを終了する様子が見られます。彼らもシリアスに戦っています。

# まとめ

再帰を手動ループであると見て、関数の出力を関数の入力に繋ぐことで値の更新を表現しました。モナドを含め、今後一貫してこの方法でプログラムを記述することになると思います。関数型プログラミングを習得したという瞬間は、案外単純な再帰関数を書いた時となるかもしれません。

再帰を確認した後は、様々なループのパターンに相当する関数を確認し、その使い方を検討しました。手続き型言語においてはどのループも可変変数と制御構文によって表現できますが、 Haskell においてはループの関数が細分化されます。事前に関数の使い方を覚えていなければ、適切なループを書くために四苦八苦するかもしれません。とりわけループの出力を複数のデータ (リスト) にしたい場合が難しいと思います。

次章では配列の使い方を確認します。可変配列が使えるようになれば大抵の問題は解けますし、問題が解ければ今の解答をより良い形に洗練させていく余裕ができます。時にはプログラムの書き方がブレて納得の行くコードが書けないかもしれませんが、長い目で見れば、いずれは Haskell ならではというコードに落ち着いて行くかと思います [^4] 。

[`$!`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:-36--33-
[`iterate`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:iterate
[`until`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:until
[`unfoldr`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:unfoldr

[`foldl'`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:foldl-39-
[`mapAccumL`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:mapAccumL

[`groupBy`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:groupBy

[`Data.Bifunctor`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Bifunctor.html
[`bimap`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-Bifunctor.html#v:bimap

[`vector`]: https://github.com/haskell/vector

[`System.Exit`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/System-Exit.html
[`exitSuccess`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/System-Exit.html#v:exitSuccess

[`Data.Sequence`]: https://www.stackage.org/haddock/lts-21.7/containers-0.6.7/Data-Sequence.html

[`cojna/iota`]: https://github.com/cojna/iota
[`Data.Buffer`]: https://cojna.github.io/iota/Data-Buffer.html

<!-- https://github.com/quchen/articles/blob/master/build.md -->
<!-- [`build`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/GHC-Exts.html#v:build -->

[^1]: 『すごい Haskell』を読んで、すごい Haskell を生み出してしまった……
[^2]: `ST` や `IO` モナドを使えば可変変数を使えます。 RealWorld の章では可変操作など存在しないという解釈を紹介します。
[^3]: 僕は先週のコンテストにおいて 5 問中 2 問で `exitSuccess` を使っていました……
[^4]: Haskell は制約の強い言語で、書きやすさを求めている内に自然とそれらしくなると思います。僕の場合は、ポイントフリースタイルを多用する (ことが楽しい) という方針を得てからは Haskell の書き方が安定しました。

