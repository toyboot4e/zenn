---
title: "State ベースのパーサ"
---


# 標準入力をパースする

競技プログラミングでは入力値が標準入力として与えられるため、プレイヤーが自力でパースする必要があります。 [`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) の開発においては、簡単なパーサ ([`Util.hs`](https://github.com/toyboot4e/ac-library-hs/blob/2a5083aeca24896b9fe595edc0eb7f9e4cc6d8fd/verify/src/Util.hs)) を用意して [AtCoder Library Practice Contest](https://atcoder.jp/contests/practice2) の問題を解いて動作確認しました。パーサ作成のモチベーションと共に、パーサの内容を解説します。


# 入力の先読み

[Range Affine Range Sum](https://atcoder.jp/contests/practice2/tasks/practice2_k) の入力では、クエリ種別によって行のフォーマットが変わります。

-   クエリ種別 0: `0 l r b c`
-   クエリ種別 1: `1 l r`

いずれも 5 値のタプル、つまり [`Unbox`](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:Unbox) なデータ型にパースして `vector` に保存したいとします。そのため各行一単語目のクエリ種別 (`0` または `1`) を見て分岐する必要があります。しかし Haskell では単語単位で標準入力を取得する (手軽な) 方法が無く、細かくしても行単位 ([`getLine`](https://hackage.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString.html#v:getLine)) の取得となります。対策を考えます。


## リストを経由する方法

定型的なテンプレートで処理する方法として、一度リストとして取得してからタプルに変換できます。やや釈然としませんが、無難に動作します。

テンプレート部分は以下です:

```haskell
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

intList :: (HasCallStack) => IO [Int]
intList = L.unfoldr (BS.readInt . BS.dropSpace) <$> BS.getLine
```

パース部分では、次のように処理します:

```haskell
main :: IO ()
main = do
  (!_, !q) <- intList
  xs <- intList
  -- クエリを読み込む部分:
  qs <- VU.replicateM q $ do
    intList <&> \case
      [0, !l, !r, !b, !c] -> (0 :: Int, l, r, b, c)
      [1, !l, !r]  -> (1 :: Int, l, r, -1, -1)
      _ -> error "unreachable"
```


## バッファに入れて処理する方法

C++ の解答では `scanf` や `cin` を使い、単語単位で標準入力を読んでいるものが多いです:

```cpp
int t;
scanf("%d", &t);
if (t == 0) {
    int l, r, c, d;
    scanf("%d %d %d %d", &l, &r, &c, &d);
    seg.apply(l, r, F{c, d});
} else {
    int l, r;
    scanf("%d %d", &l, &r);
    printf("%d\n", seg.prod(l, r).a.val());
}
```

Haskell においても `scanf` / `cin` が欲しいと思うと、 [`Stete`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Strict.html) モナドを使うのが近いです。標準入力をバッファ (`Stete` モナド) に読み出して、単語単位で読み取っていきます。

パーサ型を以下とします:

```haskell
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)

type Parser = StateT BS.ByteString Maybe

-- | バッファから一単語読んで `Int` 型にパースする
intP :: Parser Int
intP = StateT $ BS.readInt . BS.dropSpace
```

一行読んでバッファに入れて、パーサで処理する関数が以下です。部分関数になりますが、 `fromJust` で値を取り出してしまいます:

```haskell
withLine :: (HasCallStack) => Parser a -> IO a
withLine f = fromJust . evalStateT f <$> BS.getLine
```

クエリの部分を次のようにパースできます:

```haskell
qs <- VU.replicateM q $ do
  withLine $
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,-1,-1) <$> intP <*> intP
      _ -> error "unreachable"
```

なお標準入力全体を [`Stete`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Strict.html) モナドに入れておけば、 `withLine` は省略可能です。


# パーサの合成

[ABC 385 - D](https://atcoder.jp/contests/abc385/tasks/abc385_d) では、やや複雑な入力が与えられました。特に $D_i, C_i$ の部分を抜き出すと、一単語目が方向 (`Char`) 、二単語目が移動量 (`Int`) となります:

```text
L 2
D 1
R 1
U 2
```

ここで `Char` のパーサを追加します:

```haskell
charP :: Parser Char
charP = StateT $ BS.uncons . BS.dropSpace
```

すると次のように `charP`, `intP` を組み合わせて `(Char, Int)` のパーサを作って使用できます:

```haskell
!movements <- U.replicateM m $ withLine ((,) <$> charP <*> intP)
```

これは格好いい！　しかもパーサの合成により、競技プログラミングで与えられるような入力はほぼすべて処理できることが分かります。


# まとめ

[`Stete`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Strict.html) モナドを作って簡易パーサを作成しました。初めて他の競技者がパーサの合成しているのを見たときは、かなりぐっと来たのを覚えています。実際に使用してみると、格好良さと実用性が両立したスタイルで良いと思います。

なおライブラリとしては、パーサ (`*P`) に [`MonadState`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#t:MonadState) を使ったほうが良さそうです。たとえば標準入力全体を `Stete` モナドに載せておけば、 `withLine` などを経由せず、直にパーサを呼び出しできるようになります。

```haskell
main' :: StateT BS.ByteString IO ()
main' = do
  {- .. -}

main :: IO ()
main = evalStateT main' =<< BS.getContents
```
