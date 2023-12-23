---
title: "[1-2] AtCoder Beginnes Selection"
---

[AtCoder Beginners Selection](https://atcoder.jp/contests/abs) (ABS) の 10 問 を解きます。これが解けたら、演習込みで Haskell に入門できたと言って良いでしょう。

# AtCoder アカウント

問題ページの閲覧には AtCoder アカウントが必要です。アカウント作成の際には、以下の点にご注意ください:

- アカウント名  
  アカウント名の変更は 1 度しかできません。始めから慎重に決めましょう。
- 単一アカウントの個人運用  
  複数アカウントの作成やアカウントの共有は禁じられてます [^1] 。たとえば `me-haskell` と `me-rust` というアカウントを使い分けることはできません。

# ABS の問題を解く

以下に問題とヒントを記します。解答例も後ほどリンクします。

ヒント:

- ローカル環境でファイル編集するのがおすすめです  
  AtCoder サイトにエディタが付属していますが、提出する度に空欄となるため、ローカルにファイル保存した方が圧倒的に速く終わります。

- サンプルケースを確認しましょう  
  プログラムの提出前にはローカル環境でサンプルケースをすべて実行し、動作確認しておきましょう。やはりその方が速く終わります。

## 問題 1 ~ 3: 標準入出力

最初の 3 問からは標準入出力を学ぶことができます:

1. [ABC086A - Product](https://atcoder.jp/contests/abs/tasks/abc086_a)
2. [ABC081A - Placing Marbles](https://atcoder.jp/contests/abs/tasks/abc081_a)
3. [ABC081B - Shift only](https://atcoder.jp/contests/abs/tasks/abc081_b)

:::details ヒント 1
標準入力から 1 行読み込み、 `Int` 列に変換するコード例が以下です:

```hs:Main.hs
ints :: IO [Int]
ints = map read . words <$> getLine
```

- [`<$>`] 演算子は『[すごい Haskell](https://shop.ohmsha.co.jp/shopdetail/000000001926/)』 P244 に登場します。 `IO` モナドが絡むため手強いです。
- [`getLine`], [`words`], [`read`] などの慣れない関数は、 AI に聞いたり [Hoogle] などで検索しましょう
- その他、Haskell の入門書などを参照しつつ解答して行きましょう。
:::

:::details ヒント 2
出力に使えるのは `putStrLn` および `print` です。

```hs:putStrLn の型と使用例
ghci> :t putStrLn
putStrLn :: String -> IO ()

ghci> putStrLn "ABC"
ABC
```

```hs: print の型と使用例
ghci> :t print
print :: Show a => a -> IO ()

ghci> print 1
1
```

`print x` は `putStrLn (show x)` と等価です ([ソース](https://hackage.haskell.org/package/base-4.17.1.0/docs/src/System.IO.html#print)):

```hs
ghci> putStrLn (show 1)
1
```

文字列を `show` すると、元の文字列が `"` で囲われることに注意してください:

```hs
ghci> show "ABC"
"\"ABC\""

ghci> print "ABC"
"ABC"
```

したがって文字列を表示する際には `putStrLn` を使用します:

```hs
ghci> putStrLn "ABC"
ABC
```

文字列の連結には `++` を使用します:

```hs
ghci> "A" ++ "B" ++ "C"
"ABC"
```

また文字列同士を空白区切りで連結するには `unwords` を使用します:

```hs
ghci> unwords ["A", "B", "C"]
"A B C"

ghci> putStrLn (unwords ["A", "B", "C"])
A B C
```

逆に、空白区切りの文字列を単語列に分けるのが `words` です:

```hs
ghci> words "1 2 3"
["1","2","3"]

ghci> map read (words "1 2 3") :: [Int]
[1,2,3]
```
:::

## 問題 4 ~ 5: 全探索

最初の 3 問だけでも意外と大変ではなかったでしょうか。続く 2 問です:

4. [ABC087B - Coins](https://atcoder.jp/contests/abs/tasks/abc087_b)
5. [ABC083B - Some Sums](https://atcoder.jp/contests/abs/tasks/abc083_b)

:::details 解法のヒント
算数では最適解が求まりません。すべての場合を試す必要があります。いわゆる全探索です。

この問題ではリスト内包表記を使って全探索ができます:

```hs
ghci> [na | na <- [0 .. 5]]
[0,1,2,3,4,5]
ghci> [(na, nb) | na <- [0 .. 5], nb <- [0 .. 5]]
[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5)]
```

算数から全探索への発想転換は、競技プログラミングへの第一歩です。
:::

## 問題 6 ~ 10: [`Data.List`](https://hackage.haskell.org/package/base/docs/Data-List.html) の関数の利用

最後の 5 問です:

6. [ABC088B - Card Game for Two](https://atcoder.jp/contests/abs/tasks/abc088_b)
7. [ABC085B - Kagami Mochi](https://atcoder.jp/contests/abs/tasks/abc085_b)
8. [ABC085C - Otoshidama ](https://atcoder.jp/contests/abs/tasks/abc085_c)
9. [ABC049C - 白昼夢](https://atcoder.jp/contests/abs/tasks/arc065_a)
10. [ABC086C - Traveling](https://atcoder.jp/contests/abs/tasks/arc089_a)

:::details 解法のヒント
6. 降順 [`sort`] します ([`sortOn`] などを用います) 。
7. [`nub`] など [^2] で重複除去します。
8. 全探索を効率化します。
9. 前からマッチすると解けません。 [`reverse`] して後ろから読むと、 [`stripPrefix`] を繰り返し適用するだけで解けます (！)
10. 初期位置を追加した上で、 [zipWith](https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:zipWith) により隣接 2 項を比較します。
:::

# 解答例

以下が解答例です。ある程度コメントも記載しています。

> 解答が合わなければ AI に聞いてみたり、他の人の解答も検索してみてください。

| 問題                                                                             | 解答プログラム                                       |
|----------------------------------------------------------------------------------|------------------------------------------------------|
| 1. [ABC086A - Product](https://atcoder.jp/contests/abs/tasks/abc086_a)           | https://atcoder.jp/contests/abs/submissions/47966250 |
| 2. [ABC081A - Placing Marbles](https://atcoder.jp/contests/abs/tasks/abc081_a)   | https://atcoder.jp/contests/abs/submissions/47966417 |
| 3. [ABC081B - Shift only](https://atcoder.jp/contests/abs/tasks/abc081_b)        | https://atcoder.jp/contests/abs/submissions/47966713 |
| 4. [ABC087B - Coins](https://atcoder.jp/contests/abs/tasks/abc087_b)             | https://atcoder.jp/contests/abs/submissions/48277048 |
| 5. [ABC083B - Some Sums](https://atcoder.jp/contests/abs/tasks/abc083_b)         | https://atcoder.jp/contests/abs/submissions/47968099 |
| 6. [ABC088B - Card Game for Two](https://atcoder.jp/contests/abs/tasks/abc088_b) | https://atcoder.jp/contests/abs/submissions/48277086 |
| 7. [ABC085B - Kagami Mochi](https://atcoder.jp/contests/abs/tasks/abc085_b)      | https://atcoder.jp/contests/abs/submissions/48277124 |
| 8. [ABC085C - Otoshidama](https://atcoder.jp/contests/abs/tasks/abc085_c)        | https://atcoder.jp/contests/abs/submissions/48277234 |
| 9. [ABC049C - 白昼夢](https://atcoder.jp/contests/abs/tasks/arc065_a)            | https://atcoder.jp/contests/abs/submissions/48277292 |
| 10. [ABC086C - Traveling](https://atcoder.jp/contests/abs/tasks/arc089_a)        | https://atcoder.jp/contests/abs/submissions/48012395 |

# 閑話

自分の手で Haskell を書くと認識のズレが明確になり、コンパイラから様々な警告やエラーを受けるものと思います。たとえば以下の気付きがあるのではないでしょうか。

## 評価優先度

関数適用は最も評価優先度が高いです。 `()` が不要なことも多く、 linter が知らせてくれます:

```hs
ghci> 1 + (max 2 3)
4
ghci> 1 + max 2 3
4
ghci> [0, 1, 2] !! (pred 3)
2
ghci> [0, 1, 2] !! pred 3
2
```

## 関数適用演算子 ([`$`])

[`$`] 演算子を使うと `()` の数を減らすことができます。エディタでのカーソル移動も減って便利です:

```hs
ghci> (1 +) (sum [2, 3])
6
ghci> (1 +) $ sum [2, 3]
6
```

`$` 演算子の左辺は関数です。 `(1 +)` の部分は `()` を付けなければ 1 変数関数とならず、構文エラーになります:

```hs
ghci> 1 + $ sum [2, 3]
<interactive>:4:5: error:
    parse error on input ‘$’
    Suggested fix: Perhaps you intended to use TemplateHaskell
```

## 関数合成 ([`.`])

ポイントフリースタイル (引数を省略した形) で関数を定義する際は、関数合成 [`.`] を使います:

```hs
ghci> let add3 = (1 +) . (2 +)
ghci> add3 1
4
```

## `$` と `.` の区別を付けるには

`$` を書いても `.` を書いても結果が変わらない場合があります:

```hs
ghci> (1 +) $ (2 + ) $ 3 + 4
10
ghci> (1 +) . (2 + ) $ 3 + 4
10
```

しかし `$` と `.` は異なる演算子です。混同すると、コンパイルエラーから抜け出せなくなる場合があります。

- [`$`] は関数適用演算子です。左辺に関数を、右辺に値を取ります。
- [`.`] は関数合成演算子です。両辺に関数を取ります。

両者を取り違えた場合のエラーを紹介します。

### `$` で `.` を代替できないケース

関数をポイントフリースタイルで定義する際に、 `.` の替わりに `$` を使うとエラーとなります。初歩的な Haskell が Rust の borrow checker よりも手強いとさえ思えるかもしれません:

```hs
ghci> :{
ghci| let add3 :: Int -> Int
ghci|     add3 = (+ 1) $ (+ 2)
ghci| :}

<interactive>:15:13: error:
    • No instance for (Num (Int -> Int)) arising from a use of ‘+’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: (+)
      In the first argument of ‘($)’, namely ‘(+ 1)’
      In the expression: (+ 1) $ (+ 2)
```

込み入ったケースでは、 GHC の推論に付いていけなくなる場合もあります。そうした際には、まず関数に型アノテーションを付けると良いでしょう。

たとえば上の例でも、 `add3` の型アノテーションを省略した場合、 `add3` のコンパイルが通ってしまいます。そして `Integer` に対して `add3` がインスタンス化された時にエラーが顕在化し、より不可解な状況が発生します:

```hs
ghci> let add3 = (+ 1) $ (+ 2)
ghci> add3 5
<interactive>:21:1: error:
    • No instance for (Num (Integer -> Integer))
        arising from a use of ‘it’
        (maybe you haven't applied a function to enough arguments?)
    • In the first argument of ‘print’, namely ‘it’
      In a stmt of an interactive GHCi command: print it
```

`add3` を定義する際に型を書いていればコンパイルエラーが発生し、まだ理解しやすい状況と言えます。この先無数のエラーと戦うことになると思いますが、 `where` 句の関数などにも型を書けば、多少なりとも正しい意図をコンパイラに伝え、より適切なエラーメッセージが得られると思います。

### `.` で `$` を代替できないケース

関数合成演算子 (`.`) が関数適用演算子 (`$`) を代替できない例も確認します:

```hs
ghci> :{
ghci| let calc6 :: Int
ghci|     calc6 = (1 +) . (2 +) . 3
ghci| :}

<interactive>:13:13: error:
    • Couldn't match expected type ‘Int’ with actual type ‘a0 -> c0’
    • Probable cause: ‘(.)’ is applied to too few arguments
      In the expression: (1 +) . (2 +) . 3
      In an equation for ‘calc6’: calc6 = (1 +) . (2 +) . 3
```


修正すれば以下の通りです:

```hs
ghci> :{
ghci| let calc6 :: Int
ghci|     calc6 = (1 +) . (2 +) $ 3
ghci| :}
ghci> calc6
6
```

この手のエラーは、関数定義をポイントフリースタイルに切り替えた時などに意図せず発生させてしまいます。意外と長い付き合いになります。

# ABS を追えたら

これにて Haskell 入門は完了です。以降は毎週開催のコンテスト (AtCoder Beginner Contest) に参加していきましょう！

[^1]: [AtCoder の利用規約](https://atcoder.jp/tos?lang=ja)
[^2]: `nub` は $O(n^2)$ と非常に低速です。 Haskell プロジェクトの作成 (次章) 後は、 [`extra`] パッケージの [`nubSort`] を使った方が $O(n \log n)$ で速くて安心だと思います。

[`sort`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:sort
[`sortOn`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:sortOn
[`nub`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:nub
[`reverse`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:reverse
[`stripPrefix`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Data-List.html#v:stripPrefix

[`extra`]: https://www.stackage.org/lts-21.7/package/extra-1.7.14
[`nubSort`]: https://www.stackage.org/haddock/lts-21.7/extra-1.7.14/Data-List-Extra.html#v:nubSort
[`$`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:-36-
[`.`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:.

[`<$>`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:-60--36--62-
[`getLine`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:getLine
[`words`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:words
[`read`]: https://hackage.haskell.org/package/base-4.17.1.0/docs/Prelude.html#v:read
[Hoogle]: https://hoogle.haskell.org/

