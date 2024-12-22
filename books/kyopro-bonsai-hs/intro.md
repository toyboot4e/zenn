---
title: "始めに"
---

[Haskell Advent Calendar 2024](https://qiita.com/advent-calendar/2024/haskell) 22 日目の投稿です。


# 概要

これは [AtCoder Library (ACL)](https://github.com/atcoder/ac-library) を Haskell に移植した際の **ブログ** です。 [ac-library-hs](https://github.com/toyboot4e/ac-library-hs) の開発をダシに、競プロ文脈で Haskell を学ぶとこうなる、という具体例を延々とやって行きます。

『競プロ盆栽』というタイトルですが、 **Haskeller 向けの投稿** です。競プロ er で読みに来てくれた方、お求めの物ではないかもしれません。お詫びに λ をお送りします。 / λ λ λ λ λ λ λ / ﾗﾑﾀﾞｲｯﾁｮｰ

難度感としては、『Haskellで戦う競技プログラミング 第2版』を熟読した (or する) ぐらいの人が読めると思います。 [`PrimMonad`](https://hackage.haskell.org/package/primitive-0.9.0.0/docs/Control-Monad-Primitive.html#t:PrimMonad) が使える／興味があるぐらいの人にちょうどいいです。

https://booth.pm/ja/items/1577541

また投稿の内容はブログであるため、僕が調べた・考えた内容のごった煮になります。「そうそうそう」と頷ける部分があれば、「これは違う！」と嘆かれることもありそうですが、いかんせんコンテンツの少ない界隈ですので、お楽しみ頂ける部分があれば幸いです。


# 動作環境

以下の環境で作業しました。

-   Linux
-   GHC 9.8.3
