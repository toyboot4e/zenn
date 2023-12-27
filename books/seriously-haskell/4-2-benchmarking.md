---
title: "[4-2]. ベンチマークテスト"
---

[`criterion`] や [`tasty-bench`] によるベンチマークテストの様子を紹介します。

# ベンチマーク環境の作成

ベンチマーク用プロジェクトでは、コンパイル環境を AtCoder と揃えてみます。

[付録 A](./a-judge-env) で確認する通り、 AtCoder 環境では GHC の LLVM バックエンドが使用されています。 LLVM バックエンドを使った場合、コンパイル時間は長くなるものの、実行速度は速くなる傾向にあるようです。コンテストの参加時は通常の GHC を使い、ベンチマーク時は AtCoder に合わせて LLVM バックエンドを選ぶ、という風に使い分けすると良いと思います。

## プロジェクト毎に GHC のバージョンを切り替えるには

GHC のバージョンを切り替えるには、 [`direnv`] を使うのが簡単と思います。 [`direnv`] はディレクトリごとに `PATH` の追加設定を施すツールです。プロジェクトに `.envrc` ファイルを配置することによって、 GHC のバージョンを切り替えできます。

いずれビルドツールの開発が進んだ日には、 `cabal.project` のような設定ファイルから気軽に GHC のバージョンを切り替えできるようになるかもしれません。

## GHC (LLVM バックエンド)

LLVM バックエンドの GHC をインストールするには、以下の記事などをご覧ください:

https://zenn.dev/mod_poppo/articles/haskell-setup-2023#%E8%A3%9C%E9%81%BA%EF%BC%9Allvm%E3%83%90%E3%83%83%E3%82%AF%E3%82%A8%E3%83%B3%E3%83%89%E3%82%92%E4%BD%BF%E3%81%86

> Nix ユーザは以下のように LLVM フラグを立てれば良いでしょう:
>
> ```nix
> (haskell.compiler.ghc945.override { useLLVM = true; })
> ```

## Cabal プロジェクトの設定

Cabal プロジェクトの設定も [付録 A](./a-judge-env) に記載があるためご参照ください。

- `cabal.project`
  ここにも LLVM の設定があります。

- 依存パッケージ
  バージョン指定があります。

サンプルとして、以下にベンチマークプロジェクトを公開しています:

https://github.com/toyboot4e/seriously-haskell/tree/main/benchmarks

# ライブラリの紹介

ここではベンチマークテストのライブラリとして [`criterion`] および [`tasty-bench`] を紹介します。リッチな [`criterion`] に対し、軽量な [`tasty-bench`] が台頭した印象があります:

| ライブラリ      | ドキュメント性 | 軽量さ | html 出力     | 時間軸の対数プロット |
|-----------------|----------------|--------|---------------|----------------------|
| [`criterion`]   | △              | △      | ◎             | ◎                    |
| [`tasty-bench`] | ◎              | ◎      | △ (画像 1 枚) | △ (データ出力のみ)   |

[`tasty-bench`] は [`criterion`] 互換の API も提供しています ([How to switch? - README](https://github.com/Bodigrim/tasty-bench#how-to-switch)) 。まず [`criterion`] を試してから、 [`tasty-bench`] も動かしてみるのがおすすめです。

## [`criterion`] を使う場合

[`criterion`] は長らく標準的なベンチマークテストのライブラリでした。結果をリッチな html に出力してくれる他、詳しい統計値も合わせて表示してくれます [^1] 。

競技プログラミングのベンチマークにおいては、実行速度の遅さがデメリットではあるものの、時間軸を対数スケールにしてプロットできる点が ◎ です。対数スケールだと、プログラムの実行時間を比較しやすくなります:

![criterion](/images/seriously-haskell/criterion.png)
*ベンチマーク結果の例 (時間軸は等倍スケール)*

![criterion](/images/seriously-haskell/criterion-log.png)
*ベンチマーク結果の例 (時間軸は対数スケール)*

[`criterion`] で作られる html のサンプルを以下に公開しています:

https://toyboot4e.github.io/seriously-haskell/

## [`tasty-bench`] を使う場合

[`tasty-bench`] は [`vector`] などで使用されるベンチマークのライブラリです。軽量さやドキュメント性の高さが非常に良いと思います。

競技プログラミングのベンチマークにおいては、実行速度が速いというのが大きなメリットであるものの、時間軸を対数にできません。通常の画像 (SVG) 出力は次のようになります:

![tasty-bench](/images/seriously-haskell/tasty-bench.png)
*ベンチマーク結果 (時間軸は対数)*

このように、実行速度に差があり過ぎる場合は等倍スケール表示では比較が難しくなります。そこで `tasty-bench` によるベンチマークの結果を対数時間でプロットするスクリプトを書いてみました ([ソース](https://github.com/toyboot4e/seriously-haskell/blob/main/benchmarks/plot.hs)) 。僕はこちらを好んで使っています:

![criterion](/images/seriously-haskell/tasty-bench-terminal.png)
*ベンチマーク結果 (時間軸は対数)
画像が小さくて申し訳ないですが*

# 例: ナップサック問題

EDPC のナップサック問題を例に、様々な解法の実行速度を比較してみました。

問題はこちら:

https://atcoder.jp/contests/dp/tasks/dp_d

[`criterion`] の出力は以下からご覧頂けます:

https://toyboot4e.github.io/seriously-haskell/

ソースコードはこちらです:

https://github.com/toyboot4e/seriously-haskell/blob/main/benchmarks/src/Knapsack.hs

面白かった結果としては、

- Boxed vs unboxed
  Boxed な vector を使用した場合、 unboxed な vector よりも 100 倍以上遅くなっていました。それほどでしたか。

- リストでも通る
  疎なリストを DP 配列の代わりとした場合も、 DeepSeq パッケージの `force` 関数を適用すると AC できました。ベンチマーク結果としてもそこそこ速いのが確認できます。

- 単調増加リストを使うと
  `(w, v)` が単調増加するリストを持つと、枝刈りによって大幅な高速化ができます。

# 備考: GC

ベンチマークにおいては GC の所要時間を確認できませんでした。例として、 boxed な vector でナップサック問題を解いた場合のプロファイリングを取ってみます:

```sh
$ cabal run --ghc-options '-with-rtsopts="-s"'
```

```txt
Executed in    6.21 secs    fish           external
   usr time    4.50 secs  473.00 micros    4.50 secs
   sys time    1.46 secs  145.00 micros    1.46 secs
     852,980,856 bytes allocated in the heap
   2,148,227,328 bytes copied during GC
     509,994,728 bytes maximum residency (7 sample(s))
       7,444,760 bytes maximum slop
             954 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       181 colls,     0 par    0.399s   0.399s     0.0022s    0.0166s
  Gen  1         7 colls,     0 par    0.439s   0.439s     0.0627s    0.2076s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.369s  (  0.368s elapsed)
  GC      time    0.838s  (  0.838s elapsed)
  EXIT    time    0.000s  (  0.004s elapsed)
  Total   time    1.208s  (  1.210s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    2,311,589,815 bytes per MUT second

  Productivity  30.6% of total user, 30.4% of total elapsed
```

抜粋すると、 GC の時間が長いようです:

```
  MUT     time    0.369s  (  0.368s elapsed)
  GC      time    0.838s  (  0.838s elapsed)
```

ヒープサイズをデフォルト (4MB) から 1 GB に書き換えて実行してみます ([-A オプション](https://downloads.haskell.org/~ghc/9.4.5/docs/users_guide/runtime_control.html#rts-flag--A%20%E2%9F%A8size%E2%9F%A9)):

```sh
$ cabal run --ghc-options '-with-rtsopts="-s -A1G"'
```

```txt
  MUT     time    0.453s  (  0.453s elapsed)
  GC      time    0.002s  (  0.002s elapsed)
```

なぜか MUT が長くなっていますが、 GC の時間が 0 になりました。これならギリギリ AC できそうです。ただし `Main.hs` からは RTS (runtime system) の設定変更はできないため、 AtCoder 環境ではこのようにヒープサイズを変更できません。

ちなみに unboxed な vector の実行結果は以下で、 GC を抑制しても 100 倍近く速いです。そんな……:

```sh
  MUT     time    0.006s  (  0.006s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
```

## GC が長い場合

GC を避けると良い例としては `IntMap` の解法忘れなどがあります。計算過程の `IntMap` を不用意に配列に保存していると、ヒープに大きなデータを蓄えてしまい、 GC による TLE が発生したことがあります。 GC の計算時間は、少なくとも要素数に比例するはずですから、適宜解放できるように気をつけましょう。

## プロファイリング結果をプロットしたい？

MUT, GC の時間比はベンチマークのライブラリでは収集できない情報かと思います。何か気軽に GC/MUT を測定する方法が欲しいところですね。

[^1]: 僕は [`criterion`] の示す統計値を理解していません

[`direnv`]: https://direnv.net/
[`nix-direnv`]: https://github.com/nix-community/nix-direnv

[`tasty`]: https://github.com/UnkindPartition/tasty
[`tasty-bench`]: https://github.com/Bodigrim/tasty-bench
[`criterion`]: https://github.com/haskell/criterion
[`vector`]: https://github.com/haskell/vector

[`hunit`]: https://github.com/hspec/HUnit
[`hspec`]: https://github.com/hspec/hspec
