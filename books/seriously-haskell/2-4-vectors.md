---
title: "[2-4] 📜 vector の使い方"
---

# 分岐点

[`vector`] パッケージを使うかどうかは重要な分岐点です。 `vector` はプレインストールされていないため、たとえば [Codeforces](https://codeforces.com/) では使用できません。存分に `vector` を使用できるのは AtCoder と Library Checher くらいのものでしょう。

また `vector` は Haskeller の共通言語ではないという面もあります。実際 [Richard Bird 先生](https://www.amazon.co.jp/s?i=stripbooks&rh=p_27%3ARichard+Bird&s=relevancerank&text=Richard+Bird&ref=dp_byline_sr_book_1) が書いた本では、リストと `array` でプログラムを構成します。正統派 Haskeller は `vector` を使わないという感覚があります。

以上の背景を踏まえた上で、本書では `vector` パッケージを使うメリットを優先します。 `vector` の実行速度や豊富な API は素晴らしいです。それに `vector` 使いの中にも匠の技を持つ人たちはいて、そう簡単に真似できるものではないため、そうした意味でも心配ありません。

# Qualified import の名前

[`vector`] の公式ドキュメントでは、以下の import が採用されています:

```hs
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Generic qualified as VG
```

しかし以降では次の import を採用します:

```hs
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U -- VU -> U
import Data.Vector.Generic qualified as G -- VG -> G
```

理由は僕の好みと、タイプ数を減らすことで心理的抵抗を減らすことができる期待からです。合わない人には申し訳ありません。

[`vector`]: https://github.com/haskell/vector

