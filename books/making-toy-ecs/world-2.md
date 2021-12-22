---
title: "World の API - 2"
---

複数のデータを一括で挿入・削除できるようにします。

僕は `trait` を追加しましたが、マクロを公開するのでも十分かもしれません。

# `ComponentSet`

『複数の component』を抽象します。

マクロを頑張りました。

リファレンス実装: [18b6cd0](https://github.com/toyboot4e/toecs/commit/18b6cd0d5c5601c978789e228a94da7632377c82)

# `World::spawn` が `ComponentSet` を取るように変更

`Entity` の生成時に一括で component を追加します。

Component を追加しない場合は `spawn_empty` を呼びます。

> `sparsey` では、 `spawn(())` で空の `Entity` を作れるようにしています。 `toecs` では、 `spawn(())` は `()` を component として追加する風にも読めるので止めました。

リファレンス実装: [7458c41](https://github.com/toyboot4e/toecs/commit/7458c41194915dd67b77782f38a8a16cce3c295d)

# `World::*_many` を追加

`ComponentSet` のメソッドに繋ぎます。

リファレンス実装: [c84283f](https://github.com/toyboot4e/toecs/commit/c84283f4ce000abd4ccfb2036844701dd57bc45e)

`ComponentSet` はユーザから隠してもいいかもしれません。
