---
title: "ライブラリとしての API - 1"
---

# System の返値

今までは返値なしの system しか書けませんでしたが、 `SystemResult<()>` を返す関数も使えるようにします:

```rust
/// Alias of [`anyhow::Result`]
pub type SystemResult<T = ()> = anyhow::Result<T>;
```

返値なしの system は、実行すると必ず `Ok(())` を返します。

リファレンス実装: [6f724b6](https://github.com/toyboot4e/toecs/commit/6f724b67378624226409c4e1b2717022bcd211d6)

# 複数の resource を一括操作

例によって `trait ResourceSet` を追加して、マクロでタプルに実装しました。

リファレンス実装: [3057bae](https://github.com/toyboot4e/toecs/commit/3057bae689953dca07180a9ea2141f229d860625)

