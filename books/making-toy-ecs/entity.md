---
title: "ストレージ 3: Entity"
---

`Entity` は `SparseSet<T>` の亜種に保存します。

このストレージは `Entity` の世代番号をインクリメントします。

# `Entity` = `SparseIndex`

```rust:ent.rs
#[repr(transparent)]
pub struct Entity(SparseIndex);

// SparseIndex { u32, Generation }
```

後で `&SparseIndex` → `&Entity` のキャストができるように [#\[transparent\]][tr] にしました。

[tr]: https://doc.rust-lang.org/nomicon/other-reprs.html#reprtransparent

# `EntityPool`

`Entity` も sparse set に入れます:

![](/images/toecs/entity-pool.png)

前章の `SparseSet<T>` の違いとしては、保存するデータ (`Entity`) 自体が sparse index であるため `Vec<SparseIndex>` がありません。

## 世代番号の引き継ぎ

`EntityPool` は sparse index のアロケータです。空欄をリサイクルするときは世代番号を引き継いでインクリメントします。そのため空欄が世代番号を保持します:

```rust:ent.rs
pub struct EntityPool {
    entries: Vec<Entry>,
    data: Vec<Entity>,
    // free_root: Option<NonZeroU32>,
}

enum Entry {
    ToDense(DenseIndex),
    Empty {
        gen: Generation,
        // next: Option<NonZeroU32>,
    },
}
```

> 空欄で連結リストを作ると新規 `Entity` の作成コストが一定になります。

`World` に `EntityPool` を追加しておきます:

```rust:lib.rs
#[derive(Debug, Default)]
pub struct World {
    res: ResourceMap,
    ents: EntityPool,
}
```

リファレンス実装: [0359776](https://github.com/toyboot4e/toecs/commit/0359776806715424ae59e8ca23c52be3c668ac89)

## 備考: 連結リストで空きスロット管理

以下を初期状態とします:

![](/images/toecs/free-slot.png)

挿入時はルートが指す空きスロットを使います。ルートは 2 番目の空きスロットを指すように書き換えます:

![](/images/toecs/free-slot-insert.png)

削除時はルートが新たな空きスロットを指します。また新たな空きスロットは以前のルートが指すスロットを指します:

![](/images/toecs/free-slot-remove.png)

