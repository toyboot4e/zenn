---
title: "World の API - 1"
---

外部クレートから見た [`toecs`] を考えて実装を進めます。

[`toecs`]: https://github.com/toyboot4e/toecs

`World` へのデータ挿入・削除には、 `World` への排他的アクセスが必要だということにしました。

# `World` の API を作る

## Integration test を追加する

* `src/tests.rs` からは `lib.rs` の中身と `pub(crate)` なアイテムが見えます。
  内部実装のテストに適しています。

* 一方 `tests/` 以下のファイルは外部クレートとして扱われるため、 public なアイテムしか見えません。
  API のテストに適しています。

```
.
├── src
└── tests/it
    └── main.rs
```

Integration test は外部クレートなので、コンパイル時間短縮のために `it` 1 つに集約します [^1] 。

> `toecs` は `tests/` 以下に 1  つしかファイルを持ちませんが、良い習慣として `tests/it/main.rs` にしました。

## 内部可変性を使ったアクセサを公開

* `&World` から内部データ `impl DerefMut<T>` が取れます。
* ただしデータの追加・削除には必ず `&mut World` が必要であるとしました。

> `ComponentPool::insert` などはユーザから隠しています。

`prelude` モジュールも追加しました。

リファレンス実装: [0651328](https://github.com/toyboot4e/toecs/commit/06513283310260fdc9decbd97ae25f1896488a62)

## `World::despawn(Entity)`

`Entity` を削除するときはその component も削除します:

```rust:lib.rs(疑似コード)
component_pools_mut()
    .for_each(|component_pool: &mut Box<dyn Any>| {
        /** 削除する処理 **/
    });
```

## 問題

ここで `component_pool` に必要なのは:

* 従来の機能 ([`Any`])
  `ComponentPool<T>` へダウンキャストする機能

* 追加したい機能
  アップキャストされたまま `remove(Entity)` する機能

[`Any`]: https://doc.rust-lang.org/beta/core/any/trait.Any.html

## 解決策

[downcast_rs] を使いました:

[downcast_rs]: https://github.com/marcianx/downcast-rs

```rust:comp.rs
use downcast_rs::Downcast;

pub(crate) trait ErasedComponentPool: Downcast {
    fn erased_remove(&mut self, entity: Entity);
}

// `dyn ErasedComponent` が `dyn Any + ErasedComponentPool` のように使えるようになる
```

リファレンス実装: [65ce774](https://github.com/toyboot4e/toecs/commit/65ce7747b87aba3f6f401ffce948c611d6ed3add)

`ComponentPool` を `dyn ErasedComponentPool` として持つことで、 component を一括削除ができるようにできます:

```rust:lib.rs(疑似コード)
component_pools_mut()
    .for_each(|component_pool: &mut dyn ErasedComponentPool| {
        component_pool.erased_remove(entity);
    });
```

リファレンス実装: [bc3af90](https://github.com/toyboot4e/toecs/commit/bc3af90e634c67c80336c5aff42d63e75d59bbf6)

[^1]: [Delete Cargo Integration Tests](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)
[^2]: うかつにデータを借りるとパニックします。一方 Bevy の [`World`] は `&mut self` を取る安全なメソッドを提供します。内部可変を使ったメソッドは [WorldCell][wc] を経由してアクセスするようになっています。

[`World`]: https://docs.rs/bevy/latest/bevy/ecs/world/struct.World.html
[wc]: https://docs.rs/bevy/latest/bevy/ecs/world/struct.WorldCell.html

