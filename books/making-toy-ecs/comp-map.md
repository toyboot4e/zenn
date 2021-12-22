---
title: "ストレージ 4: Components"
---

Component の型毎に `SparseSet<T>` を用意します。

これらのストレージは、実は resource の親戚として捉えられます。

# `ComponentPoolMap`

`TypeId` → `ComponentPool<T>` のマップを用意します。 `ResourceMap` とほぼ同様です:

```rust:comp.rs
#[derive(Debug, Default)]
pub struct ComponentPoolMap {
    cells: FxHashMap<TypeId, AtomicRefCell<AnyPool>>,
}

#[derive(Debug)]
struct AnyPool {
    any: Box<dyn Any>,
}
```

`AnyPool` は `ComponentPool<T>` のアップキャストです:

```rust:comp.rs
/// Sparse set of components of type T
#[derive(Debug)]
pub struct ComponentPool<T> {
    set: SparseSet<T>,
}
```

* プールは `ComponentPoolMap::register` を呼んで明示的に追加することにしました。
* Component を削除する機能はひとまず実装しません。

`&[SparseIndex]` を `&[Entity]` にキャストするシーンがありました:

```rust:comp.rs
impl<T> ComponentPool<T> {
    fn to_entities(sparse: &[SparseIndex]) -> &[Entity] {
        // SAFE: `Entity` is a transparent wrapper of `SparseIndex`
        unsafe { slice::from_raw_parts(sparse as *const _ as *const _, sparse.len()) }
    }
}
```

リファレンス実装: [9f76aec](https://github.com/toyboot4e/toecs/commit/9f76aec66df3e20a3729b2bb9b42c38a7d7f4fc9)

# `Comp<T>`, `CompMut<T>`

アクセサを追加します:

```rust:comp.rs
pub fn borrow<T: Component>(&self) -> Option<Comp<T>> { /* ~~ */ }
pub fn borrow_mut<T: Component>(&self) -> Option<CompMut<T>> { /* ~~ */ }
```

返すのは `Deref` な型です:

```rust:comp.rs
pub struct Comp<'r, T: 'static> {
    borrow: AtomicRef<'r, ComponentPool<T>>,
}

pub struct CompMut<'r, T: 'static> {
    borrow: AtomicRefMut<'r, ComponentPool<T>>,
}
```

> [`toecs`] では `CompMut` を作るときに `AtomicRefCell::borrow_mut` を呼びました。干渉するようなデータの借り方をした場合、即 panic します。
>
> 一方 `CompMut` が `deref_mut` するタイミングまで `borrow_mut` を遅延すれば、 `deref_mut` のログを取ることで中のデータが変更された (可能性があるか) を後から調べることができます。
>
> * `deref_mut` が呼ばれたフラグを持つよりも、変更された時のゲームのフレーム数 (`Tick`) を持った方が楽かもしれません (`false` への初期化が不要なため) 。

## `BorrowWorld` 実装

Resource の章で定義した `BorrowWorld` を実装すれば、 system が `Comp<T>` や `CompMut<T>` を引数に取ることができるようになります。

イテレータの実装は後の章に回します。

[`toecs`]: https://github.com/toyboot4e/toecs

リファレンス実装: [9f76aec](https://github.com/toyboot4e/toecs/commit/9f76aec66df3e20a3729b2bb9b42c38a7d7f4fc9)

