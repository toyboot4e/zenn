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

`AnyPool` は `ComponentPool<T>` のアップキャストです。 `ComponentPool<T>` は:

```rust:comp.rs
/// Sparse set of components of type T
#[derive(Debug)]
pub struct ComponentPool<T> {
    set: SparseSet<T>,
}
```

* `ComponentPool::swap_remove` は、ひとまず `pub(crate)` にしてユーザから隠しました。

地道にメソッドを追加します。 `&[SparseIndex]` を `&[Entity]` にキャストするシーンがありました:

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

## `BorrowWorld` 実装

Resource の章で定義した `BorrowWorld` を実装すれば、 system が `Comp<T>` や `CompMut<T>` を引数に取ることができるようになります。 `ComponentPool` は特殊な resource であるともとれますね。

イテレータの実装は後の章に回します。

[`toecs`]: https://github.com/toyboot4e/toecs

リファレンス実装: [9f76aec](https://github.com/toyboot4e/toecs/commit/9f76aec66df3e20a3729b2bb9b42c38a7d7f4fc9)

