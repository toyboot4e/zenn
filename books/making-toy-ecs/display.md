---
title: "間話: 山猫部隊"
---

Rust はあまり問題を簡単にする方向には向かわず、比較的脳筋で、時にはどうしてもトリッキーなコードが必要になります。まるでリボルバーを撃つ前に必ずクルクルしないといけない病気にかかったかのようです。

この章では進んでトリックを使います。

# Debug タイム

## `#[derive(Debug)]` の現状

無惨です:

:::details print
```rust
World {
    res: ResourceMap {
        cells: {
            TypeId {
                t: 8766594652559642870,
            }: AtomicRefCell { ... },
            TypeId {
                t: 14670835671796707678,
            }: AtomicRefCell { ... },
        },
    },
    ents: EntityPool {
        entries: [
            ToDense(
                DenseIndex {
                    raw: RawDenseIndex(
                        0,
                    ),
                    gen: Generation {
                        raw: 1,
                    },
                },
            ),
            ToDense(
                DenseIndex {
                    raw: RawDenseIndex(
                        2,
                    ),
                    gen: Generation {
                        raw: 2,
                    },
                },
            ),
            ToDense(
                DenseIndex {
                    raw: RawDenseIndex(
                        2,
                    ),
                    gen: Generation {
                        raw: 1,
                    },
                },
            ),
        ],
        data: [
            Entity(
                SparseIndex {
                    raw: RawSparseIndex(
                        0,
                    ),
                    gen: Generation {
                        raw: 1,
                    },
                },
            ),
            Entity(
                SparseIndex {
                    raw: RawSparseIndex(
                        2,
                    ),
                    gen: Generation {
                        raw: 1,
                    },
                },
            ),
            Entity(
                SparseIndex {
                    raw: RawSparseIndex(
                        2,
                    ),
                    gen: Generation {
                        raw: 2,
                    },
                },
            ),
        ],
    },
    comp: ComponentPoolMap {
        cells: {
            TypeId {
                t: 8766594652559642870,
            }: AtomicRefCell { ... },
            TypeId {
                t: 14670835671796707678,
            }: AtomicRefCell { ... },
        },
    },
}
```
:::

1. 全体的に冗長
2. `AtomicRefCell` の中身が見えない

もっと debug に役立つ表示をします。

## Entity の pretty print

`Debug` trait を手動実装し、 `Entity(slot, generation)` の形で表示されるようにしました。

リファレンス実装: [997d2c6](https://github.com/toyboot4e/toecs/commit/997d2c6dca83bc44cf9d6257c79e5eb1f5b46972)

## `AtomicRefCell` の中を覗く debug print

### `Debug` を要求

中身を表示できるよう制約を加えました:

* `trait Resource: 'static + Debug`
* `trait Component: 'static + Debug`

ただしユーザ型に `#[derive(Debug)]` を忘れると `Component` とみなされないため、謎のエラーが出るようになります。デメリットの大きなトレードオフです。

リファレンス実装: [ec35607](https://github.com/toyboot4e/toecs/commit/ec35607fb3d2efe71a74c3cca14dc56121fd05c5)

### 排他的アクセス

`AtomicRefCell` が中身を隠すのは、中身への `borrow` で思わぬ panic が起きるのを防ぐためだと思います。

そこでまず排他的アクセスを確保します:

```rust:lib.rs
impl World {
    pub fn display(&mut self) -> WorldDisplay { /* ~~ */ }
}
```

`WorldDisplay` は `Debug` か `Display` で中のデータをすべて表示します。

> `World` のフィールドについても `&mut self` を取って `*Display` を返すようにします。
> 
> * `ResourceMap::display(&mut self)` → `ResourceMapDisplay`
> * `ComponenPoolMap::display(&mut self)` → `ComponenPoolMap`

### 内部共変性の一時的付与

[`Display`] は `&self` を引数に取りますが、今は `Display` 実装のために可変参照が必要な場面です。そこで、データを一時的に奪って `RefCell<T>` に包み、内部共変性を与えます:

[`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html

```rust:lib.rs
impl World {
    /// Returns a debug display. This is safe because it has exclusive access.
    pub fn display(&mut self) -> WorldDisplay {
        let mut world = World::default();
        mem::swap(self, &mut world);
        WorldDisplay {
            world: RefCell::new(world),
            original_world: self,
        }
    }
}

/// See [`World::display`]
pub struct WorldDisplay<'w> {
    world: RefCell<World>,
    original_world: &'w mut World,
}

impl<'w> Drop for WorldDisplay<'w> {
    fn drop(&mut self) {
        mem::swap(self.original_world, self.world.get_mut());
    }
}
```

元ネタは Bevy の [`WorldCell`][wc] でした。

[wc]: https://docs.rs/bevy/latest/bevy/ecs/world/struct.WorldCell.html

リファレンス実装: [ebe3a0e](https://github.com/toyboot4e/toecs/commit/ebe3a0ec6ba952fd78a7d0b0221b3a8526885f03)

