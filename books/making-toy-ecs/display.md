---
title: "間話: 山猫部隊"
---

Rust はあまり問題を簡単にする方向には向かわず、比較的脳筋で、時にはどうしてもトリッキーなコードが必要になります。まるでリボルバーを撃つ前に必ずクルクルしないといけない病気にかかったかのようです。

それがいい！　この章では進んでトリックを使います。

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

もっと役に立つ表示を考えます。

## Entity の pretty print

`Debug` trait を手動実装し、 `Entity(slot, generation)` の形で表示されるようにしました。

リファレンス実装: [997d2c6](https://github.com/toyboot4e/toecs/commit/997d2c6dca83bc44cf9d6257c79e5eb1f5b46972)

## `AtomicRefCell` の中を覗く debug print

### `Debug` を要求

中のデータが表示可能でないと意味がないので、制約を加えました:

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

この後は `AtomicRefCell` の中身を借りても安全なので、 `WorldDisplay` は `Debug` か `Display` で中のデータをすべて表示します。

> 同様のパターンで:
> 
> * `ResourceMap::display(&mut self)` → `ResourceMapDisplay`
> * `ComponentPoolMap::display(&mut self)` → `ComponentPoolMapDisplay`

### 内部可変性の一時的付与

[`Display::fmt`] は `&self` を引数に取りますが、今は `Display` 実装のために可変参照が必要な場面です。そこで、元のデータを一時的に奪って `RefCell<T>` に包み、内部可変性を与えます:

[`Display::fmt`]: https://doc.rust-lang.org/std/fmt/trait.Display.html#tymethod.fmt

```rust:lib.rs
impl World {
    pub fn display(&mut self) -> WorldDisplay {
        // 空データと交換することで所有権を奪う
        let mut world = World::default();
        mem::swap(self, &mut world);
        WorldDisplay {
            // 奪ったデータを `RefCell` に包んで `WorldDisplay` で利用する
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
        // drop 時に元のデータを元の位置に返す
        mem::swap(self.original_world, self.world.get_mut());
    }
}


// `Debug` や `Display` の実装で `&mut World` を使うことができる
// (`ResourceMap::display(&mut self)` や `ComponentMapPool::display(&mut self)` が呼べる)
```

元ネタは Bevy の [`WorldCell`][wc] でした。

[wc]: https://docs.rs/bevy/latest/bevy/ecs/world/struct.WorldCell.html

リファレンス実装: [ebe3a0e](https://github.com/toyboot4e/toecs/commit/ebe3a0ec6ba952fd78a7d0b0221b3a8526885f03)

