---
title: "イテレータ"
---

ポイントだけメモします。

何を言っているのか伝わらないかもしれません。実装も酷いものですみません 🙇

# 例

`Comp<T>` や `CompMut<T>` の組からイテレータが作れるようになります:

```rust
fn add_system(mut us: CompMut<usize>, is: Comp<isize>, add: Res<usize>) {
    for (u, i) in (&mut us, &is).iter() {
        *u += (-*i) as usize + *add;
    }
}

add_system.run(&world);
```

`&CompMut<T>` は `&Comp<T>` と同じ扱いとし、 `&mut CompMut<T>` から区別します。

# 1 種類の component のイテレータ

こちらは `[T]::iter` に準じる速いイテレータです。

## `Comp<T>` と `CompMut<T>` を抽象する

`trait View` を追加しました。 `Comp<T>` を `SparseSet<T>` の疎な部分と密な部分に分解できます:

```rust
/// `&Comp<T>` | `&CompMut<T>` | `&mut CompMut<T>`
pub unsafe trait View<'a> {
    type Binding: AnyBinding;
    fn into_parts(self) -> (&'a [Entity], Self::Binding);
}

/// Shorthand
type ViewItem<'a, V> = <<V as View<'a>>::Binding as AnyBinding>::Item;

/// `Binding<&[T]>` | `Binding<&mut [T]>`
pub trait AnyBinding {
    type Item;
    fn get(&mut self, ent: Entity) -> Option<Self::Item>;
    unsafe fn get_by_slot_unchecked(&mut self, slot: usize) -> Self::Item;
}

#[derive(Clone)]
pub struct Binding<'a, Slice> {
    to_dense: &'a [Option<DenseIndex>],
    data: Slice,
}
```

> `sparsey` や `shipyard` では、そもそも `Comp<T>` と `CompMut<T>` を `struct ComponentView<T>` で表しています。

## Lifetime を誤魔化す

Mutable iterator の実装が難しく、ズルをしました 🙇

```rust
// `&'_ mut self` から `&'a mut T` を返すアクセサ
impl<'a, T> AnyBinding for Binding<'a, &'a mut [T]> {
    type Item = &'a mut T;
    // ポインタ経由のキャストで実装
    fn get(&mut self, ent: Entity) -> Option<Self::Item> { /* ~~ * /}
    unsafe fn get_by_slot_unchecked(&mut self, slot: usize) -> Self::Item { /* ~~ * /}
}
```

[miri] には怒られなかったので、極端に酷い間違いはしていない……と思いたいです……

[miri]: https://github.com/rust-lang/miri

## `(Entity, Item)` のイテレータも作れるようにする

`sparsey` を真似て `(&a, &b).iter().entities()` と書けるようにしました。

## `InteIterator` は実装できない？

Unconstrained lifetime と言われたのでライフタイム付きの `IntoIterator` を作りました:

```rust
pub trait Iter<'a> {
    type I;
    fn iter(self) -> Self::I;
}
```

ユーザは必ず `.iter()` を呼びます。

> `IntoIterator` を実装する良いトリックがあるかもしれませんが、 Bevy の API もこんな感じだったので無理かもしれません。

リファレンス実装: [6b94be6](https://github.com/toyboot4e/toecs/commit/6b94be632116360120b17191d9858f8a18c2daf1)

# 複数種類の component のイテレータ

`SparseIndex` 経由のイテレータです。速度は残念ですが、イテレーションの API は完成します。

> 複数種類の component の高速なイテレーションは『グループ』の章で扱います。

## 関連型でコンパイルが通らないときは generics を使う

もう Rust 分からないですね……

## ゼロコスト具象

Const generics で脳筋しました。

リファレンス実装: [fdd3534](https://github.com/toyboot4e/toecs/commit/fdd3534bd1013036e481c9c6459b5b1ef41d915b)

