---
title: "ストレージ 1: Resource"
---

Resource は World が持つ最も単純なデータです。

この章の 200 行を経験すれば ECS を自作する見通しが立つと思います。

# Resource とは

* Anymap (`TypeId` → `T`) に入ったデータ、型毎のユニークなインスタンス
* いわば World の動的フィールド

Resource を引数に取る関数に対して `run` メソッドを実装します:

```rust:例
fn simple_system(u: ResMut<usize>) {
    *u += 20;
}

simple_system.run(&world);
```

# Anymap の実装

## プロジェクトの用意

```sh
$ cargo new --lib toecs
$ cd toecs
$ # ファイル作成
$ : > src/{res,tests,sys}.rs
```

依存クレートを追加します:

```sh
$ # cargo install cargo-edit
$ cargo add rustc_hash
$ cargo add atomic_refcell
```

* [rustc_hash] は [FxHashMap][h] を提供します。
Anymap の実装に使います。 `HashMap` よりも ~~名前がかっこいい~~ [速い][p] です。

* [atomic_refcell] は `AtomicRefCell` を提供します。
`Rc<RefCell<T>>` のマルチスレッド版と読めます。 `Arc<RwLock<T>>` と比べて ~~違いが分からない~~ 便利で速いです。

[h]: https://qiita.com/hatoo@github/items/1c627987991a0156d26c
[p]: https://nnethercote.github.io/perf-book/hashing.html

[rustc_hash]: https://docs.rs/rustc-hash/
[atomic_refcell]: https://docs.rs/atomic_refcell/

ユニットテストは専用のファイルに書きます:

```rust:lib.rs
#[cfg(test)]
mod tests;
```

ファイル分けすることで、テストを書き換えてもライブラリ部分は再コンパイルされなくなります。

> [matklad 氏のブログ記事][a] にある通りです。

[a]: https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html#assorted-tricks

## 型を用意

`World` から書き始めます:

```rust:lib.rs
#[cfg(test)]
mod tests;

pub mod res;

use crate::res::ResourceMap;

#[derive(Debug, Default)]
pub struct World {
    res: ResourceMap,
}
```

`ResourceMap` は anymap です。中から複数の可変参照を取れるように内部可変性 (`AtomicRefCell<T>`) を使います:

```rust:res.rs
use std::{
    any::{self, Any, TypeId},
    ops,
};

use atomic_refcell::{AtomicRef, AtomicRefCell, AtomicRefMut};
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub struct ResourceMap {
    cells: FxHashMap<TypeId, AtomicRefCell<AnyResource>>,
}

#[derive(Debug)]
struct AnyResource {
    any: Box<dyn Any>,
}
```

[Deref] な専用型を用意します:

[Deref]: https://doc.rust-lang.org/std/ops/trait.Deref.html

```rust:res.rs
// ResourceMap::borrow<T>(&self) で返ってくる型。 Deref を実装する
#[derive(Debug)]
pub struct Res<'r, T> {
    borrow: AtomicRef<'r, T>,
}

// ResourceMap::borrow_mut<T>(&self) で返ってくる型。 DerefMut を実装する
#[derive(Debug)]
pub struct ResMut<'r, T> {
    borrow: AtomicRefMut<'r, T>,
}
```

あとは `impl` ブロックを書いてテストが通るようにします。

リファレンス実装: [1a3ee73](https://github.com/toyboot4e/toecs/commit/1a3ee7337716989ea24438a5c378175710f1eb29)

# System に resource を割り当てる

`Res<T>` や `ResMut<T>` を引数に取る関数に以下 trait を実装します [^1]:

```rust:sys.rs
pub unsafe trait System<'w, Params> {
    unsafe fn run(&mut self, w: &'w World);
}
```

## `Res<T>` と `ResMut<T>` を抽象する

『`Res<T>` または `ResMut<T>`』を表す trait を用意します:

```rust:sys.rs
pub trait BorrowWorld<'w> {
    unsafe fn borrow(w: &'w World) -> Self;
}
```

実装します:

```rust:sys.rs
impl<'w, T: 'static> BorrowWorld<'w> for Res<'w, T> {
    unsafe fn borrow(w: &'w World) -> Self {
        w.res.borrow()
    }
}

impl<'w, T: 'static> BorrowWorld<'w> for ResMut<'w, T> {
    unsafe fn borrow(w: &'w World) -> Self {
        w.res.borrow_mut()
    }
}
```

これで `System` を実装できます。

## `System` の手動実装

まずは手書きしてみます:

```rust:src/sys.rs
unsafe impl<'w, P0, P1, F> System<'w, (P0, P1)> for F
where
    F: FnMut(P0, P1),
    P0: BorrowWorld<'w>,
    P1: BorrowWorld<'w>,
{
    unsafe fn run(&mut self, w: &'w World) {
        (self)(P0::borrow(w), P1::borrow(w))
    }
}
```

## `System` のマクロ実装

上記実装を一般化して、任意の `(P0, P1, .., PN)` について `System` を実装するマクロを組みます:

```rust:sys.rs
unsafe impl<'w, $($xs),+, F> System<'w, ($($xs,)+), ()> for F
//                                      ※ (T,) は 1 要素のタプル、 (T) は T と同じ
//                                        T に実装すると他の impl と衝突するため注意
where
    F: FnMut($($xs),+),
    $($xs: BorrowWorld<'w>),+
{
    unsafe fn run(&mut self, w: &'w World) {
        (self)(
            $(<$xs as BorrowWorld>::borrow(w),)+
        );
    }
}
```

* [繰り返し - Rust By Example 日本語版](http://doc.rust-jp.rs/rust-by-example-ja/macros/repeat.html) の構文を知っていれば読めます。

マクロを使って `System` trait を実装します:

```rust:sys.rs
impl_run!(P0);
impl_run!(P1, P0);
// ~~
impl_run!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15);
```

これで N 個の resource を引数に取る関数に対して `trait System` を実装できました。

## マクロの再帰呼び出し

`impl_run!` が 16 行並ぶのは気持ち悪いので、 1 行で書けるようにします:

```rust:sys.rs
macro_rules! recursive {
    ($macro:ident, $arg:ident) => {
        $macro!($arg);
    };
    ($macro:ident, $first:ident, $($rest:ident),*) => {
        $macro!($first, $($rest),*);
        recursive!($macro, $($rest),*);
    };
}

recursive!(impl_run, P15, P14, P13, P12, P11, P10, P9, P8, P7, P6, P5, P4, P3, P2, P1, P0,);

// 以下と等価:

// impl_run!(P15, P14, P13, P12, P11, P10, P9, P8, P7, P6, P5, P4, P3, P2, P1, P0);
// impl_run!(P14, P13, P12, P11, P10, P9, P8, P7, P6, P5, P4, P3, P2, P1, P0);
// ~~~~
// impl_run!(P1, P0);
// impl_run!(P0);

// 引数の順番が逆 (P15, P14, .. の降順) になっているが問題ない
```

リファレンス実装: [1a3ee73](https://github.com/toyboot4e/toecs/commit/1a3ee7337716989ea24438a5c378175710f1eb29)

Component ストレージを借りるための型も `BorrowWorld` で表せますから、 system が World からデータを借りる仕組みは大体分かったと言えます。

## 備考: マクロの構文

マクロがコードブロックを受け取るように見せるとよりお洒落です:

```rust
recursive! {
    impl_run!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15);
}
```

[^1]: `Params` が関連型 (`type Params;`) でない理由は、 `System` 実装で unconstraint ~ とエラーが出たためです。 Rust 難しいですね。

