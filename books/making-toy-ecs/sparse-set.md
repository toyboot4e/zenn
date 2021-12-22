---
title: "ストレージ 2: Sparse set"
---

Sparse set は 2 段構えの配列です。

スカスカな添字列を経由して密なデータ列にアクセスします。

# 脱 `Vec<Option<T>>`

最も単純な ECS は、それぞれの型の component を `Vec<Option<T>>` に保存します:

![](/images/toecs/big-array.png)

> 横列が `Vec<Option<T>>` です。

これは ECS の概念としては完全に正しいのですが、 ECS ではすべての `Entity` を 1 つの SoA [^1] に保存しますから、実用上は問題が出ます [^2]:

* メモリ消費量が多い
たとえばパーティクルみたいな `Entity` がいると、大量の空欄でメモリが埋まります。

* 巡回 (イテレーション) が遅い
空欄が多いとキャッシュミスが増えます。 (AAA ゲームでもなければ問題にならないようですが) 。

そこで図の横列に相当するストレージを作り直します。

# `SparseSet<T>` の紹介

[`toecs`] では sparse set を使います。

Sparse set に興味が無い場合は、並列実行の章まで飛んでください。

[`toecs`]: https://github.com/toyboot4e/toecs

## 疎な添字、密なデータ列

`Vec<Option<T>>` の添字には必ずしも対応するデータが存在しません。このような添字のことを sparse index (疎な添字) と呼びます。

ECS で言えば `ComponentPool<T>` に対する `Entity` が sparse index です。

* Compoment は密なデータ列 (`Vec<T>`) に入れます。
* Component は疎な添字 (`SparseIndex`) 経由でアクセスできるようにします。

## 添字のマッピング

間接層を入れ、疎な添字を密なデータ列への添字に写します:

![](/images/toecs/sparse-set.png)

> データが密な配列に入るため、 sparse set は `DenseVec` としても知られています。

*  `(SparseIndex, &T)` のイテレータも作れるように `Vec<SparseIndex>` を持っています。

## イテレーション効率

### 1 種類の `SparseSet<T>` をイテレートする場合

Component は密なデータ列 (`Vec<T>`) であり、最速でイテレートできます。

### 複数種類の `SparseSet<T>` をイテレートする場合

1. `SparseIndex` 経由でイテレートする場合
  間接層を経由したアクセスのため、キャッシュミスが増えますがイテレート可能です。

2. `usize` 経由でイテレートする場合
  後の章に登場する『グループ』のトリックを使えば、共通の `usize` で複数の `SparseSet<T>` にアクセスできます。効率が必要なときに使います。

# `SparseSet<T>` の実装

## 空欄のリサイクル

世代番号には [std::num][num] の `NonZero*` を使います:

```rust:sparse.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Generation {
    raw: NonZeroU32,
}

assert_eq!(size_of::<SparseIndex>() == size_of::<Option<SparseIndex>>());
```

世代付き添字です:

[ga]: https://docs.rs/generational-arena/latest/generational_arena/

```rust:sparse.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SparseIndex {
    slot: u32,
    generation: NonZeroU32,
}
```

[num]: https://doc.rust-lang.org/std/num/index.html

`SparseSet<T>` は図の通り:

```rust:sparse.rs
#[derive(Debug, Clone)]
pub struct SparseSet<T> {
    // SparseIndex → DenseIndex
    to_dense: Vec<Option<SparseIndex>>,
    // iter_with_index() の実装に使う
    to_sparse: Vec<SparseIndex>,
    // dense vec, packed array
    data: Vec<T>,
}
```

アクセサでは添字の世代を考慮します。世代が不一致な添字は無効な添字です。

```rust:sparse.rs
impl<T> SparseSet<T> {
    pub fn get(&self, sparse: SparseIndex) -> Option<&T> {
        let dense = self.to_dense.get(sparse)?;
        debug_assert!(sparse.gen <= dense.gen);
        if dense.gen == sparse.gen {
            Some(&self.data[dense.to_usize()])
        } else {
            None
        }
    }
}
```

世代番号の増加は次章の `EntityPool` で行います。

## `swap_remove`

* [`Vec::remove`][rm] は右側の要素をシフトするため重そうです。
* [`Vec::swap_remove`][swap_rm] を使って要素を削除します。

[rm]: https://doc.rust-lang.org/std/vec/struct.Vec.html#method.remove
[swap_rm]: https://doc.rust-lang.org/std/vec/struct.Vec.html#method.swap_remove

リファレンス実装: [f5b49aa](https://github.com/toyboot4e/toecs/commit/f5b49aae3dbcb6d3a22a94f04fa91c4dd618422e)

[^1]: SoA (Struct of Arrays) は (ECS の文脈では) component の種類毎に `Storage<T>` を作ることです。一方、 AoS (Array of Structs) では `Vec<Actor>` (`Actor { Body, Intelligent, Combat }`) のように 1 種類のストレージを使います。一部のデータにのみ関心がある場合、 AoS はキャッスミスが増えて遅くなる傾向があります。

[^2]: `Vec<Option<T>>` にも component の挿入・削除の自由度が高いなどのメリットがあります。またページ制ストレージを導入すれば極端にメモリを使い過ぎることも無いらしく、シンプルでイケてるんじゃないかと思います。

