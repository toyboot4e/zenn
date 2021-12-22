---
title: "資料室"
---

他人のふんどしです。お納めください。

# 元ネタ

## [sparsey]

Sparse set ベースの ECS ライブラリです。API が綺麗で実装も短い良いクレートでした。

[sparsey]: https://github.com/LechintanTudor/sparsey

## [ECS back and forth][ebaf]

主に [EnTT] の ECS を解説する一連の投稿です。これが元ネタ (`sparsey`) の元ネタだと思います。

[ebaf]: https://skypjack.github.io/tags/#ecs
[EnTT]: https://github.com/skypjack/entt

# ピックアップ

## ECS 実装の分類

> [What are the different ways to implement an ECS? - Entity Component System FAQ](https://github.com/SanderMertens/ecs-faq#archetypes-aka-dense-ecs-or-table-based-ecs)

Component ストレージで分類可能です。一長一短あります。

1. Bitset-based
Component ストレージは `Vec<Option<T>>` による SoA で、 [hibitset] などを活かして効率的にイテレートします。

[`specs`]: https://github.com/amethyst/specs
[hibitset]: https://docs.rs/hibitset/latest/hibitset/

2. Sparse set-based
Component ストレージは `SparseSet<T>` による SoA です。

3. Archetype-based
Component ストレージは component の組み合わせ (archetype) 毎に作り、 `Vec<T>` の SoA ストレージを使います。

## ECS 関連の記事

おすすめは:

* [EnTT] (sparse set-based) 関連の記事
  [ECS back and forth](https://skypjack.github.io/2019-02-14-ecs-baf-part-1/) (再掲)

* [flecs] (archetype-based) 関連の記事
  [Entity Component System FAQ](https://github.com/SanderMertens/ecs-faq) やリファレンスなど

> [flecs] の方はまだあまり読めていませんが……

## 有名な ECS 実装

Rust に限らず有名な ECS を列挙します。

### Sparse set-based

* [EnTT] (C++, 20,078 行; 6,000 stars)
  Minecraft にも使用される ECS ライブラリです。進んだ C++ の機能を使っており、他言語からの利用は厳しいようですが、最速の ECS として知られています。

### Archetype-based

* [flecs] (C, 40,979 行; 2,200 stars)
  Best of best と謳う人もいるフレームワークです。 C ライブラリなので [^1] どの言語からも気軽に利用できます。 UE4 と合わせて使う人たちもいます。

* [Our Machinery][machinery] (C)
  Plugin ベースのゲームエンジンで、 ECS もプラグインとして実装されています。課金するとリポジトリにアクセスできます。

* [Unity DOTS][dots] (C++ と C# ?)
  ほぼ何も知らないのですが、大量のオブジェクトを出すデモは見たことがあります。

### Hybrid (sparse set + archetype)

* [Bevy Engine][bevy] (`bevy_ecs`) (Rust, 18,838 行; 11,800 stars)
  絶賛ヒット中の Rust 製ゲームエンジンです。エンジンに対する評価とはいえ、あの EnTT の倍近い星が付いています。

[flecs]: https://github.com/SanderMertens/flecs
[EnTT]: https://github.com/skypjack/entt
[bevy]: https://bevyengine.org/
[machinery]: https://ourmachinery.com/
[dots]: https://unity.com/dots

[^1]: C 言語は (比較的 [^2] ) 安定した ABI を持っており、 C への FFI を定義できる言語が多いです。 C ライブラリが揃っているおかげで、新興の言語でも進んだライブラリを使ってゲーム開発ができます。
[^2]: たとえば `bool` が `u8` か `i8` かでブレたりします。

# ECS 以外のオブジェクト管理法

## 継承を使う場合

`Vec<Actor>` 的な Array of Structs ストレージを使います。

### E (node や actor とも)

`Entity` は scene graph 上のノードです。

### EC or C

`Entity` は `Component` の anymap です。

* C: ユーザは `Component` のみ継承できます
* EC: ユーザは `Entity` または `Component` を継承できます

## 継承を使わない場合

いわゆる [generational arena][gen] が大活躍します。

よくある `Arena<T>` は添字が世代番号を持つ `Vec<T>` です:

[gen]: https://docs.rs/generational-arena/latest/generational_arena/

```rust
pub struct Generation(NonZeroU32);

pub sturct Index<T> {
    slot: u32,
    gen: Generation,
    _ty: PhantomData<T>,
}

enum Entry<T> {
    Empty(Generation),
    Occupied(Generation, T),
}

pub struct Arena<T> {
    data: Vec<Entry<T>>,
}
```

* 空欄毎に `Generation` は単調増加します。
  既に破棄されたデータへの `Index` は無効です。

様々なオブジェクトを `Actor` にアップキャストして `Arena<Actor>` の中に入ります:

1. `struct Actor { .. }`
Optional なフィールドを活かして 1 つの型で複数種類のオブジェクトを表します。

1. `enum Actor { .. }`
`Player`, `Monster` といった variant を作ります。

1. `Box<dyn Any>`
ダウンキャストします。これは不便だと思います。

`Arena<T>` は複数種類持ってもよくて、たとえば見た目のデータを `Arena<Node>` に入れて、 `Actor` が `Index<Node>` を持ったりします。

