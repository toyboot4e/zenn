---
title: "ECS の紹介 (例)"
---

Rust の文脈で ECS を紹介します。

覚えのある方は、この章を飛ばして次の章に移動してください。

# 例 (単純化した例)


## E, C とは

* Entity: 添字
* Component: 配列に入ったデータ

![](/images/toecs/big-array.png)

* 横列は component ストレージです。
* 縦列は entity を添字に component ストレージたちを見たとき、『盗賊』『商人』など 1 つのオブジェクトとして解釈できることを示しています。

## S とは

System とは手続きのことで、たとえば IQ を下げる system を以下のように書けます:

```rust:疑似コード1
/// 戦闘可能オブジェクトの IQ を下げる system
fn decrease_iq(
    // Intelligent 配列への可変参照 (`&mut [Intelligent]` 相当)
    mut intelligent: CompMut<Intelligent>,
    // Combat 配列への不変参照 (`&[Combat]` 相当)
    combat: Comp<Combat>,
) {
    // Combat を持つ Entity の Ingelligent について
    for intelligent in (&mut intelligent).iter().with(&combat) {
        // IQ を下げる
        intelligent.iq -= 10;
    }
}
```

この手続きは対象オブジェクトのが『何』であるかに関心がありません。人・武器・機械など、 component 組で表されたものなら何に対しても働くことができます。

## ポイント 1: SoA [^1] による表現力

1. ストレージの汎用性
  ECS では任意のオブジェクトを要素の組み合わせとして表現して保存できます。

2. 手続きの汎用性
  ECS では任意のオブジェクト (の要素) に働く手続きを定義できます。

## `World` に全ゲームデータが入っている

よくある ECS `World` は entity, comopnent に加えて _resource_ を保存します:

```rust
pub struct World {
    entities: Entities,
    components: Components,
    resources: Resources,
}
```

Resource は anymap (`TypeId` から `T` へのマップ) に入った型毎のユニークなインスタンスです。

System は component だけではなく resource も借りることができます。例:

```rust:疑似コード2
/// IQ 低下フィールドを適用する system
fn decrease_iq(
    // IQ 低下フィールドへの不変参照
    iq_field: Res<IqDecreaseField>,
    // Intelligent 配列への可変参照
    mut intelligent: CompMut<Intelligent>,
    // Combat 配列への不変参照
    combat: Comp<Combat>,
    // Body 配列への不変参照
    body: Comp<Body>,
) {
    // IQ 低下フィールドが生じていなければ終了
    if field.is_zero() {
        return;
    }

    // 肉体を持つ戦闘可能な知性は IQ 低下フィールドの影響を受ける
    for (intelligent, body) in (&mut intelligent, &body).iter().with(&combat) {
        intelligent.iq -= iq_field.get(body);
    }
}
```

## ポイント 2: System は `World` からデータを借りる手続き

* 並列実行に有利
  World から借りるデータが干渉しない system は並列実行できます。

> * 借用の分割が楽
>   ECS では個々のデータ (resource または component ストレージ) の借用が互いに干渉しません。一方で階層的にデータを保存した場合、親データの可変参照 (排他的参照) を 1 つ取れば子データにアクセスできなくなります。それはそれで面白い制約だと感じますが。

# 以上です

Rust の文脈で ECS の主な機能をお伝えしました。 [`toecs`] では、以上の機能を sparse set を中心に実装します。

[`toecs`]: https://github.com/toyboot4e/toecs

[^1]: SoA (Struct of Arrays) はオブジェクトを複数の配列に分けて保存する方法です。対して 1 種類のオブジェクトを 1 つの配列に入れる方法を AoS (Array of Structs) と呼びます。 ECS と非 ECS の最大の違いは、 SoA と AoS の違いだと思います。

