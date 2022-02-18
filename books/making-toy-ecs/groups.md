---
title: "[WIP] グループによる高速化"
---

**クライマックス** です。

単純な並び替えにより sparse set 間で添字を連動させます。

# 概要

## 現状のイテレータ

イテレータの章では `Entity(SparseIndex)` 経由のイテレータを作りました。 `T` へのアクセスは 2 段階です:

* `SparseIndex` → `DenseIndex`
* `DenseIndex` → `T`

しかしこの処理ではメモリジャンプが発生し、喩えるなら連結リストに匹敵する遅さになります。データ数が増えればボトルネックになることもあるでしょう。

## 最速のイテレータへ

そこで component 列をソートする仕組み、グループの登場です:

![](/images/toecs/groups.png)

* 横列は component ストレージです
* 縦列は、グループでソートされた範囲では同じ entity の component を指します

## グループは明らかに銀の矢ではない

グループは指定された component 組のイテレーションを最速にします。しかし指定可能な component 組の制限として、一直線の包含関係でなければならないというのがあります。

たとえば `(A, B, C)` というグループと `(A, B, D)` というグループは両立できません。 `(A, B, C)` と `(A, B, C, D)` なら OK です。

# グループの実装

## Family: 包含関係にある group の配列

たとえば `(A, B)`, `(A, B, C)`, `(A, B, C, D, E)` の 3 グループが指定されたら、これらをまとめて 1 つの component family とみなします。

リファレンス実装: 

## 状態の更新

Component 追加で所属 group が変わった場合:

![](/images/toecs/group-sync.png)

Component 削除で所属 group が変わった場合:

![](/images/toecs/group-unsync.png)

リファレンス実装: 

# `DenseIter` の実装

リファレンス実装: 

