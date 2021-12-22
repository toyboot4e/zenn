---
title: "ESC の紹介 (まとめ)"
---

前章のまとめです。

# 主な概念

[`toecs`] では以下のように解釈します。

[`toecs`]: https://github.com/toyboot4e/toecs

## E, C, S とは

* **E**ntity: オブジェクトの ID
* **C**omponent: オブジェクトの構成要素
* **S**ystem: World からデータを借りる手続き

## Resource とは

* 型毎のユニークなインスタンス

## World とは

* すべてのゲームデータ [^1]

# ECS ([`toecs`]) の特徴

## ストレージの汎用性

* 任意のオブジェクトを component の組み合わせとして表現できる
* Resource として component 以外のデータも追加できる

## System の有効性

* 要素に働くため汎用性が高い
* 並列実行しやすい
  * System (=関数) が World から借りるデータはシグネチャで分かる
  * 並列実行できる system の組とは World から借りるデータが干渉しないもの

[^1]: 『World は in-memory DB で、 system はクエリを発行する手続き』という解釈もあるようです。

