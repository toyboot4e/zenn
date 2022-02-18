---
title: "はじめに"
---

[Rust advent calender](https://qiita.com/advent-calendar/2021/rust) その 2 の 22 日目の記事です。

自前の ECS 実装 ([`toecs`]) を解説します。

# 背景

ECS はゲーム開発の有名設計です。具体例は、

* Minecraft [^1] や Overwatch [^2] などの有名ゲーム
* Unity DOTS, [BevyEngine][bevy] や [Machinery] などのゲームエンジン
* 個人開発のライブラリなど

[bevy]: https://bevyengine.org/
[Machinery]: https://ourmachinery.com/

しかし本格的な ECS 実装は数万行のブラックボックスになりがちです。そこで比較的短い最近のクレート ([`sparsey`]) を参考に、小さな ECS 実装 ([`toecs`]) をカッチリ作ります。

[`sparsey`]: https://github.com/LechintanTudor/sparsey
[`toecs`]: https://github.com/toyboot4e/toecs

元ネタをあたりたい方は [資料室] をご覧ください。

[資料室]: ./references

[^1]: [EnTT (C++ 製 ECS)](https://github.com/skypjack/entt)
[^2]: [Overwatch Gameplay Architecture and Netcode (Youtube)](https://www.gdcvault.com/play/1024001/-Overwatch-Gameplay-Architecture-and)

