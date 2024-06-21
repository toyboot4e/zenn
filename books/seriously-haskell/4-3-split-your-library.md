---
title: "[4-3]. ライブラリのファイル分割"
---

AtCoder においては単一の `Main.hs` ファイルを提出します。自作ライブラリの管理としては、 `Main.hs` に関数や `import` を足していくのが簡単ですが、 `Main.hs` が肥大化するにつれて機能の把握が大変になります。

以下では自作ライブラリをファイル分割し、 `Main.hs` 作成時に統合 (バンドリング) する方法を紹介します。特に `haddock` で生成する API ドキュメントがモジュール毎に分割される点がメリットです。

# 例

競技プログラミング用ライブラリにリンクします。ファイル分割した際に得られるドキュメントの例としてご参照ください。

- [iota (API ドキュメント)][iota]  
  [cojna/iota] の API ドキュメントです。

- [toy-lib (API ドキュメント)][toy-lib-doc]  
  僕のライブラリ [toy-lib] の API ドキュメントです。

# バンドリングの方法

[cojna/iota] においては各モジュールを AST にパースし、関数定義を抜き出して、 `Main.hs` の末尾に追記するという形でライブラリをバンドリングしています。 [toy-lib] もほぼ同様です。

パースには外部ライブラリを使用しています。

## パーサライブラリの選択

パーサライブラリの選択においては、最新の Haskell (言語拡張) に対応しているか、関数定義を 1 行にフォーマットできるかという観点があります。

| パーサ               | 開発状況 | Haskell 2010 | GHC 2021 | 1 行にフォーマット | 例                           |
|----------------------|----------|--------------|----------|--------------------|------------------------------|
| [`ghc-lib-parser`]   | 開発中   | ✅           | ✅       | ❌                 | [cojna/iota]                 |
| [`haskell-src-exts`] | 更新停止 | ✅           | ❌       | ✅                 | 昔の [cojna/iota], [toy-lib] |

### 最新の Haskell に対応しているか

GHC は Haskell に言語拡張という形で新しい構文を追加しています。たとえば `import .. qualified as ..` という構文は [ImportQualifiedPost](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/import_qualified_post.html#extension-ImportQualifiedPost) という言語拡張であり、 [GHC 2021] によって自動的に有効化されます。

[`ghc-lib-parser`] は GHC 自体が使用するパーサですから、こうした最新の構文にすべて対応しています。 [`haskell-src-exts`] は開発が停止しており、 `MultiWayIf` や `ImportQualifiedPost` には対応していません。

### 関数定義を 1 行にフォーマットする

[`haskell-src-exts`] には関数定義を 1 行にフォーマットする機能があります。たとえば座標圧縮の関数が次のようにフォーマットされます:

```hs
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (indexer, U.map (fromJust . fst . f) xs)
  where
    !indexer = U.uniq $ U.modify VAI.sort xs
    f !x = bisect (0, pred $ U.length indexer) $ \i -> indexer U.! i <= x
```

```hs
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (indexer, U.map (fromJust . fst . f) xs) where { !indexer = U.uniq $ U.modify VAI.sort xs; f !x = bsearch (0, pred $ U.length indexer) $ \ i -> indexer U.! i <= x}
```

それぞれの行を `;` で区切ると、何千行とあるライブラリ全体を 1 行にフォーマットすることもできます。提出が見やすくなるため、僕は好んで使っています:

https://atcoder.jp/contests/abc334/submissions/48735578

すべてのコードを広義のワンライナーに整形し、異色のプレイヤーとなることも可能です [^1]:

https://atcoder.jp/contests/abc242/submissions/42313455

# まとめ

[cojna/iota] から学ぶことのできるソース統合の概要を確認しました。 [`ghc-lib-parser`] を使った場合には最新の構文を使用できます。 [`haskell-src-exts`] を使った場合には一部の更新が使えないものの、関数定義を 1 行にフォーマットすることができます。

ソース統合のより具体的な様子を確認したい場合は、 [cojna/iota] や [toy-lib] の `app/Main.hs` をご覧ください。ご自身のライブラリも同様にファイル分割・統合できるかもしません。

[^1]: なってどうする……

[cojna/iota]: https://github.com/cojna/iota
[iota]: https://cojna.github.io/iota/
[toy-lib]: https://github.com/toyboot4e/toy-lib
[toy-lib-doc]: https://toyboot4e.github.io/toy-lib/

[`ghc-lib-parser`]: https://www.stackage.org/lts-21.7/package/ghc-lib-parser-9.4.6.20230808
[`haskell-src-exts`]: https://www.stackage.org/lts-21.7/package/haskell-src-exts-1.23.1
[GHC 2021]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html

