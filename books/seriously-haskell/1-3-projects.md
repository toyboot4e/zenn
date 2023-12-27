---
title: "[1-3] 📜 プロジェクト作成"
---

毎週開催の AtCoder Beginner Contest に備え、より本格的に環境構築します。特に言語サーバが動くと、変数の型を表示できて助かります。

# ビルドツールの必要性

Cabal や Stack といったビルドツールが必要な理由は以下です。

1. 言語サーバが動くようになる
2. 外部パッケージが利用できるようになる

## 外部パッケージに関して

GHC に伴ってプレインストールされるパッケージは以下です:

:::details ghc-pkg list
```sh
$ ghc-pkg list
/nix/store/px7f7jv7fwjaqkhdc7xl8hm7naiw1xmc-ghc-9.4.5/lib/ghc-9.4.5/package.conf.d
    Cabal-3.8.1.0
    Cabal-syntax-3.8.1.0
    array-0.5.4.0
    base-4.17.1.0
    binary-0.8.9.1
    bytestring-0.11.4.0
    containers-0.6.7
    deepseq-1.4.8.0
    directory-1.3.7.1
    exceptions-0.10.5
    filepath-1.4.2.2
    (ghc-9.4.5)
    ghc-bignum-1.3
    ghc-boot-9.4.5
    ghc-boot-th-9.4.5
    ghc-compact-0.1.0.0
    ghc-heap-9.4.5
    ghc-prim-0.9.0
    ghci-9.4.5
    haskeline-0.8.2
    hpc-0.6.1.0
    integer-gmp-1.1
    libiserv-9.4.5
    mtl-2.2.2
    parsec-3.1.16.1
    pretty-1.1.3.6
    process-1.6.16.0
    rts-1.0.2
    stm-2.5.1.0
    system-cxx-std-lib-1.0
    template-haskell-2.19.0.0
    terminfo-0.4.1.5
    text-2.0.2
    time-1.12.2
    transformers-0.5.6.2
    unix-2.7.3
    xhtml-3000.2.2.1
```
:::

> [5.9. Packages — Glasgow Haskell Compiler 9.4.5 User's Guide](https://downloads.haskell.org/ghc/9.4.5/docs/users_guide/packages.html)

その他のパッケージ、 [`extra`] や [`vector`] などをローカルで使用する場合は、ビルドツール ([`stack`] または [`cabal`]) に頼ることになります。

# プロジェクト構成の選択

HLS を動作させるため、 Cabal プロジェクトまたは Stack プロジェクトを前提とします。

## ビルドツールの選択

ビルドツールには [`stack`] および [`cabal`] があります。時代の流れは [`cabal`] (+ [`cabal-fmt`]) に向かっていると思います。

- Stack: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/)  
  Stack のドキュメントは難解ですが、実際に使ってみるとハマり所は少ないと思います。

- Cabal: [Cabal 3.11.0.0 User's Guide](https://cabal.readthedocs.io/en/latest/index.html)  
  Cabal のドキュメントは非常によく書かれていますが、実際に使ってみるとハマりました。

参考:

https://zenn.dev/autotaker/articles/haskell-setup-2021

https://zenn.dev/mod_poppo/articles/haskell-setup-2023

## `Main.hs` のヘッダ部

`Main.hs` をスクリプト実行するとリンク時間を節約できるため、素早く動作確認できます。スクリプト実行 (bytecode interpreter による実行) のためには、 REPL または Stack / Cabal スクリプトを使います。

`Main.hs` の頭に特殊な形式でヘッダを書くと、 Stack / Cabal スクリプトファイルになります。通常の `runghc` とは異なり、外部パッケージも使用できるのがメリットです:

https://zenn.dev/mod_poppo/articles/haskell-script

ただしコンパイル実行 [^1] と `Main.hs` のスクリプトファイル化を両立できるのは、 Stack プロジェクトのみです。 Cabal プロジェクトでは以下のエラーとなりました:

```sh
$ # コンパイル実行: 可能
$ cabal run
Hello, Haskell!

$ # スクリプト実行: 不可能
$ cabal run app/Main.hs
Error: cabal: The run command can only run an executable as a whole, not files
or modules within them, but the target 'app/Main.hs' refers to the file
app/Main.hs in the executable cabal-sample.
```

## まとめ

プロジェクト構成の主なパラメータをビルドツールおよび `Main.hs` のスクリプト化としました。スクリプト化は stack プロジェクトでのみ有効でした。

| ビルドツール | スクリプト化     | コンパイル実行 [^1] | スクリプト実行 | HLS |
|--------------|------------------|---------------------|----------------|-----|
| Cabal        | なし             | ✅                  | ❌             | ✅  |
| Cabal        | Cabal スクリプト | ✅                  | ❌ ※           | ✅  |
| Stack        | なし             | ✅                  | ❌             | ✅  |
| Stack        | Stack スクリプト | ✅                  | ✅             | ✅  |

ただ僕以外に stack / cabal スクリプトを提出している人を見たことがありません。スクリプト化はあまり重要ではないかもしれません。

# サンプルプロジェクト

ひとまず動作するであろうサンプルプロジェクトを作成しました。細かい説明は `README.md` をご覧ください。

## Stack プロジェクト

https://github.com/toyboot4e/seriously-haskell/tree/main/stack-sample

## Cabal プロジェクト

https://github.com/toyboot4e/seriously-haskell/tree/main/cabal-sample

[`extra`]: https://www.stackage.org/lts-21.7/package/extra-1.7.14
[`vector`]: https://www.stackage.org/lts-21.7/package/vector-0.13.0.0

[`cabal`]: https://cabal.readthedocs.io/en/stable/
[`cabal-fmt`]: https://github.com/phadej/cabal-fmt
[`stack`]: https://docs.haskellstack.org/en/stable/GUIDE/

[`haddock`]: https://github.com/haskell/haddock
[`doctest`]: https://github.com/sol/doctest
[`quickcheck`]: https://github.com/nick8325/quickcheck

[^1]: (ネイティブコードへの) コンパイル実行

