---
title: "Haskeller のための AtCoder 言語アップデート 2025"
topics: [haskell, atcoder]
emoji: 🆕
type: idea
published: true
---



# 背景

AtCoder では、ジャッジ環境 (コンパイル・実行環境) を (ユーザの意向に沿って) 更新するイベント『言語アップデート』が隔年で実施されています。

先日 [2024-25 年度の言語アップデート](https://atcoder.jp/posts/1342) における提出が締め切られました。今後の予定は概ね以下のようです:

-   2025 年 9 月下旬〜の [AtCoder Daily Training](https://atcoder.jp/contests/adt_top) で新ジャッジが運用されます。
-   2025 年 10 月中旬を目処に本番のコンテストで新ジャッジが運用されます。

以下では更新に備え、 Haskell のジャッジ環境の設定内容と、以前と比べてどう変化するかを確認します。


# 言語アップデート 2025 の内容

[2025 年 提出ファイル](https://gist.github.com/toyboot4e/04b902271997b3c865cc91014720ec40) (`installscript.toml`) の内容をまとめます。

> 参考: [2023 年 提出ファイル](https://gist.github.com/gksato/67ce95de203c1799841a99777758e2f5) (`1-install.sh`)


## ファイル構成

ジャッジ環境の OS は Ubuntu 24.04 です。 Haskell の場合、ホームディレクトリ以下の `submission` プロジェクトで提出コードがビルドされます:

```sh
/home/runner/submission
├── app/
│   └── Main.hs          # ユーザの提出コード
├── cabal.project        # 下記参照
├── cabal.project.freeze # 下記参照
├── cabal.project.local  # 下記参照
├── submission.cabal     # 下記参照
└── dist-newstyle/       # cabal のビルドディレクトリ
```


### ファイル内容

以下のファイルが生成されます:

:::details submission.cabal (依存ライブラリのバージョン指定など)
```yaml
cabal-version:      3.4
name:               submission
version:            0.1.0.0
synopsis:           A Haskell program submitted to AtCoder
-- description:
license:            NONE
author:             submitter-anonymous
maintainer:         NONE
-- copyright:
category:           Competitive
build-type:         Simple
-- extra-doc-files:    CHANGELOG.md
-- extra-source-files:
common warnings
    ghc-options: -Wall
flag atcoder
    description:    Indicates this is on the AtCoder judge server
    default:        False
    manual:         True
executable main
    import:           warnings
    main-is:          Main.hs
    -- other-modules:
    -- other-extensions:
    build-depends:
                  Cabal ^>=3.16.0.0,
                  Cabal-syntax ^>=3.16.0.0,
                  QuickCheck ^>=2.16.0.0,
                  ac-library-hs ^>=1.5.3.0,
                  adjunctions ^>=4.4.3,
                  array ==0.5.8.0,
                  attoparsec ^>=0.14.4,
                  base ==4.19.2.0,
                  bifunctors ^>=5.6.2,
                  binary ^>=0.8.9.3,
                  bitvec ^>=1.1.5.0,
                  bytestring ^>=0.12.2.0,
                  comonad ^>=5.0.9,
                  containers ^>=0.8,
                  contravariant ^>=1.5.5,
                  deepseq ==1.5.1.0,
                  directory ^>=1.3.9.0,
                  distributive ^>=0.6.2.1,
                  exceptions ^>=0.10.7,
                  extra ^>=1.8,
                  fgl ^>=5.8.3.0,
                  filepath ^>=1.4.301.0,
                  flow ^>=2.0.0.9,
                  free ^>=5.2,
                  ghc-bignum ==1.3,
                  ghc-boot-th ==9.8.4,
                  ghc-prim ==0.11.0,
                  hashable ^>=1.5.0.0,
                  heaps ^>=0.4.1,
                  hmatrix ^>=0.20.2,
                  hmatrix-glpk ^>=0.19.0.0,
                  hmatrix-gsl ^>=0.19.0.1,
                  hmatrix-special ^>=0.19.0.0,
                  ilist ^>=0.4.0.1,
                  indexed-traversable ^>=0.1.4,
                  indexed-traversable-instances ^>=0.1.2,
                  integer-gmp ^>=1.1,
                  integer-logarithms ^>=1.0.4,
                  kan-extensions ^>=5.2.7,
                  lens ^>=5.3.5,
                  linear-base ^>=0.5.0,
                  list-t ^>=1.0.5.7,
                  massiv ^>=1.0.5.0,
                  megaparsec ^>=9.7.0,
                  monad-memo ^>=0.5.4,
                  mono-traversable ^>=1.0.21.0,
                  mtl ^>=2.3.1,
                  mutable-containers ^>=0.3.4.1,
                  mwc-random ^>=0.15.2.0,
                  parallel ^>=3.2.2.0,
                  parsec ^>=3.1.18.0,
                  parser-combinators ^>=1.3.0,
                  pretty ^>=1.1.3.6,
                  primitive ^>=0.9.1.0,
                  process ^>=1.6.26.1,
                  profunctors ^>=5.6.3,
                  psqueues ^>=0.2.8.2,
                  random ^>=1.3.1,
                  reflection ^>=2.1.9,
                  regex-tdfa ^>=1.3.2.4,
                  safe-exceptions ^>=0.1.7.4,
                  scientific ^>=0.3.8.0,
                  semialign ^>=1.3.1,
                  semigroupoids ^>=6.0.1,
                  split ^>=0.2.5,
                  stm ^>=2.5.3.1,
                  strict ^>=0.5.1,
                  strict-lens ^>=0.4.1,
                  tagged ^>=0.8.9,
                  template-haskell ==2.21.0.0,
                  text ^>=2.1.3,
                  tf-random ^>=0.5,
                  these ^>=1.2.1,
                  these-lens ^>=1.0.2,
                  time ^>=1.12.2,
                  transformers ^>=0.6.1.0,
                  trifecta ^>=2.1.4,
                  unboxing-vector ^>=0.2.0.0,
                  unix ==2.8.6.0,
                  unordered-containers ^>=0.2.20,
                  utility-ht ^>=0.0.17.2,
                  vector ^>=0.13.2.0,
                  vector-algorithms ^>=0.9.1.0,
                  vector-split ^>=1.0.0.4,
                  vector-stream ^>=0.1.0.1,
                  vector-th-unbox ^>=0.2.2,
                  wide-word ^>=0.1.7.1,
                  witherable ^>=0.5,
                  xhtml ^>=3000.2.2.1
    hs-source-dirs:   app
    default-language: GHC2021
    if flag(atcoder)
      cpp-options: -DATCODER
```
:::

:::details cabal.project (フラグ設定、最適化設定など)
```yaml
packages: ./submission.cabal
constraints: bitvec +libgmp,
             -- clock +llvm,
             text +simdutf,
             vector-algorithms +llvm
optimization: 2
package *
    compiler: ghc
    ghc-options: -fllvm -Wall
allow-newer: witherable:containers,
             semialign:containers,
             scientific:containers,
             parallel:containers,
             megaparsec:containers,
             foldl:containers,
             linear-generics:containers,
             indexed-traversable:containers,
             hashable:containers
```
:::

:::details cabal.project.local (atcoder フラグの設定)
`cabal v2-configure --flags="+atcoder"` の実行結果、以下のファイルが生成されます:

```yaml
ignore-project: False
flags: +atcoder
```
:::

:::details cabal.project.freeze (バージョンロックファイル)
```yaml
active-repositories: hackage.haskell.org:merge
constraints: any.Cabal ==3.16.0.0,
             Cabal -git-rev,
             any.Cabal-syntax ==3.16.0.0,
             any.OneTuple ==0.4.2,
             any.QuickCheck ==2.16.0.0,
             QuickCheck -old-random +templatehaskell,
             any.StateVar ==1.2.2,
             any.ac-library-hs ==1.5.3.0,
             any.adjunctions ==4.4.3,
             any.alex ==3.5.4.0,
             any.ansi-terminal ==1.1.3,
             ansi-terminal -example,
             any.ansi-terminal-types ==1.1.3,
             any.array ==0.5.8.0,
             any.assoc ==1.1.1,
             assoc -tagged,
             any.atomic-primops ==0.8.8,
             atomic-primops -debug,
             any.attoparsec ==0.14.4,
             attoparsec -developer,
             any.base ==4.19.2.0,
             any.base-orphans ==0.9.3,
             any.bifunctors ==5.6.2,
             bifunctors +tagged,
             any.binary ==0.8.9.3,
             any.bitvec ==1.1.5.0,
             bitvec +simd,
             any.blaze-builder ==0.4.4.1,
             any.blaze-html ==0.9.2.0,
             any.blaze-markup ==0.8.3.0,
             any.bytestring ==0.12.2.0,
             bytestring -pure-haskell,
             any.call-stack ==0.4.0,
             any.case-insensitive ==1.2.1.0,
             any.charset ==0.3.12,
             any.clock ==0.8.4,
             clock -llvm,
             any.colour ==2.3.6,
             any.comonad ==5.0.9,
             comonad +containers +distributive +indexed-traversable,
             any.containers ==0.8,
             any.contravariant ==1.5.5,
             contravariant +semigroups +statevar +tagged,
             any.data-default-class ==0.1.2.2,
             any.deepseq ==1.5.1.0,
             any.directory ==1.3.9.0,
             directory -os-string,
             any.distributive ==0.6.2.1,
             distributive +semigroups +tagged,
             any.exceptions ==0.10.7,
             any.extra ==1.8,
             any.fgl ==5.8.3.0,
             fgl +containers042,
             any.file-io ==0.1.5,
             file-io -os-string,
             any.filepath ==1.4.301.0,
             filepath -cpphs,
             any.fingertree ==0.1.6.2,
             any.flow ==2.0.0.9,
             flow -pedantic,
             any.foldl ==1.4.18,
             any.free ==5.2,
             any.ghc-bignum ==1.3,
             any.ghc-boot-th ==9.8.4,
             any.ghc-prim ==0.11.0,
             any.hashable ==1.5.0.0,
             hashable -arch-native -random-initial-seed,
             any.heaps ==0.4.1,
             any.hmatrix ==0.20.2,
             hmatrix -disable-default-paths -no-random_r -openblas,
             any.hmatrix-glpk ==0.19.0.0,
             hmatrix-glpk -disable-default-paths,
             any.hmatrix-gsl ==0.19.0.1,
             hmatrix-gsl -disable-default-paths +onlygsl,
             any.hmatrix-special ==0.19.0.0,
             hmatrix-special -safe-cheap,
             any.ilist ==0.4.0.1,
             any.indexed-traversable ==0.1.4,
             any.indexed-traversable-instances ==0.1.2,
             any.integer-gmp ==1.1,
             any.integer-logarithms ==1.0.4,
             integer-logarithms -check-bounds +integer-gmp,
             any.invariant ==0.6.4,
             any.kan-extensions ==5.2.7,
             any.lens ==5.3.5,
             lens -benchmark-uniplate -dump-splices +inlining -j +test-hunit +test-properties +test-templates +trustworthy,
             any.linear-base ==0.5.0,
             any.linear-generics ==0.2.3,
             any.list-t ==1.0.5.7,
             any.logict ==0.8.2.0,
             any.massiv ==1.0.5.0,
             massiv -unsafe-checks,
             any.math-functions ==0.3.4.4,
             math-functions +system-erf +system-expm1,
             any.megaparsec ==9.7.0,
             megaparsec -dev,
             any.mmorph ==1.2.1,
             any.monad-control ==1.0.3.1,
             any.monad-memo ==0.5.4,
             any.mono-traversable ==1.0.21.0,
             any.mtl ==2.3.1,
             any.mutable-containers ==0.3.4.1,
             any.mwc-random ==0.15.2.0,
             mwc-random -benchpapi,
             any.optparse-applicative ==0.19.0.0,
             optparse-applicative +process,
             any.os-string ==2.0.8,
             any.parallel ==3.2.2.0,
             any.parsec ==3.1.18.0,
             any.parser-combinators ==1.3.0,
             parser-combinators -dev,
             any.parsers ==0.12.12,
             parsers +attoparsec +binary +parsec,
             any.pretty ==1.1.3.6,
             any.prettyprinter ==1.7.1,
             prettyprinter -buildreadme +text,
             any.prettyprinter-ansi-terminal ==1.1.3,
             any.primitive ==0.9.1.0,
             any.process ==1.6.26.1,
             process -os-string,
             any.profunctors ==5.6.3,
             any.psqueues ==0.2.8.2,
             any.pvar ==1.0.0.0,
             any.random ==1.3.1,
             any.reducers ==3.12.5,
             any.reflection ==2.1.9,
             reflection -slow +template-haskell,
             any.regex-base ==0.94.0.3,
             any.regex-tdfa ==1.3.2.4,
             regex-tdfa +doctest -force-o2,
             any.rts ==1.0.2,
             any.safe-exceptions ==0.1.7.4,
             any.scheduler ==2.0.1.0,
             any.scientific ==0.3.8.0,
             scientific -integer-simple,
             any.semialign ==1.3.1,
             semialign +semigroupoids,
             any.semigroupoids ==6.0.1,
             semigroupoids +comonad +containers +contravariant +distributive +tagged +unordered-containers,
             any.semigroups ==0.20,
             semigroups +binary +bytestring -bytestring-builder +containers +deepseq +hashable +tagged +template-haskell +text +transformers +unordered-containers,
             any.split ==0.2.5,
             any.splitmix ==0.1.3.1,
             splitmix -optimised-mixer,
             any.stm ==2.5.3.1,
             any.storable-complex ==0.2.3.0,
             any.storable-record ==0.0.7,
             storable-record -buildtests +splitbase,
             any.storable-tuple ==0.1,
             storable-tuple +splitbase,
             any.strict ==0.5.1,
             any.strict-lens ==0.4.1,
             any.system-cxx-std-lib ==1.0,
             any.tagged ==0.8.9,
             tagged +deepseq +transformers,
             any.tasty ==1.5.3,
             tasty +unix,
             any.template-haskell ==2.21.0.0,
             any.text ==2.1.3,
             text -developer -extendedbenchmarks -pure-haskell +simdutf,
             any.tf-random ==0.5,
             any.th-abstraction ==0.7.1.0,
             any.these ==1.2.1,
             any.these-lens ==1.0.2,
             any.time ==1.12.2,
             any.transformers ==0.6.1.0,
             any.transformers-base ==0.4.6,
             transformers-base +orphaninstances,
             any.transformers-compat ==0.7.2,
             transformers-compat -five +five-three -four +generic-deriving +mtl -three -two,
             any.trifecta ==2.1.4,
             any.unboxing-vector ==0.2.0.0,
             unboxing-vector +mono-traversable,
             any.unix ==2.8.6.0,
             unix -os-string,
             any.unliftio-core ==0.2.1.0,
             any.unordered-containers ==0.2.20,
             unordered-containers -debug,
             any.utf8-string ==1.0.2,
             any.utility-ht ==0.0.17.2,
             any.vector ==0.13.2.0,
             vector +boundschecks -internalchecks -unsafechecks -wall,
             any.vector-algorithms ==0.9.1.0,
             vector-algorithms +bench +boundschecks -internalchecks +llvm -unsafechecks,
             any.vector-split ==1.0.0.4,
             any.vector-stream ==0.1.0.1,
             any.vector-th-unbox ==0.2.2,
             any.void ==0.7.3,
             void -safe,
             any.wide-word ==0.1.7.1,
             any.witherable ==0.5,
             any.xhtml ==3000.2.2.1
index-state: hackage.haskell.org 2025-09-07T04:34:43Z
```
:::


### 特筆すべき設定

-   環境変数 `ATCODER` が定義されています
    -   例: `CPP` 言語拡張により `#ifdef ATCODER` が書けます
-   言語エディション [GHC2021](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021) が有効化されています


## 使用可能なパッケージの一覧

[Direct dependencies of `submission:exe:main`](https://gist.github.com/toyboot4e/04b902271997b3c865cc91014720ec40#direct-dependencies-of-submissionexemain) をご参照ください。 Haddock へのリンクもあります。


# 言語アップデート 2025 による変化

AtCoder の Haskeller への影響を確認します。


## コンパイラの更新

現状安定した最新のコンパイラに更新されました:

-   GHC: 9.4.5 (LLVM 14 backend) → 9.8.4 (LLVM 15 backend)
-   Cabal: 3.4 → 3.14.2.0


## ライブラリの更新


### 新規追加のライブラリ

ユーザの要望により、以下のパッケージが追加されました:

- [ac-library-hs-1.5.3.0](https://hackage.haskell.org/package/ac-library-hs-1.5.3.0)
  [AtCoder Library](https://atcoder.github.io/ac-library/production/document_ja/index.html) の Haskell 移植版です。 "Extra" モジュールとして、元々の AtCoder Library には含まれないアルゴリズム・データ構造も多数収録しています。
- [flow-2.0.0.9](https://hackage.haskell.org/package/flow-2.0.0.9)
  パイプライン演算子等を定義した小さなライブラリです。よく見る Haskell に違和感がある人は興味を惹かれると思います。
- [hmatrix-0.20.2](https://hackage.haskell.org/package/hmatrix-0.20.2), [hmatrix-glpk-0.19.0.0](https://hackage.haskell.org/package/hmatrix-glpk-0.19.0.0), [hmatrix-gsl-0.19.0.1](https://hackage.haskell.org/package/hmatrix-gsl-0.19.0.1), [hmatrix-special-0.19.0.0](https://hackage.haskell.org/package/hmatrix-special-0.19.0.0)
  BLAS / LAPACK のラッパーです。ヒューリスティック・コンテスト等で役に立つ時があるかもしれません。
- [ilist-0.4.0.1](https://hackage.haskell.org/package/ilist-0.4.0.1)
  `imap` など、添字関連のリスト処理の関数を定義するライブラリです。 [`vector`](https://hackage.haskell.org/package/vector) と比べてリストの API は小さめですが、このパッケージの導入により大幅にリッチになると思います。
- [monad-memo-0.5.4](https://hackage.haskell.org/package/monad-memo-0.5.4)
  メモ化モナドを定義するパッケージです。キャッシュの実装はマップ (`containers` パッケージ) または配列 (`vector` パッケージ) から選べる模様です。
- [vector-split-1.0.0.4](https://hackage.haskell.org/package/vector-split-1.0.0.4)
  `chunksOf` など、 `unfoldr` の具体例にあたる関数が定義されているパッケージです。たまに必要な処理が一発で書けるかもしれません。
- [wide-word-0.1.8.0](https://hackage.haskell.org/package/wide-word-0.1.8.0)
  128 bit 整数や 256 bit 整数を定義するパッケージです。 SIMD 命令を使っているため高速に動作します。 Barrett reduction, Montgomery 乗算などの実装に利用できます。
  - 最近 `Word128` の `(-)` が修正されました ([wide-word#107](https://github.com/erikd/wide-word/pull/107)).
- [witherable-0.5](https://hackage.haskell.org/package/witherable-0.5)
  これは僕の手に余るので、解説記事だけリンクします:
  - [Too lazy to evaluate - FilterableとWitherableについて](https://viercc.github.io/blog/posts/2021-10-24-witherable-1.html)
  - [Composable filters using Witherable optics](https://chrispenner.ca/posts/witherable-optics)


### 主なパッケージの主な更新内容

主観になりますが、影響の大きなパッケージの主な更新内容をリストしました。

-   array ([0.5.4.0](https://hackage.haskell.org/package/array-0.5.4.0) → [0.5.8.0](https://hackage.haskell.org/package/array-0.5.8.0)) [[changelog](https://hackage.haskell.org/package/array-0.5.8.0/changelog)]
    -   `IArray`: [`!?`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-IArray.html#v:-33--63-), [`foldArray'`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-IArray.html#v:foldrArray) など追加
    -   `MArray`: [`newGenArray`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-MArray.html#v:newGenArray), [`modifyArray`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-MArray.html#v:modifyArray), [`foldMArray'`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-MArray.html#v:foldlMArray-39-) など追加
-   base ([4.17.1.0](https://hackage.haskell.org/package/base-4.17.1.0) → [4.19.2.0](https://hackage.haskell.org/package/base-4.19.2.0)) [[changelog](https://hackage.haskell.org/package/base-4.19.2.0/changelog)]
    -   [`Data.Bifoldable1`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-Bifoldable1.html) 追加
    -   [`Data.Foldable1`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-Foldable1.html) 追加
    -   `Data.Function`: [`applyWhen`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-Function.html#v:applyWhen) 追加
    -   `Data.List`: [(!?)](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#v:-33--63-) 追加
    -   `Data.List.NonEmpty`: [`inits1`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-List-NonEmpty.html#v:inits1), [`tails1`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-List-NonEmpty.html#v:tails1), 追加
    -   `Data.Traversable`: [`mapAccumM`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-Traversable.html#v:mapAccumM), [`forAccumM`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Data-Traversable.html#v:forAccumM) 追加
    -   `Debug.Trace`: [`traceWith`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Debug-Trace.html#v:traceWith), [`traceShowWith`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Debug-Trace.html#v:traceShowWith), [`traceEventWith`](https://hackage.haskell.org/package/base-4.19.2.0/docs/Debug-Trace.html#v:traceEventWith) 追加
-   bytestring ([0.11.4.0](https://hackage.haskell.org/package/bytestring-0.11.4.0) → [0.12.2.0](https://hackage.haskell.org/package/bytestring-0.12.2.0)) [[changelog](https://hackage.haskell.org/package/bytestring-0.12.2.0/changelog)]
    -   `Data.ByteString.Char8`: [`readInt64`](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Char8.html#v:readInt64), .., [`readInt8`](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Char8.html#v:readInt8), [`readWord64`](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Char8.html#v:readWord64), .., [`readWord8`](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Char8.html#v:readWord8) 追加
    -   バグ修正、最適化など多数
-   containers ([0.6.8](https://hackage.haskell.org/package/containers-0.6.8) → [0.8](https://hackage.haskell.org/package/containers-0.8)) [[changelog](https://hackage.haskell.org/package/containers-0.8/changelog)]
    -   `Data.IntSet`: [`lookupMin`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntSet.html#v:lookupMin), [`lookupMax`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntSet.html#v:lookupMax), [`intersections`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntSet.html#v:intersections), [`symmetricDifference`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntSet.html#v:symmetricDifference), [`foldMap`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntSet.html#v:foldMap) 追加
    -   `Data.Map(.Lazy)`: [`filterKeys`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Lazy.html#v:filterKeys), [`symmetricDifference`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Lazy.html#v:symmetricDifference) 追加
    -   `Data.IntMap(.Lazy)`: [`filterKeys`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntMap-Lazy.html#v:filterKeys), [`symmetricDifference`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntMap-Lazy.html#v:symmetricDifference) 追加
    -   `Data.Tree`: [`leaves`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html#v:leaves), [`edges`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html#v:edges), [`pathsToRoot`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html#v:pathsToRoot), [`pathsFromRoot`](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html#v:pathsFromRoot) 追加
    -   バグ修正、最適化、整合性のための変更など多数
-   extra ([1.7.13](https://hackage.haskell.org/package/extra-1.7.13) → [1.8](https://hackage.haskell.org/package/extra-1.8)) [[changelog](https://hackage.haskell.org/package/extra-1.8/changelog)]
    -   `Data.List.NonEmpty.Extra`: [`compareLength`](https://hackage-content.haskell.org/package/extra-1.8/docs/Data-List-NonEmpty-Extra.html#v:compareLength) 追加
    -   `Data.Foldable.Extra`: [`compareLength`](https://hackage-content.haskell.org/package/extra-1.8/docs/Data-Foldable-Extra.html#v:compareLength) 追加
-   primitive ([0.8.0.0](https://hackage.haskell.org/package/primitive-0.8.0.0) → [0.9.1.0](https://hackage.haskell.org/package/primitive-0.9.1.0))
    -   `Data.MutVar`: [`mutVarFromIORef`](https://hackage-content.haskell.org/package/primitive-0.9.1.0/docs/Data-Primitive-MutVar.html#v:mutVarFromIORef) など追加
    -   その他、低レベル操作の関数を追加
-   random ([1.2.1.1](https://hackage.haskell.org/package/random-1.2.1.1) → [1.3.1](https://hackage.haskell.org/package/random-1.3.1)) [[changelog](https://hackage.haskell.org/package/random-1.3.1/changelog)]
    -   `System.Random.Stateful`: [`uniformShuffleList`](https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random-Stateful.html#v:uniformShuffleList), [`uniformShuffleListM`](https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random-Stateful.html#v:uniformShuffleListM) 追加
    -   その他、 seed や `UniformRange` 関連の関数を多数追加
-   vector ([0.13.0.0](https://hackage.haskell.org/package/vector-0.13.0.0) → [0.13.2.0](https://hackage.haskell.org/package/vector-0.13.2.0)) [[changelog](https://hackage.haskell.org/package/vector-0.13.2.0/changelog)]
    -   `Data.Vector.Unboxed`: [`DoNotUnboxLazy`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxLazy), [`DoNotUnboxStrict`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxStrict), [`DoNotUnboxNormalForm`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxNormalForm) 追加
    -   `Data.Vector.Generic.Mutable`:
        -   [`nextPermutation`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic-Mutable.html#v:nextPermutation), [`prevPermutation`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic-Mutable.html#v:prevPermutation) の特大高速化
        -   [`nextPermutationBy`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic-Mutable.html#v:nextPermutationBy), [`prevPermutationBy`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic-Mutable.html#v:prevPermutationBy) 追加
    -   `Data.Vector.Generic`: [`spanR`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic.html#v:spanR), [`breakR`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Generic.html#v:breakR) 追加
-   vector-algorithms ([0.9.0.1](https://hackage.haskell.org/package/vector-algorithms-0.9.0.1) → [0.9.1.0](https://hackage.haskell.org/package/vector-algorithms-0.9.1.0)) [[changelog](https://hackage.haskell.org/package/vector-algorithms-0.9.1.0/changelog)]
    -   `sort`, `sortUniq`, `nub`, `nubBy`, `nubByMut` の高速化 (`INLINE` 追加)


# まとめ

以上の通り、順当に最新環境に更新されると思います。僕としては、特に言語サーバ (`haskell-language-server`) を更新できること、 [ac-library-hs](https://hackage.haskell.org/package/ac-library-hs) を追加できたことが嬉しいです。

今回パッケージの追加の機会を逃してしまった人は、ぜひ次回の言語アップデートにご参加ください。おそらく次回の更新も、 [AtCoderの言語アップデートに関して (2024-25年度)](https://atcoder.jp/posts/1342) に記載の Discord で実施されるのではかと思います。

> 前回の言語アップデートで [gksato](https://atcoder.jp/users/gksato) さんご作成のテスト環境があり、今年はその延長で更新されました。いつもありがとうございます。また AtCoder はこれだけ存分に Haskell を振るえる唯一のコンテストサイトだと改めて認識し、深く感謝しております。
