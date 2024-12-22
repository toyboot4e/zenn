---
title: "インクリメンタルな開発"
---


# テストが欲しい

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) の開発は、言ってしまえばただの写経です。本家 [`ac-library`](https://github.com/atcoder/ac-library) のコードを右から左へ書き写すだけ。楽勝でした！

実際は、色々バグが出て大変でした。特に酷かったのは最小費用流で、大きな関数を一から何度も読み直して確認しました。デバッグに一週間かかり、 **先にテストを書くべきだった** と痛感しました。しかも馬鹿らしくなるほど単純なもので十分でした。


# [`tasty`](https://github.com/UnkindPartition/tasty) のセットアップ

Haskell のテストフレームワークは [`tasty`](https://github.com/UnkindPartition/tasty) が主流と思います。 2013 年当時は [`test-framework` が普及していた](https://blog.ocharles.org.uk/posts/2013-12-03-24-days-of-hackage-tasty.html) ようですが、メンテナンス困難になり [`tasty`](https://github.com/UnkindPartition/tasty) が生まれたとか。

[`tasty`](https://github.com/UnkindPartition/tasty) は Haskell 界では珍しく README が充実しています。ツールはドキュメント性で選べば間違い無いと思いますから、 [`tasty`](https://github.com/UnkindPartition/tasty) には安心感があります。開発者ブログも面白いです。


## `ac-library-hs.cabal`

`test-suite` を追加しました。どうも **`cabal` 自身に `test-suite` などのエントリーを追加するコマンドが無さそう** で、手動でコピペしました。こういうのは手書きしたくないものですが:

```yaml
test-suite ac-library-hs-test
  import:         warnings
  import:         dependencies
  other-modules:
    Tests.Internal.MinHeap

  type:           exitcode-stdio-1.0
  hs-source-dirs: test
  main-is:        Main.hs
  build-depends:
    , ac-library-hs
    , hspec
    , mtl
    , QuickCheck
    , quickcheck-classes
    , random
    , tasty
    , tasty-hspec
    , tasty-hunit
    , tasty-quickcheck
    , tasty-rerun
    , transformers
```


## `tests/Main.hs`

`Main.hs` は [`tasty-rerun`](https://github.com/ocharles/tasty-rerun) を使う形にしました:

```haskell
module Main (main) where

import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun
import Tests.Internal.MinHeap qualified

main :: IO ()
main =
  defaultMainWithRerun
    . testGroup "toplevel"
    $ [ testGroup "Tests.Internal.MinHeap" Tests.Internal.MinHeap.tests,
      ]
```


## `tests/Internal/MinHeap.hs`

ここで作成したテストは、自作の `MinHeap` が本当に昇順で値を取り出すか確認します:

```haskell
module Tests.Internal.MinHeap (tests) where

import AtCoder.Internal.MinHeap qualified as ACIMH
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

prop_ordered :: [Int] -> Bool
prop_ordered xs =
  let n = length xs
      expected = L.sort xs
      result = runST $ do
        -- ヒープを作成する
        heap <- ACIMH.new n
        for_ xs (ACIMH.push heap)
        -- ヒープから順番に値を取り出す
        replicateM n (fromJust <$> ACIMH.pop heap)
   in expected == result

tests :: [TestTree]
tests =
  [ QC.testProperty "min heap ordering" prop_ordered
  ]
```

この単純なテストに失敗し、実は最小費用流ではなく `MinHeap` にバグがあると分かりました。これで数日潰れたのは悲しいです。以降のモジュールは、先に簡単なテストを書いてから作成しました。


## テスト実行

[`tasty-rerun`](https://github.com/ocharles/tasty-rerun) による実行のため、オプションを指定しています。前回のテストで失敗したテストがあった場合は、失敗したテストのみ再実行できます:

```sh
$ cabal test --test-option --rerun
```

毎回オプションを指定するのは手間で、シェルスクリプトにして置いています。

> [`just`](https://github.com/casey/just) か [`cargo-make`](https://github.com/sagiegurari/cargo-make) のようなタスクランナーを導入した方が良いかもしれません。コミュニティでどんどん質問して行きたいです。


# 各種テストの作成

[`tasty`](https://github.com/UnkindPartition/tasty) により各種テストライブラリを `TestTree` にまとめて一括実行できます。 [`tasty-discorver`](https://github.com/haskell-works/tasty-discover) に [記載がある](https://github.com/haskell-works/tasty-discover?tab=readme-ov-file#write-tests) 通り、テスト関数名に接頭辞を付けると (Template Haskell により) すべてのテストを実行する仕組みがあります。

> - prop_: [QuickCheck](http://hackage.haskell.org/package/tasty-quickcheck) properties.
> - scprop_: [SmallCheck](http://hackage.haskell.org/package/tasty-smallcheck) properties.
> - hprop_: [Hedgehog](http://hackage.haskell.org/package/tasty-hedgehog) properties.
> - unit_: [HUnit](http://hackage.haskell.org/package/tasty-hunit) test cases.
> - spec_: [Hspec](http://hackage.haskell.org/package/tasty-hspec) specifications.
> - test_: [Tasty](http://hackage.haskell.org/package/tasty) TestTrees.
> - tasty_: Custom tests

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) ではテストの一覧を手書きしていますが、テスト名には慣習通り接頭辞を付けました。


## [`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit)

[`hunit`](https://hackage.haskell.org/package/HUnit) は僅か 739 行の単体テストのライブラリです。主に `assert` 相当の関数と演算子を提供します。 [`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit) が本家 [`hunit`](https://hackage.haskell.org/package/HUnit) の内容を re-export しているため、 [`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit) のみを見れば良いと思います。

[`ac-library`](https://github.com/atcoder/ac-library) (C++) の単体テストは assertion であるため、主に [`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit) で移植しました。 [`@=?`](https://hackage.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html#v:-64--63--61-) による assert の使用頻度が高かったです。

```haskell
module Tests.Dsu (tests) where

import AtCoder.Dsu qualified as Dsu
import Test.Tasty
import Test.Tasty.HUnit

unit_simple :: TestTree
unit_simple = testCase "simple" $ do
  uf <- Dsu.new 2
  (@?= False) =<< Dsu.same uf 0 1
  x <- Dsu.merge uf 0 1
  (@?= True) =<< Dsu.same uf 0 1

tests :: [TestTree]
tests =
  [ unit_simple
  ]
```

[`hunit`](https://hackage.haskell.org/package/HUnit) はテスト用ライブラリとしては小さ過ぎます。たとえば [`assertEqual`](https://hackage.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html#v:assertEqual) に対して `assertNotEqual` がありません。また例外発生を予期したテストを書く仕組みもありません。そのため [`hunit` の上に作られた](https://hspec.github.io/hunit.html) という [`hspec`](https://hackage.haskell.org/package/hspec) も使います。


## [`hspec`](https://hackage.haskell.org/package/hspec), [`tasty-hspec`](https://hackage.haskell.org/package/tasty-hspec)

`hspec-*` パッケージを色々集めると 15,000 行くらいあります。独特の記法に面食らいましたが、 [`hspec`](https://hackage.haskell.org/package/hspec) は [BBD (behavior driven development) のライブラリ](https://www.mew.org/~kazu/material/2012-bdd.pdf) らしく、きっとメジャーな API です。

ここでは例外発生のテストを書いてみます:

```haskell
module Tests.Dsu (tests) where

import AtCoder.Dsu qualified as Dsu
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  -- it throws error と読める (behaviour を説明している)
  it "throws error" $ do
    Dsu.new (-1) `shouldThrow` anyException

tests :: [TestTree]
tests =
  [ unsafePerformIO spec_invalid
  ]
```

[Examples](https://hackage.haskell.org/package/tasty-hspec-1.2.0.4/docs/Test-Tasty-Hspec.html#g:3) にある通り、ここでの `unsafePerformIO` には他のテストへの副作用が無いため問題ありません。もう少しシンプルに `tasty` の `TestTree` に組み込めないかという気はしますが……。また使い方の割に依存が大きかったため、 `hunit` に合わせて `assertException` のような関数を作るだけでも十分だった気はします。


## [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck), [`tasty-quickcheck`](https://hackage.haskell.org/package/tasty-quickcheck-0.11)

我らが [† 伝家の宝刀 †](https://x.com/xuzijian629/status/1222056854978617345) 、 7,114 行の PBT (プロパティベーステスト) のライブラリです。大きめのライブラリですが、 API はそこまで大きくなく、逆に機能不足を感じるときもあります。

僕は [実践プロパティベーステスト](https://www.lambdanote.com/products/proper) を 1/3 まで読んで積んでいますが、身構えず、ラフなテストを書くために使ってみました。実例は `MinHeap` の例の通りですが、その他便利な使い方をメモしておきます。


### `Bool` を返す関数は [`Testable`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#t:Testable)

自明なテストは `Bool` を返す関数で十分だと思います。ここでは失敗ケースを見たいので、滅茶苦茶なプロパティを書いてみます。

```haskell
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

prop_example :: [Int] -> Bool
prop_example xs = xs == reverse xs

tests :: [TestTree]
tests =
  [ QC.testProperty "example" prop_example,
  ]
```

当然失敗します。ここで入力値が表示しますが、 ****左辺と右辺の値は表示されません**** 。改善します。

```sh
example: FAIL
  *** Failed! Falsified (after 3 tests and 1 shrink):
  [1,0]
  Use --quickcheck-replay="(SMGen 15558177999669110064 7725842824357830847,2)" to reproduce.

1 out of 1 tests failed (0.00s)
```

> なお verbose check にすれば通過ケースや shrink の過程も確認できます。 QuickCheck のソースを読む時は shrink 周辺が見どころですね。
> 
> ```sh
> $ cabal test --test-options '--quickcheck-verbose'
> example: FAIL
>   Passed:
>   []
> 
>   Failed:
>   [2,2,1]
> 
>   Passed:
>   []
> 
>   Failed:
>   [2,1]
> ..
> ```


### エラー表示を詳細に

テスト失敗時のカスタムメッセージを指定する関数が [`counterexample`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:counterexample) です。判例 (counterexample) の詳細を示すということですね:

```haskell
prop_example :: [Int] -> QC.Property
prop_example xs = QC.counterexample "error case" $ xs == reverse xs
```

エラーメッセージに `error case` が追加されました:

```haskell
example: FAIL
  *** Failed! Falsified (after 4 tests and 2 shrinks):
  [0,1]
  error case
  Use --quickcheck-replay="(SMGen 7441833134974059469 3200274122949544035,3)" to reproduce.

1 out of 1 tests failed (0.00s)
```

[`===`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:-61--61--61-) や [`=/=`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:-61--47--61-) も [`counterexample`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:counterexample) を使って実装されています。失敗時に左辺と右辺の値を表示してくれるようになります:

```haskell
(===) :: (Eq a, Show a) => a -> a -> Property
x === y =
  counterexample (show x ++ interpret res ++ show y) res
  where
    res = x == y
    interpret True  = " == "
    interpret False = " /= "
```

実際に [`===`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:-61--61--61-) を使うと、次のようなエラー表示になります:

```haskell
prop_example :: [Int] -> QC.Property
prop_example xs = xs QC.=== reverse xs
```

```sh
example: FAIL
  *** Failed! Falsified (after 6 tests and 6 shrinks):
  [0,1]
  [0,1] /= [1,0]
  Use --quickcheck-replay="(SMGen 16394814400549318447 147282775057894905,5)" to reproduce.

1 out of 1 tests failed (0.00s)
```

欲を言えば、両辺の計算式をそれぞれテキストで表示して、何の式を比較したか一目で確認したいところです。これは『モノイドのテスト』の `myForAllShrink` が実現しています。


### [`Gen`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#t:Gen) モナド

[`Arbitrary`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#t:Arbitrary) 型の引数は規定の [`Gen`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#t:Gen) により生成されますが、手動で `Gen` を使うこともできます:

```haskell
prop_example :: QC.Property
prop_example = do
  QC.forAll (QC.arbitrary @Int) $ \n ->
    QC.forAll (QC.vectorOf n (QC.arbitrary @Int)) $ \xs ->
      xs QC.=== reverse xs
```

[`forAll`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:forAll) でも悪くないですが、 [`Gen`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#t:Gen) モナドの bind (`<-`) を使えばフラットに記述できます:

```haskell
prop_example :: QC.Gen QC.Property
prop_example = do
  n <- QC.arbitrary @Int
  xs <- QC.vectorOf n (QC.arbitrary @Int)
  pure $ xs QC.=== reverse xs
```

実行するとこうなりました:

```txt
example: FAIL
  *** Failed! Falsified (after 2 tests):
  [-1,-1,-1,1,0,0] /= [0,0,1,-1,-1,-1]
  Use --quickcheck-replay="(SMGen 15437984156035230625 5370423934061024679,1)" to reproduce.

1 out of 1 tests failed (0.00s)
```

Shrinking が起きませんでした。 `Gen` の使い方には注意が必要そうです。


### `Arbitrary` の orphan instance

`Max`, `Sum`, `ByteString`, `Vector` 等々には `Arbitrary` インスタンスが定義されていません。都度自分で (orphan) instance を定義するか、 [quickcheck-instances](https://hackage.haskell.org/package/quickcheck-instances) パッケージを利用すると良さそうです。


### `Monadic` なテスト

`MinHeap` の例では `ST` モナドを使用しましたが、 monadic な計算過程で常に invariant が成り立つか確認したいとき、手軽にテストしたい時などは [`Test.QuickCheck.Monadic`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html) が使えます。 Mutable データ型のテストにも利用できます。

`Gen` の利用には [`forAllM`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html#v:forAllM) を使うか、 [`pick`](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html#v:pick) を使うとフラットにできます。以下では最小費用流の CSR で順方向・逆方向の辺の頂点が互いに逆向きであるか確認しています:

```haskell
module Tests.Internal.McfCsr (tests) where

import AtCoder.Internal.McfCsr qualified as ACIMCSR
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- 最小費用流の辺を生成する
edgeGen :: Int -> Int -> QC.Gen [(Int, Int, Int, Int, Int)]
edgeGen n m = QC.vectorOf m $ do
  from <- QC.chooseInt (0, n - 1)
  to <- QC.chooseInt (0, n - 1)
  cap <- QC.chooseInt (0, 16)
  flow <- QC.chooseInt (0, cap)
  cost <- QC.chooseInt (0, 16)
  pure (from, to, cap, flow, cost)

-- 最小費用流の CSR で順方向・逆方向の辺が互いに逆向きであるか確認する
revEdgeDirection :: Int -> Int -> QC.Property
revEdgeDirection n m = QC.monadicIO $ do
  -- 辺の数と頂点の数を決める
  n <- QC.pick $ QC.chooseInt (1, 16)
  m <- QC.pick $ QC.chooseInt (0, 128)
  -- 辺を生成する
  edges <- QC.pick $ edgeGen n m
  -- グラフを生成する
  (!_, csr@ACIMCSR.Csr {..}) <- QC.run $ ACIMCSR.build n (VU.fromList edges)
  -- 辺の向きをチェックする
  for_ [0 .. n - 1] $ \from -> do
    VU.forM_ (ACIMCSR.adj csr from) $ \(!_to, !rev, !cost) -> do
      QC.assert $ toCsr VU.! rev == from
      QC.assert $ -costCsr VU.! rev == cost
```

こうした大雑把なチェックを書くことで開発が速まります。


## 特定のテストを実行する

`TestTree` の中の特定のテストを実行するため、 `tasty` の [patterns](https://github.com/UnkindPartition/tasty?tab=readme-ov-file#patterns) を利用します。開発者ブログ ([New patterns in tasty](https://ro-che.info/articles/2018-01-08-tasty-new-patterns)) にあるとおり、 `tasty` の [patterns](https://github.com/UnkindPartition/tasty?tab=readme-ov-file#patterns) は AWK のアイデアが元になっています。

AWK と聞いただけで、すべて理解されたかもしれません。まずテストの一覧を `tasty` の [`-l`](https://github.com/UnkindPartition/tasty?tab=readme-ov-file#runtime) オプションで確認します:

```text
$ cabal test --test-options '-l'
toplevel.Convolution.empty
toplevel.Convolution.butterfly
```

プログラム上は次の関係にあり、 *group* 名が `.` で区切られていることが分かります:

```haskell
main :: IO ()
main =
  defaultMainWithRerun
    . testGroup "toplevel"
    $ [ testGroup "Convolution"
        [ unit_empty,
          unit_butterfly
        ]
      ]

unit_empty :: TestTree
unit_empty = testCase "empty" $ do
  VU.empty @=? ACC.convolutionRaw (Proxy @998244353) (VU.empty @Int) (VU.empty @Int)

unit_butterfly :: TestTree
unit_butterfly = testCase "butterfly" $ do {.. }
```

`empty` テストのみ実施する方法としては、以下のパターンなどがあります:

```sh
$ cabal test --test-option '-p $NF=="empty'"
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: OK
  Scc
    empty: OK
  String
    empty: OK
  TwoSat
    empty: OK

All 4 tests passed (0.00s)
```

```sh
$ cabal test --test-option '-p /empty/'
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: OK
  Scc
    empty: OK
  String
    empty: OK
  TwoSat
    empty: OK

All 4 tests passed (0.00s)
```

`Convolution.empty` テストのみ実施する方法としては、以下のパターンなどがあります:

```sh
$ cabal test --test-option '-p $0=="toplevel.Convolution.empty"'
Running 1 test suites...
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: OK

All 1 tests passed (0.00s)
```

```sh
$ cabal test --test-option '-p $2=="Convolution" && $3=="empty"'
Running 1 test suites...
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: OK

All 1 tests passed (0.00s)
```

> ところで `--test-options` にすると、エラーが出ます:
> 
> ```sh
> $ cabal test --test-options '-p $NF=="empty'"
> ...
> option -p: Could not parse: $NF==toplevel.Convolution.empty is not a valid pattern
> ```
> 
> どうも `cabal` による単語分解で `"` が消えるようです。 `cabal` の引数はめんどくさいぞ〜

一応、 [`tasty-rerun`](https://github.com/ocharles/tasty-rerun) との併せ技もできます:

```sh
$ cabal test --test-options '--rerun -p /empty/'
Running 1 test suites...
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: FAIL
      test/Tests/Convolution.hs:62:
      expected: False
       but got: True
      Use -p '/empty/&&/Convolution.empty/' to rerun this test only.
  Scc
    empty: OK
  String
    empty: OK
  TwoSat
    empty: OK

1 out of 4 tests failed (0.00s)
Test suite ac-library-hs-test: FAIL

$ cabal test --test-options '--rerun -p empty'                                                                                                                                          ✘ 1 wip ⬆ ✚ ✱ ◼
Running 1 test suites...
Test suite ac-library-hs-test: RUNNING...
toplevel
  Convolution
    empty: FAIL
      test/Tests/Convolution.hs:62:
      expected: False
       but got: True

1 out of 1 tests failed (0.00s)
Test suite ac-library-hs-test: FAIL
```


# まとめ

[`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs) の開発経験から、テストを書く必要性を実感しました。テストのカバレッジが 100% である必要は無く、 QuickCheck で雑なテストを書くだけで十分に開発を高速化できます。境界値チェックや網羅的なテストは `hunit` や `hspec` で assert を書けば良いと思います。
