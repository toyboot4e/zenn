---
title: "Borrow, rules?"
emoji: "👻"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["rust", "miri"]
published: true
---

_じゃなーい！_

[Aliasing][aliasing] の話です。 **絶対に間違ったことも言っています** が、素人記事だと思って許してください。

[aliasing]: https://rust-lang.github.io/unsafe-code-guidelines/glossary.html#aliasing

# UB

スライスから `&mut T` を 2 つ取ります。 Safe なやり方は、標準ライブラリの [split_at_mut] などですが、まどろっこしいコードになりますね。

[split_at_mut]: https://moshg.github.io/rust-std-ja/std/primitive.slice.html#method.split_at_mut

`unsafe` 、やっちゃいましょうか！

## Pointer dereferencing (1)

`&mut T` → `*mut T` → `&mut T` と変換すれば、 borrow checker を欺くことができます; コンテナの可変参照を取らずに、コンテナが所有するヒープデータの可変参照が取れます:

```rust
fn main() {
    let xs = &mut [0, 1, 2];

    let x0 = &xs[0];
    let x0 = unsafe { &mut *(x0 as *const _ as *mut _) };

    let x1 = &xs[1];
    let x1 = unsafe { &mut *(x1 as *const _ as *mut _) };

    *x0 = 10;
    *x1 = 20;

    println!("{:?}", xs);
}
```

実行すると:

```sh
$ cargo run
   Compiling ub-test v0.1.0 (/Users/tbm/dev/rs/ub-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.33s
     Running `target/debug/ub-test`
[10, 20, 2]
```

確かに動作しています。 Rust でもその気になれば、 C に近いポインタの使い方ができるというわけですね！　安心しました〜

## Pointer dereferencing (2)

こんな世界じゃ、 `&mut T` も `&T` も変わりませんね。スライスを immutable にします:

```diff-rust
fn main() {
-    let xs = &mut [0, 1, 2];
+    let xs = &[0, 1, 2];

    let x0 = &xs[0];
    let x0 = unsafe { &mut *(x0 as *const _ as *mut _) };

    let x1 = &xs[1];
    let x1 = unsafe { &mut *(x1 as *const _ as *mut _) };

    *x0 = 10;
    *x1 = 20;

    println!("{:?}", xs);
}
```

ヘヘッ……　Rust があれば、何だってできるさ…… !!

```sh
$ cargo run
   Compiling ub-test v0.1.0 (/Users/tbm/dev/rs/ub-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.43s
     Running `target/debug/ub-test`
fish: Job 1, 'cargo $argv' terminated by signal SIGBUS (Misaligned address error)
```

**_なにぃィイイイイイイイイイイイィッッ_**

これはつまり…… UB [^1] です ！　Safe Rust と同様に、 unsafe Rust には特殊なルールが存在します。ルールを違反すると、 UB を引き起こす可能性があります。

# `UnsafeCell`

`Cell` と `RefCell` の説明ばかりで、 `UnsafeCell` をスルーしていませんか。

それは私です。

`UnsafeCell` を見にいくと、 unsafe Rust における制約 (aliasing rules) の解説があります。

## `&T` → `&mut T` は UB

[UnsafeCell] のドキュメントによると:

[UnsafeCell]: https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html

> Mutating that data, for example through an alias or by transmuting an &T into an &mut T, is considered undefined behavior.

ま、まるで `&T` から `&mut T` を取るのは UB みたいな書き方ですね！

(※ Rust の仕様上は UB 、実際にクラッシュするかは不明　だと思います)

## `UnsafeCell` の interior mutability

冒頭では `&T` → `&mut T` の変換で UB が起きました。しかし `T` を `UnsafeCell` の中に入れれば、 `&UnsafeCell<T>` → `*mut T` → `&mut T` が可能だと言います:

> UnsafeCell<T> opts-out of the immutability guarantee for &T: a shared reference &UnsafeCell<T> may point to data that is being mutated. This is called “interior mutability”.

## Aliasing rules

ただし、実行時に同じ値の `&mut T` を 2 つ作ることは、 `UnsafeCell<T>` を使っても違反だと言います:

> There is no legal way to obtain aliasing &mut, not even with UnsafeCell<T>.

この実行時の borrow check みたいなルールを _[aliasing] rules_ と言います。 Interior mutability と aliasing rules を合わせて、 `&UnsafeCell<T>` → `*mut T` → `&mut T` は実行時に alias (参照の重なり) が無い限り sound [^2] であると言えます。

`UnsafeCell` を含め、 [miri] で検証して行きます。

[miri]: https://github.com/rust-lang/miri

# `miri`

[miri] は、 Rust の [aliasing] モデル……を定義しようとするモデル、 [Stacked Borrows][SB] を実装した Rust MIR の interpreter だそうです。 [miri] でコードを実行すれば、様々な UB を検出できます。

[SB]: https://github.com/rust-lang/unsafe-code-guidelines/blob/master/wip/stacked-borrows.md

## 検証 1: `&T` → `&mut T` は常にエラー

```rust
fn main() {
    let xs = &mut [0, 1, 2];
    println!("{:?}", xs);

    let x0 = &xs[0];
    let x0 = unsafe { &mut *(x0 as *const _ as *mut _) };

    *x0 = 10;

    println!("{:?}", xs);
}
```

```sh
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
error: Undefined Behavior: trying to reborrow for Unique at alloc1486, but parent tag <untagged> does not have an appropriate item in the borrow stack
 --> src/main.rs:6:23
  |
6 |     let x0 = unsafe { &mut *(x0 as *const _ as *mut _) };
  |                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ trying to reborrow for Unique at alloc1486, but parent tag <untagged> does not have an appropriate item in the borrow stack
  |
  = help: this indicates a potential bug in the program: it performed an invalid operation, but the rules it violated are still experimental
  = help: see https://github.com/rust-lang/unsafe-code-guidelines/blob/master/wip/stacked-borrows.md for further information
          
  = note: inside `main` at src/main.rs:6:23
  = note: inside `<fn() as std::ops::FnOnce<()>>::call_once - shim(fn())` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:227:5
  = note: inside `std::sys_common::backtrace::__rust_begin_short_backtrace::<fn(), ()>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/sys_common/backtrace.rs:125:18
  = note: inside closure at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/rt.rs:63:18
  = note: inside `std::ops::function::impls::<impl std::ops::FnOnce<()> for &dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe>::call_once` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:259:13
  = note: inside `std::panicking::r#try::do_call::<&dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe, i32>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panicking.rs:401:40
  = note: inside `std::panicking::r#try::<i32, &dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panicking.rs:365:19
  = note: inside `std::panic::catch_unwind::<&dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe, i32>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panic.rs:129:14
  = note: inside closure at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/rt.rs:45:48
  = note: inside `std::panicking::r#try::do_call::<[closure@std::rt::lang_start_internal::{closure#2}], isize>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panicking.rs:401:40
  = note: inside `std::panicking::r#try::<isize, [closure@std::rt::lang_start_internal::{closure#2}]>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panicking.rs:365:19
  = note: inside `std::panic::catch_unwind::<[closure@std::rt::lang_start_internal::{closure#2}], isize>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/panic.rs:129:14
  = note: inside `std::rt::lang_start_internal` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/rt.rs:45:20
  = note: inside `std::rt::lang_start::<()>` at /Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/library/std/src/rt.rs:62:5

error: aborting due to previous error

[0, 1, 2]
```

途中で実行が止まりました。 [miri] としては `&T` → `&mut T` はエラーのようです。分かっていたことですが、コードを実行すると確信できました。

> たぶん `&T` → `&mut T` は経路によらずエラーです。 Stacked borrow model を調べないと分かりませんが……

## 検証 2: `UnsafeCell` の interior mutability

`&UnsafeCell<T>` → `*mut T` → `&mut T` は、 `UnsafeCell` 内部のデータに関して実行時に borrow rules を守れば sound  です。

```sh
$ rustup +nightly component add miri
```

```rust
fn main() {
    use std::cell::UnsafeCell;

    let xs = &[UnsafeCell::new(0), UnsafeCell::new(1), UnsafeCell::new(2)];
    println!("{:?}", xs);

    let x0 = xs[0].get();
    let x1 = xs[1].get();
    let x0 = unsafe { &mut *(x0 as *mut _) };
    let x1 = unsafe { &mut *(x1 as *mut _) };

    *x0 = 10;
    *x1 = 20;

    println!("{:?}",
        xs.iter().map(|x| unsafe { *x.get() }).collect::<Vec<_>>()
    );
}
```

```sh
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
[UnsafeCell { .. }, UnsafeCell { .. }, UnsafeCell { .. }]
[10, 20, 2]
```

**`xs: &[U]` であるにも関わらず、内部のデータを変更しましたが、冒頭の UB は出ませんでした** 。 `miri` もエラーを出しません。これが、 `UnsafeCell` の interior mutability!

## 検証 3 (追記): やっぱり危険で、 `slice::split_at_mut` が有用でした

検証 3-1, 3-2 では、コンテナの要素それぞれの可変参照を同時に取れると言いたかったです。しかし、少し例をいじると UB になることが分かりました ([code](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d56d5b42be1b0e8c9ea9a43ab9a65d11)):

```diff-rust
fn main() {
    let xs = &mut [0, 1, 2];
    let x0 = unsafe { &mut *(&mut xs[0] as *mut _) };
    let x1 = unsafe { &mut *(&mut xs[1] as *mut _) };
    *x0 = 10;
+    println!("{}", xs.len());
    *x1 = 11;
    assert_eq!(xs, &mut [10, 11, 2]);
}
```

```sh
   Compiling playground v0.0.1 (/playground)
    Finished dev [unoptimized + debuginfo] target(s) in 1.25s
     Running `/playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/cargo-miri target/miri/x86_64-unknown-linux-gnu/debug/playground`
error: Undefined Behavior: no item granting write access to tag <2854> at alloc1420+0x4 found in borrow stack.
 --> src/main.rs:7:5
  |
7 |     *x1 = 11;
  |     ^^^^^^^^ no item granting write access to tag <2854> at alloc1420+0x4 found in borrow stack.
  |
  = help: this indicates a potential bug in the program: it performed an invalid operation, but the rules it violated are still experimental
  = help: see https://github.com/rust-lang/unsafe-code-guidelines/blob/master/wip/stacked-borrows.md for further information
          
  = note: inside `main` at src/main.rs:7:5
  = note: inside `<fn() as std::ops::FnOnce<()>>::call_once - shim(fn())` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:227:5
  = note: inside `std::sys_common::backtrace::__rust_begin_short_backtrace::<fn(), ()>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/sys_common/backtrace.rs:125:18
  = note: inside closure at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/rt.rs:63:18
  = note: inside `std::ops::function::impls::<impl std::ops::FnOnce<()> for &dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe>::call_once` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:259:13
  = note: inside `std::panicking::r#try::do_call::<&dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe, i32>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panicking.rs:403:40
  = note: inside `std::panicking::r#try::<i32, &dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panicking.rs:367:19
  = note: inside `std::panic::catch_unwind::<&dyn std::ops::Fn() -> i32 + std::marker::Sync + std::panic::RefUnwindSafe, i32>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panic.rs:129:14
  = note: inside closure at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/rt.rs:45:48
  = note: inside `std::panicking::r#try::do_call::<[closure@std::rt::lang_start_internal::{closure#2}], isize>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panicking.rs:403:40
  = note: inside `std::panicking::r#try::<isize, [closure@std::rt::lang_start_internal::{closure#2}]>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panicking.rs:367:19
  = note: inside `std::panic::catch_unwind::<[closure@std::rt::lang_start_internal::{closure#2}], isize>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/panic.rs:129:14
  = note: inside `std::rt::lang_start_internal` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/rt.rs:45:20
  = note: inside `std::rt::lang_start::<()>` at /playground/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/rt.rs:62:5

error: aborting due to previous error
```

`xs.len()` がスライス全体の参照を取ります。一方で、 **スライスの indexer (`&mut xs[m]`) はスライス全体の参照を取らなかった** ようですが、これは **例外** 的な操作に見えます。

非常に気を遣えば、スライスから複数の可変参照を取ることができそうです (3-1) が、やはり危険だったのでお勧めできません。

## 危険 ~~検証 3-1: `&mut T` → `*mut T` → `&mut T` は重なりが無いなら sound~~

**上の検証 3 (追記) もご参照ください**

これが sound だと明確に書いてあるのを見た記憶は無いのですが、現在の `miri` としては sound でした。

```rust
fn main() {
    let xs = &mut [0, 1, 2];
    println!("{:?}", xs);

    let x0 = &mut xs[0] as *mut _;
    let x1 = &mut xs[1];
    let x0 = unsafe { &mut *x0 };

    *x0 = 10;
    *x1 = 20;

    println!("{:?}", xs);
}
```

```sh
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
[0, 1, 2]
[10, 20, 2]
```

`x0`, `x1` の参照を重ねると、 [miri] はエラーを出します。

## ？ ~~検証 3-2: `&mut T` → `*mut T` → `&mut T` の `*mut T` は `&T` と同じ扱い~~

**上の検証 3 (追記) もご参照ください**

同じ値の `*mut T` と `&mut T` を取ります。 `*mut T` は後でデリファレンスします。デリファレンスするまでの間、同じ値への他の参照を作ったとき、 aliasing rules (実行時の borrow rules と解釈) に違反するしょうか？

```rust
// unsound!
fn main() {
    let xs = &mut [0, 1, 2];

    let x1_ptr = &mut xs[1] as *mut _;

    {
        let x1_mut = &mut xs[1];
        *x1_mut = 20;
    }

    unsafe {
        *x1_ptr = 300;
    }

    println!("{:?}", xs);
}

```

```
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
error: Undefined Behavior: no item granting write access to tag <untagged> at alloc1480 found in borrow stack.
  --> src/main.rs:45:9
   |
45 |         *x_ptr = 300;
   |         ^^^^^^^^^^^^ no item granting write access to tag <untagged> at alloc1480 found in borrow stack.
   |
~~
error: aborting due to previous error
```

可変参照を取ると違反しました。このエラー、やはり alias のエラーではないでしょうか？

`x1_mut` を `x1_ref` に置き換えると通ります:

```diff-rust
fn main() {
    let xs = &mut [0, 1, 2];

    let x1_ptr = &mut xs[1] as *mut _;

+    {
+        let x1_ref = &xs[1];
+        println!("{:?}", *x1_ref);
+    }
-    {
-        let x1_mut = &mut xs[1];
-        *x1_mut = 20;
-    }

    unsafe {
        *x1_ptr = 300;
    }

    println!("{:?}", xs);
}
```

```rust
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
1
[0, 300, 2]
```

`x1_mut` を `x1_ptr_2` に置き換えても通ります:

```diff-rust
fn main() {
    let xs = &mut [0, 1, 2];

    let x1_ptr = &mut xs[1] as *mut _;

+    {
+        let x1_ptr_2 = &mut xs[1] as *mut _;
+        unsafe {
+            *x1_ptr_2 = 20;
+        }
+    }
-    {
-        let x1_mut = &mut xs[1];
-        *x1_mut = 20;
-    }
    unsafe {
        *x1_ptr = 300;
    }

    println!("{:?}", xs);
}
```

* `*mut T` と `&T`, `*mut T` と `*mut T` は共存できました。
* `*mut T` と `&mut T` は共存できませんでした。

[miri] を信じるなら、 `*mut T` は、デリファレンスするまでは `&T` と同じ扱いと言えるでしょう。

# まとめ

[miri] で試した限り、このようになっていました:

* Borrow rules: コンパイル時に borrow checker が課すルール。
* Aliasing rules: Unsafe で borrow rules を掻い潜ったとしても、実行時に borrow rules を守られなければ unsound とするルール。
* `UnsafeCell<T>` は interior mutability を提供する特殊な型である。
* ~~`&mut T` → `*mut T` → `&mut T` において、 `*mut T` は aliasing rules 上 `&T` と同じ扱い~~
* 追記: **やっぱりコンテナから複数の可変参照を取るのは非常に厳しい**
    * **`&mut xs[n]` はスライス全体の借用を取らない** 。しかし、 **うっかり** `xs.len()` などを呼ぶと、 **スライス全体の借用を取ってしまう** 。したがって、コンテナから複数の `&mut T` を取るのは非常に危険
    * TODO: `split_at_mut` の実装を見る
    * TODO: `split_at_mut` 他の方法で強引に借用の分割ができるか調べる

[miri] を通して、 `UnsafeCell` への理解が進みました。今日の [miri] の aliasing モデルは、 Rust が (将来的に) 想定するモデルとは別物ですが、今は miri が通したら sound だと思うことにします。

以上です！

## 備考: `Freeze`

`UnsafeCell` がいかに特別かは、 [簡潔なQでも言及されて][Q] います。謎のマーカー `Freeze` がついているので、やっぱり特別なのでしょう。

[Q]: https://qnighy.hatenablog.com/entry/2017/05/20/070000

## TODO: スライスの所有者の扱い？

[miri] は以下のコードを通します:

```rust
fn main() {
    let mut xs = vec![0i32, 1, 2];
    let x0 = unsafe { &mut *(&mut xs[0] as *mut _) };
    xs.len(); // <---------
    *x0 = 10;
    println!("{:?}", xs);
}
```

```sh
$ cargo +nightly miri run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `/Users/tbm/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/cargo-miri target/miri/x86_64-apple-darwin/debug/ub-test`
[10, 1, 2]
```

`xs.len()` では `&xs` を取っていますが、 `[T]` の参照 (`&xs[0..]`) を取ったことにはならないようですね。ふーむ。 Rust チョットワカラナイ……

[^1]: UB = undefined behavior. UB とは言うけれど、 Unspecified behavior / undefined behavior は区別しよう、みたいな議論も [unsafe-code-guidelines](https://github.com/rust-lang/unsafe-code-guidelines) にあったような……
[^2]: Sound = 100% 実行時に UB を起こさない　だと思います。

