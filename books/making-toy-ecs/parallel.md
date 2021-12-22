---
title: "[WIP] 並列実行"
---

`Send` な system は他スレッドに送って並列実行できます。

[`rayon`] で一番簡単な並列実行を 2 種類実装します。

[`rayon`]: https://docs.rs/rayon/latest/rayon/

# WIP

……と言いたかったのですが、カレンダー記事当日は実装が間に合っておりません 🙇

# World への access はシグネチャで分かる

```diff-rust:sys.rs
pub trait BorrowWorld<'w> {
    unsafe fn borrow(w: &'w World) -> Self;
+    fn access() -> Access;
}
+
+#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
+pub enum Access {
+    Res(TypeId),
+    ResMut(TypeId),
+    Comp(TypeId),
+    CompMut(TypeId),
+}
+
+impl Access {
+    pub fn conflicts(self, other: Self) -> bool {
+        match (self, other) {
+            (Self::Res(i0), Self::ResMut(i1)) => i0 == i1,
+            (Self::ResMut(i0), Self::Res(i1) | Self::ResMut(i1)) => i0 == i1,
+            (Self::Comp(i0), Self::CompMut(i1)) => i0 == i1,
+            (Self::CompMut(i0), Self::Comp(i1) | Self::CompMut(i1)) => i0 == i1,
+            _ => false,
+        }
+    }
+}
```

リファレンス実装: [d221e4a](https://github.com/toyboot4e/toecs/commit/d221e4a2295bcfebd4c629e3a4ae2942be272232)

# `World::run_par`

WIP

Send な関数に `ParalleSystem` を実装する

# `Iter::iter_par`

WIP

