Kotlinの
===

Reflectionクイズ
===

@mike_neck

---

* Kotlinのreflectionに関するクイズをやります
* 主に `KClass` に関する問題を中心に見ていきます
* 全問正解したからと言って特にナニがあるわけでもありません

---

### 自己紹介

* 持田(@mike_neck)
* チャットの会社でｼﾞｬｯﾊﾞ書いてる
* kuick-checkというHaskellのQuick CheckをKotlinに移植したライブラリー作りました

---

第1問
===

---

### 1. 次のファイルをコンパイルした時に作られるクラスファイルの数はいくつでしょう？

```kotlin
@file:JvmName("Exercise1")
fun main(args: Array<String>) {
    val text = message.whenNotNull { println(it) }
            .or { "hello" }
    println("result -> $text")
}
val message: String? = "exercise 1"
inline fun <A> A?.whenNotNull(f: (A) -> Unit): A? = if (this != null) this.apply(f) else this
fun <A> A?.or(g: () -> A): A = this ?: g()
```

* A: 0個
* B: 1個
* C: 2個
* D: コンパイルエラー


