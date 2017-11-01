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

### 1. コンパイルした時に作られるクラスファイルの数はいくつでしょう？

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

* A: 1個
* B: 2個
* C: 3個
* D: コンパイルエラー

---

### 1. 正解

B: 2個
===

---

### 1. 解説

以下の2つのファイルが作られます

* `Exercise1.class`
  * `@file:JvmName` により名前が指定される
* `Exercise1$main$text$2.class`
  * `inline` にしていない `or` 関数が受け取った関数のクラスが作られる

---

第2問
===

---

### 2. コンパイルした時に作られるクラスファイルの数はいくつでしょう？

```kotlin
interface NamedFile {
    val name: String
    val extension: String
    fun preciseName(): String = "$name.$extension"
    companion object {
        fun create(name: String, extension: String): NamedFile = object : NamedFile {
            override val name: String get() = name
            override val extension: String get() = extension
        }
    }
}
```

* A: 2個
* B: 3個
* C: 4個
* D: コンパイルエラー

---

### 2. 正解

C: 4個
===

---

### 2. 解説

次の4個のファイルが作られます

* `NamedFile.class`
  * インターフェースクラス
* `NamedFile$DefaultImpls.class`
  * インターフェースのデフォルト実装
* `NamedFile$Companion.class`
  * コンパニオンクラス
* `NamedFile$Companion$create$1.class`
  * `create` メソッドで作られるインターフェースの実装クラス

---

第3問
===

---

### 3. コンパイルした時に作られるクラスファイルの数はいくつでしょう？

```kotlin
enum class Bool(val asBoolean: Boolean) {
    TRUE(true) { override val asInt: Int get() = 1 },
    FALSE(false) { override val asInt: Int get() = 0 };
    abstract val asInt: Int
    companion object {
        fun not(bool: Bool): Bool = when (bool) {
            Bool.TRUE -> FALSE
            Bool.FALSE -> TRUE
        }
    }
}
```

* A: 1個
* B: 2個
* C: 3個
* D: 5個

---

### 3. 正解

D: 5個
===

---

### 3. 解説

1. `enum class` は一つのクラスにコンパイルされます
  * `Bool.class`
1. ただし、 `enum entry` が実装を持つと `entry` 一つにつき一つクラスが作られる
  * `Bool$TRUE.class`
  * `Bool$FALSE.class`
1. `when` に `enum class` が使われると `WhenMappings` というクラスが作られる
  * `Bool$Companion$WhenMappings.class`
  * `Bool$Companion.class`



