Kotlinの
===

コンパイラーの
===

気持ちになる
===

クイズ
===

@mike_neck

---

* Kotlinのreflectionに関するクイズをやります
* 主に `KClass` に関する問題を中心に見ていきます
* 全問正解したからと言って特にナニがあるわけでもありません

---

### 自己紹介

* 持田(@mike_neck)
* directというチャットの会社(L is B)でｼﾞｬｯﾊﾞ書いてる
* kuick-checkというHaskellのQuick CheckをKotlinに移植したライブラリー作りました

---

第1問
===

---

### 1. コンパイルした時に作られるクラスファイルの数はいくつでしょう？

ファイル名は(`Exercise1.kt`)

```kotlin
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

* `Exercise1Kt.class`
  * `@file:JvmName` により名前が指定される
* `Exercise1$main$text$2.class`
  * `inline` にしていない関数が受け取った関数(lambda)のクラスが作られる

---

第2問
===

---

### 2. コンパイルした時に作られるクラスファイルの数はいくつでしょう？

```kotlin
interface NamedFile {
    val name: String
    fun greeting(): String = "Hello ${name}."
    companion object {
        fun create(name: String): NamedFile = object : NamedFile {
            override val name: String get() = name
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

* A: 2個
* B: 3個
* C: 5個
* D: 6個

---

### 3. 正解

C: 5個
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

---

第4問
===

---

### 4. 次のコードを実行した時の表示内容はどうなる？

(ファイル名は `App.kt`)

```kotlin
fun main(args: Array<String>) {
    val classLoader: ClassLoader = getClassLoader()
    val javaClass: Class<*> = classLoader.loadClass("AppKt")
    val kClass: KClass<*> = javaClass.kotlin
    val primaryConstructor: KFunction<*> = kClass.primaryConstructor
    val appkt = primaryConstructor.call()
    println(appkt)
}
```

* A: `KotlinReflectionInternalError`
* B: `UnsupportedOperationException`
* C: `IllegalStateException`
* D: `AppKt@53976f5c`

---

### 4. 正解

B: 
===

---

### 4. 解説

1. `FooKt` のような kt クラスは **package and file facades** と呼ばれる特殊なクラス
1. **package and file facades** に対するkotlin-reflectionによる操作はバージョン1.1.51時点で未サポート
1. kotlin-reflectionが未サポートのクラスファイル
  * `WhenMappings`
  * lambda
  * `DefaultImpls`
  * `enum`(一部未サポート)
  * `enum` の entry(一部未サポート)

---

### まとめ

* Kotlinコンパイラーが生成する特殊なクラスファイルにはいくつかのパターンがある
* kotlin-reflectionはそれらの特殊なファイルを扱えないようにしている
* Kotlinでフレームワーク的なものを作りたい場合、これらのパターンを把握していたほうがよい
