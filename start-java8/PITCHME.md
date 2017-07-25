今から始める

Java8 Lambda
===

きつねさんのリポジトリーとともに

---

2014年3月18日にJava8がリリースされてから、3年と4ヶ月が経ちました

今年9月くらいにリリースされるJava9に備えている方も多いと思います

そこでJava8を振り返ってみるプレゼンテーションを作ってみました

(以上、ここまで煽りです)

---

自己紹介
===

* 持田
* 入社8ヶ月
* Java8飽きてKotlin書いてる
* 今年オススメのゲームはニーアオートマタ

---

本日の元になるソースの引用
===

きつねさん(open jdk コミッター)のリポジトリー

https://bitbucket.org/bitter_fox/lambda.git

---

lambda式とは
===

* 計算速度をあげるための試みはCPUのクロックを上げていく方法ではなく、計算の多重度をあげる方向に
* 他のCPUに計算してもらうために計算をオブジェクトとして扱えるようにしたい
* オブジェクトとして扱えるのはいいけど、Javaでは無名関数の作り方が尋常じゃないほど面倒くさい

---?image=images/back-1.jpg&size=auto 90%

# ラムダ式登場

---

ラムダ式の条件
===

* Single Abstract Method (SAM) なインターフェースであること
  * デフォルトメソッドは含まない

---

例
===

```java
public interface Gen<T> {
    // たった一つだけのabstractメソッド
    @FunctionalInterface
    T ofRandom();
    // デフォルトメソッドは含まない
    <T, R> Gen<R> map(Function<? super T, ? extends R> f) {
        return () -> f.apply(ofRandom());
    }
}
```

---

練習1
===

### `java.lang.Runnable` をラムダで書いてみる

```java
final Rannable runnable = new Runnable() {
    @Override
    public void run() {
        System.out.println("にゃーん");
    }
};
runnable.run();
```

---

練習1-答え
===

```java
final Rannable runnable = () -> System.out.println("にゃーん");
runnable.run();
```

* 引数なしの場合のラムダは `() ->` ではじめる

---

練習2
===

### `java.util.concurrent.Callable<V>` をラムダで書いてみる

```java
final Callable<InputStream> callable = new Callable<InputStream> {
    @Override
    public InputStream call() throws Exception {
        return new FileInputStream("にゃーん.jpg");
    }
};
```

---

練習2-答え
===

```java
final Callable<InputStream> callable = () -> {
    return new FileInputStream("にゃーん.jpg");
};
```

* まず、引数なしなので `() -> ` で書き始められます。

---

練習2-答え(2)
===

```java
final Callable<InputStream> callable = () -> {
    new FileInputStream("にゃーん.jpg");
};
```

* 返されるオブジェクトは推論可能なので、 `return` を省略できます

---

練習2-答え(3)
===

```java
final Callable<InputStream> callable = () -> new FileInputStream("にゃーん.jpg");
```

* 一つの式しかないのでセミコロンと `{}` 波括弧は省略できます

---

