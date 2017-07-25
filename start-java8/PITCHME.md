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

* 1つの式しかないので `return` を省略できます

---

練習2-答え(3)
===

```java
final Callable<InputStream> callable = () -> new FileInputStream("にゃーん.jpg");
```

* 一つの式しかないのでセミコロンと `{}` 波括弧は省略できます

---

練習3
===

### `Consumer<V>` をラムダで書いてみます

```java
final List<Cat> cats = getCats();
cats.forEach(new Consumer<Cat>(){
    @Override
    public void accept(final Cat cat) {
        System.out.println(cat.miao());
    }
});
```

---

練習3-答え
===


```java
final List<Cat> cats = getCats();
cats.forEach( (final Cat cat) -> {
    System.out.println(cat.miao());
});
```

* パラメーター一つの場合は書き始めを `([型名] 変数名) -> ` に省略できます

---

練習3-答え(2)
===


```java
final List<Cat> cats = getCats();
cats.forEach( (cat) -> {
    System.out.println(cat.miao());
});
```

* 引数がメソッドのシグネチャー(`List#forEach(Consumer<T>)`)から推論可能なので省略できます

---

練習3-答え(3)
===


```java
final List<Cat> cats = getCats();
cats.forEach( cat -> {
    System.out.println(cat.miao());
});
```

* 引数が1つの場合、パラメーターの前後の `()` を省略できます

---

練習3-答え(4)
===


```java
final List<Cat> cats = getCats();
cats.forEach( cat -> System.out.println(cat.miao()));
```

* 式が1つなので `{}` とセミコロンを省略できます

---

練習4
===

### `java.util.function.Function` を書いてみる

```java
final List<Cat> cats = getCats();
cats.stream()
    .map(new Function<Cat, Food>(){
        @Override
        public Food apply(Cat cat) {
            return cat.mostFavoriteFood();
        }
    })
    .collect(Collectors.toList());
```

---

練習4-答え
===

```java
final List<Cat> cats = getCats();
cats.stream()
    .map(cat -> cat.mostFavoriteFood())
    .collect(Collectors.toList());
```

* これまでの問題に出てくる部分をまとめてラムダを省略しつつ書く

---

練習4-答え(2)
===

```java
final List<Cat> cats = getCats();
cats.stream()
    .map(Cat::mostFavoriteFood)
    .collect(Collectors.toList());
```

* `Cat#mostFavoriteFood` というレシーバーが `Cat` で引数なしのメソッドを呼び出しているので、 `[Class名]::[method名]` という形のメソッド参照に省略できる

---

基本的な型
===

* `Runnable` : `void -> void`
* `Consumer<T>` : `T -> void`
* `Supplier<T>` : `void -> T`
* `Function<T, R>` : `T -> R`
* `BiFunction<P, Q, R>` : `(P,Q) -> R`
* `BiConsumer<P, Q>` : `(P,Q) -> void`

---

よくある疑問
===

## ラムだって単なる無名クラスの糖衣構文でしょ？

---

回答
===

きっとどこかでマサカリ受けるのでやめておきましょう

---

回答-比較
===

無名クラスとラムダ

```java
cats.stream().map(new Function<Cat,Food>(){ ... }....)

cats.stream().map(Cat::mostFavoriteFood)....
```

---

回答-比較
===

バイトコード見てみると(無名クラス)

```
 8: invokeinterface #3,  1 // InterfaceMethod java/util/List.stream:()Ljava/util/stream/Stream;
13: new           #9       // class com/example/Foo$1
16: dup
17: aload_0
18: invokespecial #10      // Method com/example/Foo$1."<init>":(Lcom/example/Foo;)V
21: invokeinterface #5,  2 // InterfaceMethod java/util/stream/Stream.map:(Ljava/util/function/Function;)Ljava/util/stream/Stream;
```

---

回答-比較
===

バイトコード見てみると(無名クラス)

```
 8: invokeinterface #3,  1 // InterfaceMethod java/util/List.stream:()Ljava/util/stream/Stream;
13: invokedynamic #4,  0   // InvokeDynamic #0:apply:()Ljava/util/function/Function;
18: invokeinterface #5,  2 // InterfaceMethod java/util/stream/Stream.map:(Ljava/util/function/Function;)Ljava/util/stream/Stream;
```

---

これであなたも明日からJava8でラムダラムダ
===
