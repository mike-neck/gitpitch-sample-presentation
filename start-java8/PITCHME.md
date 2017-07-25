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

---?image=start-java8/images/back-1.jpg?size=auto 90%

### ラムダ式登場

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
  T ofRandom();

  <T, R> Gen<R> map(Function<? super T, ? extends R> f);
}
```

