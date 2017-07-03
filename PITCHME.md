Etaについて
===

---

### 最近頭イカれてる(褒め言葉)なと思ったEtaというプログラミング言語について紹介します

---

Etaとは
===

Etaは次のような特徴を持つ言語です

* 静的多相型付け
* 代数敵データ型のサポート
* 純粋関数型
* 遅延評価戦略
* Hindley/Milner型推論
* JVM上で動作

---

あれ？それって？？？
===

---

Haskellのことです
===

---

まずは論より証拠
===

早速動かしてみましょう

```haskell
module Main where
main :: IO ()
main = putStrLn "Hello, Daitokai!"
```

```bash
$ etlas configure --enable-uberjar-mode
$ etlas build
$ java -jar dist/build/project-name/project-name.jar
```

