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

Haskellですね
===

* EtaはHaskellの一つの方言です

---

#### まずは論より証拠

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

---

Etaの仕組み
===

* GHCの拡張の仕組みを利用してclassファイルおよびjarファイルを生成しているらしい
* GHCの拡張なので、他のGHC拡張(7.10.3)も利用可能
  * `RankNTypes`
  * `GADTs`
  * `FlexibleContexts`
  * ...etc

---

Etaのその他の特徴
===

* JVM言語なのでもちろんJavaのクラス・メソッドを使えます
* JavaのクラスをHaskell(Eta)の型で定義
* メソッドをHaskell(Eta)の関数で定義
* メソッドの呼び出しは `Java` モナドにつつまれる

---

EtaでJavaを呼び出し
===

```haskell
data {-# CLASS "java.nio.file.Path" #-} Path =
  Path (Object# Path) deriving (Class, Show, Eq)
foreign import java unsafe "@interface toAbsolutePath"
  toAbsolutePath :: Java Path Path
foreign imoprt java unsafe "@interface toString"
  toString :: (a <: Path) => Java a String
foreign import java unsafe "@static java.nio.file.Paths.get"
  getPath :: String -> JStringArray -> Java a Path

main :: IO ()
main = do
  pathString <- java $ do
    emptyArray <- arrayFromList []
    path <- getPath "foo.cabal" emptyArray
    absolutePath <- path <.> toAbsolutePath
    absolutePath <.> toString
  putStrLn pathString
```
@[1,2](`Path` クラスをHaskellに定義)
@[3,4,15](`Path#toAbsolutePath` メソッドをHaskellの関数で定義)
@[12](Javaメソッドの呼び出しは `Java` モナドを返す)


