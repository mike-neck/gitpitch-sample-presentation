Etaについて
===

---

### 最近頭イカれてる(褒め言葉)なと思ったEtaというプログラミング言語について紹介します

---

Etaとは
===

Etaは次のような特徴を持つJVM言語です

* 静的多相型付け
* 純粋関数型
* 遅延評価戦略
* 代数的データ型のサポート
* Hindley/Milner型推論
* JVM上で動作

---

あれ？それって？？？
===

---

つまり<br/>Haskell
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

### Etaでできること

* Java、Haskell相互に利用可能
  * Javaの各種クラス・メソッドをHaskellから呼び出し
  * Haskellの関数をJavaから呼び出し
* Uber Jar を生成
  * Hackage/Haskell標準ライブラリーをJarにコンパイル
  * それらとアプリケーションをまとめて一つの実行可能Jarにパッケージング

---

### EtaでJavaを呼び出し

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

---

### Etaでできないこと

* Haskellの型をJavaにエクスポート
* インターフェース/抽象クラスの実装
  * SAMタイプのインターフェースは実装できる
* アノテーションのついたメソッド/クラス

---

### Etaを使うときの注意 1

* マッピングが全く型安全でない
  * クラスと型のマッピングが正しくできている場合は、Haskellのコンパイラーがプログラムの正しさを保証する
  * ただしマッピングが正しくない場合は、コンパイラーでもプログラムの正しさを保証できない
  * マッピングには `String` で自由に記述する箇所が多々ある
  * `String` の内容はコンパイラーでは検査できない


