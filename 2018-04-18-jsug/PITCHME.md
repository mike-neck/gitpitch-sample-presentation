### Spring Boot アプリケーションの起動をほんの少し気持ちだけ速くしてみた

#### 持田(@mike_neck)

---

#### さっそく Spring Boot アプリケーションの起動をほんの少しだけ速くしてみたいと思います

---

#### まずは Java を用意します

---

#### 次に Spring Boot アプリケーションを用意します


---

#### 何もしない状態で起動してみましょう


---

#### Class Data Sharing?

* クラスをロードした状態をファイルにダンプしておいて、複数のJVMで共有する仕組み
* Java 8 からあったコマーシャルフィーチャーで、 Java 10 から OpenJDK で利用可能になった

---

#### やること

* アーカイブ対象のクラスのリスト(テキストファイル)を作る
* アーカイブファイル(バイナリーファイル)を作る
* アーカイブを読み込む

---

#### アーカイブ対象のクラスのリストを作る

以下のJVMオプションを指定してアプリケーションを起動する

* `-XX:+UnlockCommercialFeatures`
* `-XX:+UseAppCDS`
* `-Xshare:off`
* `-XX:DumpLoadedClassList={classList}`

---

#### アーカイブファイルを作る

以下のJVMオプションを指定してアプリケーションを起動する

* `-XX:+UnlockCommercialFeatures`
* `-XX:+UseAppCDS`
* `-Xshare:dump`
* `-XX:SharedClassListFile={classList}`
* `-XX:SharedArchiveFile={archiveFile}`

---

#### アーカイブを読み込む

以下のJVMオプションを指定してアプリケーションを起動する

* `-XX:+UnlockCommercialFeatures`
* `-XX:+UseAppCDS`
* `-Xshare:on`
* `-XX:SharedArchiveFile={archiveFile}`

---

#### 


