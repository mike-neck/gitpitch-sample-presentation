#### YouTrackを使いこなす

---

#### このプレゼンテーションについて

YouTrackのコンセプトを理解して、使いこなしのヒントをつかんでもらう

* コンセプトを抑える
* イシューを読む
* イシューに記録する

---

#### 自己紹介

* 持田(@mike_neck)
* チャットの会社でｼﾞｬｯﾊﾞしてる
* IntelliJ IDEA大好きおじさん
  * IntelliJが大好きすぎて隣の若手にIntelliJを布教してる

---

#### コンセプトを抑える

* YouTrackの使いこなし方を学ぶためにそのコンセプトを抑えることは重要です
* ツールの機能追加・リリースはコンセプトを元に判断する
* コンセプトを理解することはツールの機能を理解する一番の近道

---

#### YouTrack

* Issue tracking and project management tool for **Developers**.
* プログラマーが使いやすいissue tracking system

---

#### プログラマーが使いやすいとは？

* 以下、ホームページより引用
  * Smart search(補完機能がついたクエリによる検索)
  * Keep your hands on the keyboard(キーボードでなんでもできる)
  * Command window(複数の操作、複数のチケットの操作を一気に)

---

#### イシューを読む

* クエリーを使ってチケットを探す
* チケットの画面で詳細を読む

---

#### クエリーを使ってチケットを探す

すべての基本はクエリーです。バーンダウンチャートやアジャイルボードといった機能はすべてクエリーによって作られていると言っても過言ではありません。

クエリーで覚えるべきポイントは二つです。

* キーとバリュー形式で指定する
* キーの値だけを覚えておけばよい

---

#### クエリーのポイント

##### キーとバリュー形式で指定する

条件を一つ指定する

```
# キー: バリュー
Asignee: shinya.mochida
```

条件を複数指定する(AND検索)

```
Asignee: shinya.mochida Status: Open
```

(`#{Assign to me}` とか `#UnResolved` などの特殊な条件もあります)