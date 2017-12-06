好きな
===

コード
===

について
===

@mike_neck

---

* 好きなコードについて語っていきます
* 理屈は特にないですが、わりと人から影響を受けています

---

### 小さいメソッド、小さいクラス

* 小さいメソッド、小さいクラスが好きです
* ただ単純に何をやっているか理解できないだけです
* 一つのメソッドは5行以下、クラスも100行くらいまでがちょうどよいです

---

### クラスを継承するよりはインターフェースを実装

* 何らかのクラスを継承しているクラスを改造するのはわりと怖い
* 元のクラスをいじって、継承しているクラスに影響が出たりとか
* 継承しているクラスで欲しい値を取りたいがために、元のクラスを無理矢理いじったりとか
* どうしても実装しないといけない場合は、移譲できないか検討する

---

### 全部publicメソッドでいいと思ってる

* (自分もprivateメソッドを作ってしまうというのは置いといて)
* privateメソッドの存在はそのクラスが元々持っていた意味が大きくなって複数の意味をもってしまったという状態
* 新しく作られたメソッド/役割の意味を表す別のクラスを作ってpublicメソッドにする

---

### 振る舞いをもつクラス

* getter/setterしかないクラスは◯ね
* 値を持っているクラスが持っている値を操作する = オブジェクト指向の考え方
* `if`/`switch` 文の後にgetterを使っていたら、 `if` の対象となったクラスにその振る舞いを持てないか検討する

---

### 何か実例っぽいの

```java
List<UserScore> getDailyRanking(final LocalDate date) {
  final LocalDate today = timeRepository.getToday();
  if (date.isBefore(today)) {
    return Collections.emptyMap();
  }
  final List<UserScore> userScoreList = gameDao.dailyUserScoreList(date);
  if (userScoreList.isEmpty()) {
    return Collections.emptyMap();
  }
  final Set<UserId> users = userScore.stream().map(UserScore::getUserId).collect(toSet());
  final Collection<UserId> premiumUsers = userExternalRepository.findPremiumUsers(users);
  if (premiumUsers.isEmpty()) {
    return Collections.emptyMap();
  }
  return userScoreList.stream()
    .filter(s -> premiumUsers.contains(s.getUserId()))
    .sorted((l,r) -> Long.compare(l.getScore(), r.getScore()))
    .collect(toList());
}
```

---

### 何か実例っぽいの

* `LocalDate` を取ってきて `LocalDate` と比較して何かをする
* のではなくて、データを取得するオブジェクトが比較結果を返してほしい

```java
final LocalDate today = timeRepository.getToday();
if (date.isBefore(today)) {
  return Collections.emptyMap();
}
```

↓

```java
if (!timeRepository.isBeforeOfToday(date)) {
  return Collections.emptyMap();
}
```


