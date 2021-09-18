# Recommender Systems

A recommender system, or a recommendation system, is a subclass of information filtering system that seeks to predict the "rating" or "preference" a user would give to an item.

*Source:* Wikipedia

## Aproaches

There are a lot of types for Recommender systems, some of them being **Collaborative filtering, Content-based filtering, Session-based recommender systems, etc**. In this script we will implement the Content-based filtering. The idea is that the system will recommend to the users similar items to what they already buy/use. The similarities between items are calculated based on the contents/attributes of the items.


## Usage
We will use a netflix dataset that you can find on [Kaggle] https://www.kaggle.com/shivamb/netflix-shows . This dataset contains information only for the movies. There is no information for the users and therefore we will only calculate the similarities.

In my opinion, the only good information to be used are:
* Cast
* Director
* Movie genre


There are `22820` unique actors, `20` unique genres and `3926` unique directors and so we can create a total of `22820+20+3926 = 26766` attributes/columns for every movie. We can then create a binary matrix - every movie will have 1's in the corresponding casts, genre and director cells. We will end up with a 4834×26766 sparse matrix and we will perform the Cosine Similarity measurement.


Let's have a look at Quentin Tarantino's movies:

id|Django Unchained|Inglourious Basterds|Jackie Brown|Kill Bill: Vol. 1|Kill Bill: Vol. 2|Pulp Fiction|The Hateful Eight
-|-|-|-|-|-|-|-
1|The Hateful Eight|Django Unchained|El Camino: A Breaking Bad Movie|Kill Bill: Vol. 2|Kill Bill: Vol. 1|A Clockwork Orange|Django Unchained
2|Casino Tycoon|By the Sea|Small Crimes|The Hateful Eight|The Hateful Eight|Mean Streets|Kill Bill: Vol. 1
3|Casino Tycoon 2|National Treasure|Righteous Kill|Disciples Of The 36th Chamber|Disciples Of The 36th Chamber|Who's That Knocking at My Door?|Kill Bill: Vol. 2
4|The Next Karate Kid|Haywire|The Score|Code Name: The Cleaner|Legendary Weapons of China|Alice Doesn't Live Here Anymore|Jackie Brown 
5|American Son|Kill Bill: Vol. 1|반드시 잡는다|Legendary Weapons of China|Blood Father|Taxi Driver|Pulp Fiction
6|The Bittersweet|Kill Bill: Vol. 2|Oththa Seruppu Size 7|Return To The 36th Chamber|Return To The 36th Chamber|Jackie Brown|Machete Kills
7|Inglourious Basterds|Slow West|An Ordinary Man|Charlie's Angels|Inglourious Basterds|The Hateful Eight|Cave
8|Jackie Brown|Aftershock|Silver Linings Playbook|Inglourious Basterds|Wyatt Earp|187|Cut Bank
9|187|Ocean's Thirteen|Taxi Driver|Wyatt Earp|Pulp Fiction|Carrie|Mojave
10|Faraar|Ocean's Twelve|The Hateful Eight|Charlie's Angels: Full Throttle|Die Another Day|Django Unchained|Hickok


Some movies are 100% similar, like:

title|cast|genre|director
-|-|-|-
GODZILLA City on the Edge of Battle|Mamoru Miyano, Takahiro Sakurai, Kana Hanazawa, Tomokazu Sugita, Yuki Kaji, Junichi Suwabe, Reina Ueda, Ari Ozawa, Daisuke Ono, Kenta Miyake, Kenyu Horiuchi, Kazuya Nakai, Kazuhiro Yamaji|Action & Adventure, Anime Features, International Movies|Kobun Shizuno, Hiroyuki Seshita
GODZILLA The Planet Eater|Mamoru Miyano, Takahiro Sakurai, Kana Hanazawa, Tomokazu Sugita, Yuki Kaji, Junichi Suwabe, Reina Ueda, Ari Ozawa, Daisuke Ono, Kenta Miyake, Kenyu Horiuchi, Kazuya Nakai, Kazuhiro Yamaji|Action & Adventure, Anime Features, International Movies|Kobun Shizuno, Hiroyuki Seshita

