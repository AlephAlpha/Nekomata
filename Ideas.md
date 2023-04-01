# 一些想法

Nekomata 现在已经有了一个非常简单的解释器。不过已有的内置函数非常少，写不出什么有用的程序。准备等内置函数丰富一些之后再写文档。

先在 [Code Golf Stack Exchange](https://codegolf.stackexchange.com/) 上随便找一些题目，看看要用 Nekomata 解答的话会是什么样子，需要加上哪些内置函数，或者别的什么功能。

尤其要参考 [Brachylog](https://github.com/JCumin/Brachylog)、[Vyxal](https://github.com/Vyxal/Vyxal)、[05AB1E](https://github.com/Adriandmen/05AB1E)、[Jelly](https://github.com/DennisMitchell/jellylanguage)、[Husk](https://github.com/barbuz/Husk) 等语言的解答，看看有什么可以借鉴的地方。

以下函数都写全称，不用缩写；但计算长度时还是用缩写，假设每个内建函数和助词都只占一个字节。

此处总结一下 Code Page 中已有但还没有用上的字符：

```
¥§×∂∕√∞∩≈≢&'.FGHKOVWXY`gkvwyz|
```

有些是已经确定分配给什么函数的，比如说 `×` 给 `\convolve`，`∕` 给 `\setMinus`，`∩` 给 `\intersect`，`√` 给 `\sqrt`。别的都还没想好。

LiberationMono 字体所支持的字符也列举于此，以后新的符号可以从这里挑选：

```
 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcde
fghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌ
ÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒ
ēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘ
řŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞ
ƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟǠǡǢǣǤ
ǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪ
ȫȬȭȮȯȰȱȲȳȴȵȶȸȹȺȻȼȽȾȿɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰɱ
ɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯʰʱʲʳʴʵʶʷ
ʸʹʺʻʼʽʾʿˀˁ˂˃˄˅ˆˇˈˉˊˋˌˍˎˏːˑ˒˓˔˕˖˗˘˙˚˛˜˝˞˟ˠˡˢˣˤ˥˦˧˨˩˪˫ˬ˭ˮ˯˰˱˲˳˴˵˶˷˸˹˺˻˼˽
˾˿̀́̂̃̄̅̆̇̈̉
ʹ͵ͺͻͼͽ;΄΅Ά·ΈΉΊΌΎΏΐΑΒΓΔΕΖΗΘ
ΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟϠ
ϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦ
ЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюяѐёђѓєѕіїјљњћќѝўџѠѡѢѣѤѥѦѧѨѩѪѫѬ
ѭѮѯѰѱѲѳѴѵѶѷѸѹѺѻѼѽѾѿҀҁ҂҃҄҅҆҇҈҉ҊҋҌҍҎҏҐґҒғҔҕҖҗҘҙҚқҜҝҞҟҠҡҢңҤҥҦҧҨҩҪҫҬҭҮүҰұҲ
ҳҴҵҶҷҸҹҺһҼҽҾҿӀӁӂӃӄӅӆӇӈӉӊӋӌӍӎӏӐӑӒӓӔӕӖӗӘәӚӛӜӝӞӟӠӡӢӣӤӥӦӧӨөӪӫӬӭӮӯӰӱӲӳӴӵӶӷӸ
ӹӺӻӼӽӾӿԀԁԂԃԄԅԆԇԈԉԊԋԌԍԎԏԐԑԒԓԚԛԜԝ֑֖֚֒֓֔֕֗֘֙
־ֿ׀ׁׂ׃ׅׄ׆ׇאבגדהוזחטיךכלםמןנסעףפץצקרשתװױײ׳״ᴀᴁᴂᴃᴄᴅᴆᴇᴈᴉᴊᴋᴌᴍᴎᴏᴐᴑᴒᴓᴔᴕ
ᴖᴗᴘᴙᴚᴛᴜᴝᴞᴟᴠᴡᴢᴣᴤᴥᴦᴧᴨᴩᴪᴫᴬᴭᴮᴯᴰᴱᴲᴳᴴᴵᴶᴷᴸᴹᴺᴻᴼᴽᴾᴿᵀᵁᵂᵃᵄᵅᵆᵇᵈᵉᵊᵋᵌᵍᵎᵏᵐᵑᵒᵓᵔᵕᵖᵗᵘᵙᵚᵛ
ᵜᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᵫᵬᵭᵮᵯᵰᵱᵲᵳᵴᵵᵶᵷᵸᵹᵺᵻᵼᵽᵾᵿᶀᶁᶂᶃᶄᶅᶆᶇᶈᶉᶊᶋᶌᶍᶎᶏᶐᶑᶒᶓᶔᶕᶖᶗᶘᶙᶚᶛᶜᶝᶞᶟᶠᶡ
ᶢᶣᶤᶥᶦᶧᶨᶩᶪᶫᶬᶭᶮᶯᶰᶱᶲᶳᶴᶵᶶᶷᶸᶹᶺᶻᶼᶽᶾᶿ᷂᷀᷁᷃᷄᷅᷆᷇᷈᷉ḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏḐḑḒḓḔḕḖḗḘḙḚ
ḛḜḝḞḟḠḡḢḣḤḥḦḧḨḩḪḫḬḭḮḯḰḱḲḳḴḵḶḷḸḹḺḻḼḽḾḿṀṁṂṃṄṅṆṇṈṉṊṋṌṍṎṏṐṑṒṓṔṕṖṗṘṙṚṛṜṝṞṟṠ
ṡṢṣṤṥṦṧṨṩṪṫṬṭṮṯṰṱṲṳṴṵṶṷṸṹṺṻṼṽṾṿẀẁẂẃẄẅẆẇẈẉẊẋẌẍẎẏẐẑẒẓẔẕẖẗẘẙẚẛẞẠạẢảẤấẦầẨẩ
ẪẫẬậẮắẰằẲẳẴẵẶặẸẹẺẻẼẽẾếỀềỂểỄễỆệỈỉỊịỌọỎỏỐốỒồỔổỖỗỘộỚớỜờỞởỠỡỢợỤụỦủỨứỪừỬửỮữ
ỰựỲỳỴỵỶỷỸỹἀἁἂἃἄἅἆἇἈἉἊἋἌἍἎἏἐἑἒἓἔἕἘἙἚἛἜἝἠἡἢἣἤἥἦἧἨἩἪἫἬἭἮἯἰἱἲἳἴἵἶἷἸἹἺἻἼἽἾἿ
ὀὁὂὃὄὅὈὉὊὋὌὍὐὑὒὓὔὕὖὗὙὛὝὟὠὡὢὣὤὥὦὧὨὩὪὫὬὭὮὯὰάὲέὴήὶίὸόὺύὼώᾀᾁᾂᾃᾄᾅᾆᾇᾈᾉᾊᾋᾌᾍᾎᾏ
ᾐᾑᾒᾓᾔᾕᾖᾗᾘᾙᾚᾛᾜᾝᾞᾟᾠᾡᾢᾣᾤᾥᾦᾧᾨᾩᾪᾫᾬᾭᾮᾯᾰᾱᾲᾳᾴᾶᾷᾸᾹᾺΆᾼ᾽ι᾿῀῁ῂῃῄῆῇῈΈῊΉῌ῍῎῏ῐῑῒΐῖῗῘῙ
ῚΊ῝῞῟ῠῡῢΰῤῥῦῧῨῩῪΎῬ῭΅`ῲῳῴῶῷῸΌῺΏῼ´῾ ‌‍‎‏‐‒–—―‗‘’‚‛“”„‟†‡•…‪‫‬‭‮‰′″‴‹›‼‾⁄
⁞⁪⁫⁬⁭⁮⁯⁴⁵⁶⁷⁸⁹ⁿ₀₁₂₃₄₅₆₇₈₉ₐₑₒₓₔ₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₿⃰℅ℓ№℗™Ω℮⅍ⅎ⅓⅔⅛⅜⅝⅞ↄ←
↑→↓↔↕↨⇔∂∆∏∑−∕∙√∞∟∩∫≈≠≡≢≤≥⌂⌐⌠⌡─│┌┐└┘├┤┬┴┼═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬▀
▄█▌▐░▒▓■□▪▫▬▲►▼◄◊○◌●◐◑◒◓◔◕◖◗◘◙◦☺☻☼♀♂♠♣♥♦♩♪♫♬♯ⱠⱡⱢⱣⱤⱥⱦⱧⱨⱩⱪⱫⱬⱭⱱⱲⱳⱴⱵⱶⱷ⸗ꜗꜘꜙ
```

---

## [Group elements by their displacement](https://codegolf.stackexchange.com/q/239999/9288)

参考现有的解答的话，大概是这样：

```
\rangeLength1 \absDiff \groupBy
```

或者

```
\sort \absDiff \groupBy
```

- [ ] `\rangeLength1`：输入一个列表，输出一个从 1 开始到列表长度的列表。不清楚这个函数用得多不多，用得不多的话可以拆成 `\range1` 和 `\length` 两个更常用的函数。
- [x] `\sort`：把一个列表按照元素的大小排序。应该是一个比较常用的函数，但在 non-deterministic 语言可能不太好实现。
- [ ] `\absDiff`：求两个数的绝对值差；自动向量化。同样，也可以考虑拆成 `\sub` 和 `\abs` 两个函数。
- [ ] `\groupBy`：输入两个列表，其长度必须一致。根据第一个列表的元素，将第二个列表的元素分组。比如输入 `[1, 2, 1, 2]` 和 `[a, b, c, d]`，输出 `[[a, c], [b, d]]`。

其它语言里的 `\groupBy` 一般都是高阶函数，输入的是一个函数和一个列表，而非两个列表。我觉得输入两个列表更方便一些，能更好地利用自动向量化。

如果用高阶函数的话，`\groupBy` 就是一个助词，而不是一个普通的函数。大概看起来是这样：

```
\groupBy { \index \absDiff }
```

- [ ] `\index`：输入一个列表和一个元素，输出这个元素在列表中的索引。这个函数肯定是比较常用的，不过不知道要不要弄成 non-deterministic 的。

即使可以省略右括号（目前未实现），这个解答也比前面的要多一个字节。

## [Smallest groups in an array](https://codegolf.stackexchange.com/q/79037/9288)

```
\chunks \dup \map \length \minimumBy
```

- [ ] `\chunks`：将一个列表从相邻元素不相等的地方拆开，得到一个列表的列表。比如输入 `[1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 6]`，输出 `[[1], [2, 2], [3, 3, 3], [4], [5, 5, 5, 5], [6]]`。
- [ ] `\minimumBy`：输入两个列表，其长度必须一致。根据第一个列表中最小的元素的索引，返回第二个列表中对应的元素。由于最小的元素可能不止一个，所以这个函数是 non-deterministic 的。

和前面的 `\groupBy` 一样，`\minimumBy` 也是一个普通的函数而不是助词。但如果改成助词的话，解答会更短一些：

```
\chunks \minimumBy \length
```

看来是用普通函数还是助词还需要斟酌。可能两种都要支持，不过名字怎样区分是个问题。

## [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288)

```
\nub \setMinus \nub \length
```

- [x] `\nub`：输入一个列表，将其中的重复元素去掉。比如输入 `[1, 2, 3, 2, 1]`，输出 `[1, 2, 3]`。不清楚要不要排序。这道题用不着排序，但其它题目可能会用到。
- [ ] `\setMinus`：输入两个列表，返回第一个列表中不在第二个列表中的元素。比如输入 `[1, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 3]`。如果第二个列表中的元素在第一个列表中有重复，那么只删除一个。比如输入 `[1, 2, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 2, 3]`。

## [Consolidate an Array](https://codegolf.stackexchange.com/q/70779/9288)

这道题需要用到 sortBy。和前面说过的 `\groupBy` 和 `\minimumBy` 一样，也需要考虑它究竟要写成助词还是普通的函数。

如果是助词：

```
\sortBy \logicalNot
```

如果是普通的函数：

```
\logicalNot \sortBy
```

- [ ] `\sortBy`：输入两个列表，其长度必须一致。将第一个列表中的元素按照第二个列表中的元素的大小进行排序。这要求排序算法是稳定的。
- [x] `\logicalNot`：逻辑非。如果输入的数为 0，返回 1；如果输入的数不为 0，返回 0。不过给一个根本没有布尔值的语言加上逻辑运算符有点奇怪。就当它是一个数学函数好了。

## [There's more than one way to skin a set](https://codegolf.stackexchange.com/q/247326/9288)

```
\subset \sum \allValues \dup \nub \setMinus \nub
```

参考了前面 [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288) 的解答。

也许要给 Nekomata 加上一个输出时自动去重的选项。

## [Split some points](https://codegolf.stackexchange.com/q/257870/9288)

参考现有的 Vyxal 解答：

```
\integer \allValues \dup \anyPair \lengthIs \predicate { 1 \neg \cons \swap \map { 1 \cons \reverse \dot \sign \nonzero } \sum 0 \eq }
```

- [x] `\integer`：non-deterministic 地输出任意一个整数，按 `0, 1, -1, 2, -2, 3, -3, ...` 的顺序。
- [ ] `\anyPair`：输入两个列表，输出一个二元组，第一个元素是第一个列表的任意一个元素，第二个元素是第二个列表的任意一个元素。这个函数是 non-deterministic 的。实现时要注意两个列表的长度都可能是无穷的。
- [x] `\dot`：求两个列表的点积。如果两个列表的长度不同，返回 `Fail`。
- [x] `sign`：求一个整数的符号。如果是正数，返回 `1`；如果是负数，返回 `-1`；如果是零，返回 `0`。

## [Chunk + Enumerate a list of digits](https://codegolf.stackexchange.com/q/189932/9288)

参考 Jonathan Allan 的 Jelly 解答：

```
\prefix \nonempty \chunks \length
```

几次用到 `\prefix` 都只需要用到非空的前缀。可以考虑让它只输出非空的前缀，省去 `\nonempty`。但如果碰上确实需要空前缀的情况，需要补上的字节数会更多。`\suffix` 和 `\subsequence` 也有类似的问题。

## [Approximate a root of an odd degree polynomial](https://codegolf.stackexchange.com/q/258021/9288)

假设 Nekomata 支持有理数，可以参考 Charcoal 的解答：

```
\abs \sum \dup \iterate { \dupDip { \dip \dup \sub \choice \predicate { \fromBase 0 \greaterEq } \oneValue } 2 \div } \swap
```

23 个字节，而且不知道对不对。也许还有改进的空间。

## [Mode (most common element) of a list](https://codegolf.stackexchange.com/q/42529/9288)

```
\map \count \maximumBy
```

需要 `-1` flag 来仅输出第一个结果。

- [ ] `\count`：求一个元素在一个列表中出现的次数。
- [ ] `\maximumBy`：输入两个列表，其长度必须一致。根据第一个列表中最大的元素的索引，返回第二个列表中对应的元素。由于最大的元素可能不止一个，所以这个函数是 non-deterministic 的。

```
\sort \chunks \last \first
```

## [String rotation - output string repeatedly moving first character to the end](https://codegolf.stackexchange.com/q/177221/9288)

目前的解答：

```
\split \nonempty \swap \join
```

- [x] `\split`：non-deterministic 地将一个列表拆成两个列表

可以考虑加一个 `\rotate` 函数：

```
\length \range0 \rotate
```

- [ ] `\rotate`：输入一个整数和一个列表，将列表的前 n 个元素移动到列表的末尾，n 为输入的整数。如果 n 为负数或者大于列表的长度，将其对列表的长度取模。

甚至干脆搞一个 Non-deterministic 的 `\rotation` 函数：

```
\rotation
```

- [ ] `\rotation`：non-deterministic 地求一个列表的所有可能的旋转。比如说，`[1, 2, 3]` 的所有可能的旋转是 `[1, 2, 3]`, `[2, 3, 1]`, `[3, 1, 2]`。

但这里的几个新函数未必常用。先看看其它的问题。

## [Smallest Bit Rotation](https://codegolf.stackexchange.com/q/257372/9288)

前面刚说未必常用的 `\rotation` 函数，这里就用上了：

```
2 \toBase \rotation 2 \fromBase \allValues \minimum
```

进制转换的最常见的基数是 2。可以考虑加上 `\toBase2` 和 `\fromBase2` 函数：

```
\toBase2 \rotation \fromBase2 \allValues \minimum
```

- [ ] `\toBase2`：将一个整数转换成二进制表示的列表。
- [ ] `\fromBase2`：将一个二进制表示的列表转换成整数。

## [A Fine sequence with fine interpretations](https://codegolf.stackexchange.com/q/258110/9288)

参考 Vyxal 或 Jelly

```
\apply2 \increment \dupDip \neg \range0 \dupDip \binomial \increment \reverse \dot \abs \swap \div
```

13 个字节，输得比较惨。如果学 Vyxal 加上 `\mean` 和 `\cumsum` 函数，可以缩减到 8 个字节：

```
\increment \dupDip \neg \range0 \binomial \cumsum \abs \mean
```

- [ ] `\mean`：求一个列表的平均值。不清楚它用得多不多。可能要在 Nekomata 支持有理数之后再加上。
- [x] `\cumsum`：求一个列表的累加和。

## [Painting with Line Filler](https://codegolf.stackexchange.com/q/256978/9288)

```
\while { \orApply \transpose \extract \allEqual \delete } \isEmpty
```

10 个字节，还用了一堆未实现的函数。输的比较惨。

- [ ] `\transpose`：转置一个二维列表。
- [ ] `\extract`：从一个列表中任意取出一个元素，原来的列表中删去这个元素。这个函数是 non-deterministic 的。
- [ ] `\delete`：从一个列表中删除所有等于某个元素的元素。
- [ ] `\isEmpty`：检查一个列表是否为空。

Vyxal、Jelly、05AB1E 都用到了类似于 fixed-point 的函数，但这个在 non-deterministic 语言中不好弄，语义也不是很明确。

## [Find the Prime Signature](https://codegolf.stackexchange.com/q/256147/9288)

```
\factor \tally \sort \reverse
```

- [ ] `\factor`：将一个整数分解成质因数。比如说 `12` 分解成 `[2, 2, 3]`。
- [ ] `\tally`：统计一个列表中每个元素出现的次数。比如说 `[2, 2, 3]` 统计成 `[2, 3] [2, 1]`。

`\factor` 的实现可以用 arithmoi 包的 `factorise` 函数，不过这个函数的返回值是已经 tally 过的。如果 `\factor` 也直接返回已经 tally 过的结果，那么不但实现更方便，还可以减少一个字节。另一个问题是如何向量化。等遇到更多涉及到质因数分解的题目再说。

---

目前已发布 0.1.0.0 版。还有很多常用的 built-in 没有实现。另外，由于 code page 中字符的选取过于随意，在常见的字体中甚至做不到等宽；可能需要重新设计一下 code page。

不想让这篇文档太长，前面已实现的部分就删去了。

## [Shortest Valid Parentheses](https://codegolf.stackexchange.com/questions/258511/shortest-valid-parentheses/258549#258549)

目前的解法有 17 个字节，输得比较惨。主要问题是缺乏字符串操作。比如说，如果有一个字符串替换的 built-in 的话，可以：

```
\subsequence \predicate { "()" \emptyString \stringReplaceRepeated \isEmpty } \length \allValues \maximum
```

- [ ] `\emptyString`：空字符串。
- [ ] `\stringReplaceRepeated`：重复地将一个字符串中的某个子串替换成另一个子串，直到没有子串可以替换为止。这个函数有点复杂，感觉用得不多，以后再说。

## [Shortest code to generate all Pythagorean triples up to a given limit](https://codegolf.stackexchange.com/q/258432/9288)

```
\range1 \subset 3 \lengthIs \predicate { \square \reverse \foldl1 \sub \isZero }
```

- [ ] `\square`：求一个数的平方。
- [ ] `\foldl1`：助词。就是 Haskell 里的 `foldl1`。
- [ ] `\isZero`：检查一个数是否为 0。

```
\range1 \subset 3 \lengthIs \predicate { \square \unsnoc \swap \sum \equal }
```

- [x] `\unsnoc`：把一个列表拆成最后一个元素，和剩下的元素。比如说 `[1, 2, 3]` 拆成 `3` 和 `[1, 2]`。

```
\range1 \subset 2 \lengthIs \dup \squareNorm \sqrt \snoc \swap \lessEq
```

- [ ] `\squareNorm`：求一个向量各项的平方和。
- [ ] `\sqrt`：求一个数的平方根。如果这个数的平方根不是整数，那么返回 `Fail`。这个需要用到 integer-roots 包。

## [The Unaverageables](https://codegolf.stackexchange.com/q/248991/9288)

```
\filter { \add 2 \div \intersection 1 \lengthIs }
```

- [ ] `\filter`：助词。过滤一个列表，只保留函数不返回 `Fail` 的元素。
- [ ] `\intersection`：求两个列表的交集。

## [Is it a super-prime?](https://codegolf.stackexchange.com/q/130390/9288)

```
\isPrime \primePi \isPrime
```

- [ ] `\primePi`：求小于等于某个数的质数的个数。感觉不会很常用，以后再说。

## [Move to Right and left](https://codegolf.stackexchange.com/q/241474/9288)

```
5 \toBase2 \convolve
```

- [ ] `\toBase2`：将一个数转换成二进制表示。
- [ ] `\convolve`：卷积。比如说 `[1, 2, 3]` 和 `[4, 5, 6]` 卷积成 `[4, 13, 28, 27, 18]`。

## [Guessing on straws](https://codegolf.stackexchange.com/q/258992/9288)

参考 05AB1E 的解答：

```
\subset \concat \tally 2 \divExact \countValues 2 \equal
```

只差 `\tally` 还没有实现。

或者：

```
\subset \concat \setPartition 2 \lengthIs \allEqual \countValues 2 \equal
```

- [ ] `\setPartition`：将一个列表拆成若干个子列表，使得每个子列表的元素互不相同。比如说 `[1, 2, 3, 4]` 拆成 `[[1, 2], [3, 4]]` 和 `[[1, 3], [2, 4]]`。

只用现有的函数则需要 13 个字节：

```
\subset \concat \sort \unconcat \map { 2 \lengthIs \allEqual } \countValues 2 \equal
```

## [Reconstruct Matrix from its diagonals](https://codegolf.stackexchange.com/q/252082/9288)

目前已有 17 个字节的解答；但如果换一种思路，再增加一些 built-in，可以做到 10 个字节：

```
\emptyList \cons \split \swap \zipWith \join \transpose \enumerate \zipWith \rotate
```

## [How long to carry sort?](https://codegolf.stackexchange.com/q/259167/9288)

```
\ordering \enumerate \sub \cons0 \maximum
```

- [ ] `\ordering`：求一个列表的排序索引。比如说 `[3, 1, 2]` 的排序索引是 `[2, 3, 1]`。
- [x] `\enumerate`：相当于 `\dup \length \range0`。感觉会比较常用。
