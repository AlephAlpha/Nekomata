# 一些想法

Nekomata 现在已经有了一个非常简单的解释器。不过已有的内置函数非常少，写不出什么有用的程序。准备等内置函数丰富一些之后再写文档。

先在 [Code Golf Stack Exchange](https://codegolf.stackexchange.com/) 上随便找一些题目，看看要用 Nekomata 解答的话会是什么样子，需要加上哪些内置函数，或者别的什么功能。

尤其要参考 [Brachylog](https://github.com/JCumin/Brachylog)、[Vyxal](https://github.com/Vyxal/Vyxal)、[05AB1E](https://github.com/Adriandmen/05AB1E)、[Jelly](https://github.com/DennisMitchell/jellylanguage)、[Husk](https://github.com/barbuz/Husk) 等语言的解答，看看有什么可以借鉴的地方。

以下函数都写全称，不用缩写；但计算长度时还是用缩写，假设每个内建函数和助词都只占一个字节。

此处总结一下 Code Page 中已有但还没有用上的字符：

```
≈&'.VWXY`vwy|
```

有些是已经确定分配给什么函数的，比如说 `|` 给 `\bitOr`，`&` 给 `\bitAnd`，`X` 给 `\bitXor`。别的都还没想好。

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
\enumerate \absDiff \groupBy
```

或者

```
\sort \absDiff \groupBy
```

- [ ] `\absDiff`：求两个数的绝对值差；自动向量化。可以考虑拆成 `\sub` 和 `\abs` 两个函数。
- [ ] `\groupBy`：输入两个列表，其长度必须一致。根据第一个列表的元素，将第二个列表的元素分组。比如输入 `[1, 2, 1, 2]` 和 `[a, b, c, d]`，输出 `[[a, c], [b, d]]`。

其它语言里的 `\groupBy` 一般都是高阶函数，输入的是一个函数和一个列表，而非两个列表。我觉得输入两个列表更方便一些，能更好地利用自动向量化。

如果用高阶函数的话，`\groupBy` 就是一个助词，而不是一个普通的函数。大概看起来是这样：

```
\groupBy { \index \absDiff }
```

- [ ] `\index`：输入一个列表和一个元素，输出这个元素在列表中的索引。这个函数肯定是比较常用的，不过不知道要不要弄成 non-deterministic 的。

即使省略右括号，这个解答也比前面的要多一个字节。而且，如果助词修饰的函数是 non-deterministic 的，不知道该怎么处理。

## [Smallest groups in an array](https://codegolf.stackexchange.com/q/79037/9288)

```
\chunks \dup \map \length \minimumBy
```

- [ ] `\minimumBy`：输入两个列表，其长度必须一致。根据第一个列表中最小的元素的索引，返回第二个列表中对应的元素。由于最小的元素可能不止一个，所以这个函数是 non-deterministic 的。

和前面的 `\groupBy` 一样，`\minimumBy` 也是一个普通的函数而不是助词。但如果改成助词的话，解答会更短一些：

```
\chunks \minimumBy \length
```

看来是用普通函数还是助词还需要斟酌。可能两种都要支持，不过名字怎样区分是个问题。

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

## [Mode (most common element) of a list](https://codegolf.stackexchange.com/q/42529/9288)

```
\tally \maximumBy
```

需要 `-1` flag 来仅输出第一个结果。

- [ ] `\maximumBy`：输入两个列表，其长度必须一致。根据第一个列表中最大的元素的索引，返回第二个列表中对应的元素。由于最大的元素可能不止一个，所以这个函数是 non-deterministic 的。

## [Shortest Valid Parentheses](https://codegolf.stackexchange.com/q/258511/9288)

目前的解法有 17 个字节，输得比较惨。主要问题是缺乏字符串操作。比如说，如果有一个字符串替换的 built-in 的话，可以：

```
\subsequence \predicate { "()" \emptyString \stringReplaceRepeated \isEmpty } \length \allValues \maximum
```

- [ ] `\emptyString`：空字符串。
- [ ] `\stringReplaceRepeated`：重复地将一个字符串中的某个子串替换成另一个子串，直到没有子串可以替换为止。这个函数有点复杂，感觉用得不多，以后再说。

## [How long to carry sort?](https://codegolf.stackexchange.com/q/259167/9288)

```
\ordering \enumerate \sub \cons0 \maximum
```

- [ ] `\ordering`：求一个列表的排序索引。比如说 `[3, 1, 2]` 的排序索引是 `[1, 2, 0]`。

## [Make a Custom Bayer Matrix](https://codegolf.stackexchange.com/q/259633/9288)

```
\range0 \toBase2Rev \dup \outer { \dip \dup \bitxor 2 \mul \add \cons0 4 \recip \fromBaseRev }
```

- [ ] `\bitxor`：按位异或。

或者：

```
\range0 \toBase2Rev \dup \outer { \dip \dup \bitxor \interleave \cons0 2 \recip \fromBaseRev }
```

- [ ] `\interleave`：将两个列表交错合并。比如说 `[1, 2, 3]` 和 `[4, 5, 6]` 交错合并成 `[1, 4, 2, 5, 3, 6]`。第二个列表可以和第一个列表一样长，也可以比第一个列表少一个元素。

## [Hunt for discount](https://codegolf.stackexchange.com/q/233641/9288)

```
\sort \reverse \deinterleave \sum \half
```

- [ ] `\deinterleave`：将一个列表拆成两个列表，一个列表包含所有奇数位置的元素，另一个列表包含所有偶数位置的元素。比如说 `[1, 2, 3, 4]` 拆成 `[1, 3]` 和 `[2, 4]`。目前还没遇到其它会用到这个函数的场景。

## [Sum every second digit in a number](https://codegolf.stackexchange.com/a/255665/9288)

```
\ten \toBase \deinterleave \sum
```

刚说完 `\deinterleave`，这里又用到了。

## [How Super is this Prime?](https://codegolf.stackexchange.com/q/259875/9288)

```
\iterate { \primePi \isPrime } \countValues 2 \fromBase \length \decrement
```

也许可以考虑加一个助词：

```
\lengthWhile { \primePi \isPrime } 2 \fromBase \length \decrement
```

- [ ] `\lengthWhile`：助词。重复执行一个函数，直到这个函数返回 `Fail`，返回这个函数执行的次数。

## [Is it a completely even number?](https://codegolf.stackexchange.com/q/142534/9288)

```
\decrement \bitxor \less
```

## [Gödel encoding - Part I](https://codegolf.stackexchange.com/q/259960/9288)

目前完全无法解答，除非支持递归。

即使有递归，也还要增加一些新的函数：

```
\dup \enumerate \increment \nthPrime \dip \map \self \pow \product \swap 2 \pow \if
```

- [ ] `\nthPrime`：求第 n 个质数。
- [ ] `\self`：用于递归，表示当前函数。

支持递归需要对现有的解释器进行大量的修改。一个问题是它会让 arity 变得不确定。可以考虑为不同 arity 的函数分别定义 `\self`，比如说 `\self1`、`\self2`、`\self3` 等等。

## [Pseudofactorial](https://codegolf.stackexchange.com/q/82494/9288)

```
\range1 \foldl1 \lcm
```

- [ ] `\foldl1`：从左到右地将一个二元函数应用于一个列表中的所有元素。比如说 `[1, 2, 3] \foldl1 \add` 等价于 `1 2 \add 3 \add`。

## [String Comparison](https://codegolf.stackexchange.com/q/259987/9288)

```
\swap \apply2Pair \join \less
```

- [ ] `\apply2Pair`：助词。输入四个参数，将一个二元函数分别应用于前两个参数和后两个参数。

## [Generate a Walsh Matrix](https://codegolf.stackexchange.com/q/162254/9288)

```
2 \pow \dup \outer \bitand \popcount \neg1 \pow
```

- [ ] `\bitand`：按位与。
- [ ] `\popcount`：求一个数的二进制表示中 1 的个数。

位运算还是很有用的。

## [Shortest distinguishable slice](https://codegolf.stackexchange.com/q/259707/9288)

```
\charToInt \pad \transpose \enumerate \dup \apply2 \anyOf \increment \predicate { \range0 \add \nth \transpose \allUnique } \cons0 \swap \add
```

- [ ] `\pad`：输入一个列表的列表，将每个列表的长度补齐到最长的子列表的长度。比如说 `[[1, 2], [3, 4, 5]]` 补齐成 `[[1, 2, 0], [3, 4, 5]]`。
- [ ] `\allUnique`：判断一个列表中的元素是否都不相同。

如果 `\subsequence` 的输出是按长度从小到大排序的，那么可以简化为（16 字节）：

```
\charToInt \pad \transpose \enumerate \subsequence \predicate { \nth \transpose \allUnique } \dupDip \first \last \increment \pair
```

## [Find Index of Rational Number in Calkin-Wilf Sequence](https://codegolf.stackexchange.com/q/260472/9288)

需要 `-n` flag。


```
\iterate { \recip \decrement \neg1 \divMod \add \neg \nonZero }
```

- [ ] `\divMod`：同时求商和余数。由于有两个返回值，还不知道怎么向量化。

## [Is it a brainfuck instruction?](https://codegolf.stackexchange.com/q/203330/9288)

```
"+,-.<>[]" \elem
```

- [ ] `\elem`：检查一个元素是否在一个列表中。注意其参数顺序与 `\free` 相反。

## [Find Unique Anagrams](https://codegolf.stackexchange.com/q/261128/9288)

```
\map \sort \groupBy \map \first
```

## [Implement Takewhile](https://codegolf.stackexchange.com/q/84519/9288)

```
2 \mod \cumsum \minimumBy
```

## [Make a Court Transcriber](https://codegolf.stackexchange.com/q/252927/9288)

```
\filter { \unconcat \subset \equal } \dup \map \length \minimumBy
```

## [2048-like array shift](https://codegolf.stackexchange.com/q/95409/9288)

```
\chunks \map { \deinterleave \add \reverse } \concat
```

## [The shortest way to find one unique value when all other values are the same](https://codegolf.stackexchange.com/q/207736/9288)

```
\tally \minimumBy
```

## [Most Common Multiple](https://codegolf.stackexchange.com/q/170047/9288)

```
\subset \unpair \times \allValues \tally \maximumBy
```

## [Least Common Multiple](https://codegolf.stackexchange.com/q/94999/9288)

```
\foldl1 \lcm
```
