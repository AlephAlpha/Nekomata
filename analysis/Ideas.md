# 一些想法

## CodePage

此处总结一下 Code Page 中已有但还没有用上的字符：

```
¤.W`w
```

`¤` 原本是用于 `\normalForm` 函数。但这个函数由于定义不清晰且在现有的答案中没有用到，因此被移除了。这个字符空了出来，可以用于其它的内置函数。

如果 CodePage 中的 256 个字符都用完了，可以考虑用像 05AB1E 那样，用 `.` 开头的字符来表示双字节的内置函数。现在的 CodePage 还没有用完，先不考虑这个问题，但 `.` 也先不要用掉。

LiberationMono 字体所支持的字符列举于[此文件](analysis/LiberationMonoGlyphs.txt)，以供选取。

## 来自统计结果的想法

写了个[脚本](analyze.py)来统计、分析现有的解答。得到一些有趣的结果（以下排名可能不是最新的）：

* 最常用的一个字符是 `{`，用来开启一个 block。
* 单个字符中，排第二、第三的分别是 `$`（`\swap`）和 `:`（`\dup`），都是常见的栈操作。其它 stack-based 的 golfing 语言中，这两个操作也排名很靠前。
* 四到八名分别是 `=`（`\eq`）、`+`（`\add`）、`∑`（`\sum`）、`ᵐ`（`\map`）、`→`（`\increment`）。这些也都是常见的操作。
* 第九名是 `R`（`\range1`）。这个操作用于生成一个 1 到 n 的列表。另一个类似的操作是 `r`（`\range0`），用于生成一个 0 到 n-1 的列表，相对来说用得较少，排到了六十多名。这是因为很多列表操作会把单个元素自动转换成从 0 开始的列表，可以省去一个 `r`。
* `}` 排到第十二，出现次数不到 `{` 的一半。看来代码结尾可以省略 `}` 的设计是正确的。
* `1` 排到第十四，是最常见的常量。
* `ç`（`\cons0`）能排到第十八。这个完全出乎我的意料，因为很多别的 golfing 语言中根本没有这个操作。它的作用是给列表加一个 0，通常是配合 `\head`、`\last`、`\minimum`、`\maximum` 等函数使用，来处理空列表的特殊情况。需要看一看其它语言是怎么处理空列表的。
* 与 non-deterministic 机制有关的操作，除了 `=`（`\eq`）没有一个排到前二十。排最高的是第二十四的 `a`（`\allValues`）。
* 在两个字符的组合中，排前六的分别是 `ᶦ{`、`ᵖ{`、`ʷ{`、`ˡ{`、`ᵑ{`、`ᶠ{`。也许可以像 Vyxal 等语言一样，让这些助词自动开启 block，省去一个 `{`。不过另外一些助词通常只修饰单个 built-in，自动开启 block 反而会额外需要一个 `}`。尤其是 `ᵖ`、`ᵑ`、`ᵐ`，两种用法都很常见，不好取舍。详见 [`analysis/particles.txt`](particles.txt)，以及下文的[关于助词](#关于助词)。
* `{$` 在两个字符的组合中排第七，`{:` 排第八。这两个组合没有特别的意义，只是因为 `\dup` 和 `\swap` 是最常用的两个操作，因此它们的组合也很常用。
* `Jᵐ` 在两个字符的组合中排第八，和 `{:`、`ᵐ{`、`Ťđ` 并列。这完全出乎我的意料。`J` 是将输入 non-deterministic 地拆成若干部分，`ᵐ` 是对后一个函数进行 map，这两个操作的组合可以看作 Haskell 中 `concatMap` 的反操作。可能 `concatMap` 也是一个常用的操作，但由于 `ᵐ` 和 `j`（`\concat`）之间会插入别的操作，所以没有出现在这个统计结果中。可以考虑加上 `\concatMap` 和 `\unconcatMap` 两个助词。目前已加上。
* `Ťđ` 在两个字符的组合中也排第八，和 `{:`、`Jᵐ`、`ᵐ{` 并列。这个组合是将二维列表转置，然后进行 unpair 操作。看起来确实比较常用。先等更多的解答出现再决定是否增加这个 built-in。
* `Ňᵖ{` 在三个字符的组合中排第一，也许值得为它增加一个助词。
* 统计结果与其它语言的差异，除了考虑到语言本身的特点之外，还要考虑到语言的使用者的偏好。目前 Nekomata 的使用者基本上只有我自己，解答的也主要是我感兴趣的题目类型，string 相关的题目较少，ascii-art 更是完全没有。

随着已有解答的增多，增加新的内置函数时，很难再把已有的解答都看一遍，看看是否能用新的内置函数来改进。因此，这一统计结果不再代表最优的方案，但仍有一定的参考价值。

## 来自 codegolf 题目的想法

先在 [Code Golf Stack Exchange](https://codegolf.stackexchange.com/) 上随便找一些题目，看看要用 Nekomata 解答的话会是什么样子，需要加上哪些内置函数，或者别的什么功能。

以下函数都写全称，不用缩写；但计算长度时还是用缩写，假设每个内建函数和助词都只占一个字节。

### [Group elements by their displacement](https://codegolf.stackexchange.com/q/239999/9288)

参考现有的解答的话，大概是这样：

```
\enumerate \absDiff \groupBy
```

或者

```
\sort \absDiff \groupBy
```

- [ ] `\groupBy`：输入两个列表，其长度必须一致。根据第一个列表的元素，将第二个列表的元素分组。比如输入 `[1, 2, 1, 2]` 和 `[a, b, c, d]`，输出 `[[a, c], [b, d]]`。

其它语言里的 `\groupBy` 一般都是高阶函数，输入的是一个函数和一个列表，而非两个列表。我觉得输入两个列表更方便一些，能更好地利用自动向量化。

如果用高阶函数的话，`\groupBy` 就是一个助词，而不是一个普通的函数。大概看起来是这样：

```
\groupBy { \index \absDiff }
```

即使省略右括号，这个解答也比前面的要多一个字节。而且，如果助词修饰的函数是 non-deterministic 的，不知道该怎么处理。

### [Consolidate an Array](https://codegolf.stackexchange.com/q/70779/9288)

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

用 `\ordering` 的话会多一个字节：

```
\logicalNot \ordering \nth
```

### [Gödel encoding - Part I](https://codegolf.stackexchange.com/q/259960/9288)

目前完全无法解答，除非支持递归。

即使有递归，也还要增加一些新的函数：

```
\dup \enumerate \increment \nthPrime \dip \map \self \pow \product \swap \powOf2 \if
```

- [ ] `\nthPrime`：求第 n 个质数。
- [ ] `\self`：用于递归，表示当前函数。

支持递归需要对现有的解释器进行大量的修改。一个问题是它会让 arity 变得不确定。可以考虑为不同 arity 的函数分别定义 `\self`，比如说 `\self1`、`\self2`、`\self3` 等等。

### [Find Unique Anagrams](https://codegolf.stackexchange.com/q/261128/9288)

```
\map \sort \nubBy
```

- [ ] `\nubBy`：输入两个列表，其长度必须一致。根据第二个列表的元素，将第一个列表的元素去重。比如输入 `[a, b, c, d]` 和 `[1, 2, 1, 2]`，输出 `[a, b]`

### [Remove duplicates from my academic transcript](https://codegolf.stackexchange.com/q/256441/9288)

```
\map \first \groupBy \anyOf \dup \last \last \minimumBy
```

### [XOR of independent Bernoulli variables](https://codegolf.stackexchange.com/q/262140/9288)

```
\mul2 \complement \product \complement \div2
```

- [ ] `\complement`：1 减去一个数。

### [How many trailing zeros in the hyperfactorial?](https://codegolf.stackexchange.com/q/263597/9288)

```
\range1 \dup 5 \valuation \dot
```

- [ ] `\valuation`：一个数的 p 进赋值。

### [Number of bits needed to represent the product of the first primes](https://codegolf.stackexchange.com/q/263531/9288)

```
\range1 \nthPrime \product \binary \length
```

- [ ] `\nthPrime`：求第 n 个质数。

### [Hypercube elements](https://codegolf.stackexchange.com/q/70558/9288)

```
2 \range1 \swap \polPow
```

- [ ] `\polPow`：将一个列表视为多项式的系数，求多项式的 n 次幂。感觉不会太常用。

### [Remove falsy rows and columns](https://codegolf.stackexchange.com/q/269954/9288)

```
\isNonzero \dup \transpose \removeFail \transpose \swap \filterBy
```

- [ ] `\filterBy`：输入两个列表，其长度必须一致。根据第二个列表的元素是否为 Fail，将第一个列表的元素过滤。比如输入 `[1,2,3,4]` 和 `[4,Fail,2,Fail]`，输出 `[1,3]`。

## 关于字符串

从 v0.5.0.0 开始，Nekomata 将不再区分字符串和列表，增加了一个字符类型，当一个列表的全部元素都是字符时，就视为一个字符串。字符串和列表只在输入输出时有区别，其它时候都是一样的。

此外，一些常用的数学函数会自动把字符串按 Nekomata 的 code page 转换成数字。

但这带来了以下的新问题：

- [ ] 一些函数会自动把输入的数字或字符串转换成列表。数字会转换成 range。但如果增加字符类型的话，单个字符不知道该转换成什么。暂时先定为 Fail，以后可能会改成单个字符的列表，或者按 code page 转换成数字再 range。
- [ ] 空集和空字符串无法区分。应该输出 `[]` 还是 `""`？暂时先输出 `[]`。

等以后的版本再考虑这些问题。

## 关于助词

根据目前的统计结果，两个字符的组合中排前几名的都是助词 + `{` 的形式。也许可以像 Vyxal 等语言一样，让这些助词自动开启 block，省去一个 `{`。不过另外一些助词通常只修饰单个 built-in，自动开启 block 反而会额外需要一个 `}`。尤其是 `ᵖ`、`ᵑ`、`ᵏ`，两种用法都很常见，不好取舍。详见 [`analysis/particles.txt`](particles.txt)。

自动开启 block 是很大的 breaking change，会影响到大量的解答。最好能有一个改动没有这么大的方案。

可以考虑如果一个助词出现在程序的结尾，其修饰的就是除它之外的整个程序。比如说 `ᶦ{Ƃ2ŗɔƃ3M` 可以写成　`Ƃ2ŗɔƃ3Mᶦ`。这样解决不了所有的问题，但可以解决一些问题。不过这会降低可读性（虽然 golfing language 谈可读性本就有点搞笑），而且 parser 也不好写。

另一个问题是很多助词的 arity 限制太严格了，比如说 `\map` 只能用于 arity 为 `m -> 1` 的函数。这限制了函数的使用方式。下面的关于 arity 的部分作了一些讨论。不过，也许有另一种改动没那么大的方案。

现有的助词大概可以分成以下几类：

- 像 `\predicate`、`\predicateNot`、`\filter` 这类，把修饰的函数当作谓词，只看它的返回值是否有值，不看具体的值是什么。有值的视为真，无值的视为假。如果有多个返回值，只看最后一个。它们对 arity 没有要求，因此不需要改动。
- 像 `\map`、`\outer`、`\zipWith` 这类，把修饰的函数作用在列表的每个元素上，返回一个新的列表。它们的 arity 限制太严格了，只能用于 arity 为 `m -> 1` 的函数；而且 `m` 还有一个下界，比如说 `\map` 的下界是 1，`\outer` 的下界是 2。
  - 可以考虑把这些限制放宽到 `m -> n`，`n>=1`。如果 `n>1`，可以返回多个列表。比如说 `[1,2,3] \map \dup` 可以返回 `[1,2,3] [1,2,3]`。
  - 如果 `m` 小于原本的下界，可以把 `m -> n` 的函数视为 `m + 1 -> n + 1`，或者 `m + 2 -> n + 2`。比如说常值函数 `1` 的 arity 是 `0 -> 1`，但 `\map` 要求 `m > 0`。此时可以把 `1` 视为 `1 -> 2`，然后 `[1,2,3] \map 1` 可以返回 `[1,2,3] [1,1,1]`。
- 像 `\iterate`、`\while`、`\nTimes` 这类，把修饰的函数重复作用在一个值上，直到满足某个条件。它们只能用于 arity 为 `n -> n` 的函数，也就是说输入和输出的个数必须相同。这个限制不太好改。一个可能的思路是，如果输入的函数的 arity 是 `m -> n`，`m != n`，则想办法把它转换成 `m -> m` 或 `n -> n` 的函数。具体怎么转换还不好说，但不管怎么转换，都会严重影响可读性。也许还是不要改了。
- 其它不好归类的助词，具体情况具体分析。

## 关于 arity

当前，Nekomata 中的助词会检查它修饰的函数的 arity，如果不匹配就报错。这严格限制了函数的使用方式，比如说 `\map` 只能用于 arity 为 `n -> 1` 的函数。而且 arity 本身也有一些不太清晰的地方。

目前，一个函数的 arity 由两部分组成：输入的个数和输出的个数。比如说 `\add` 的 arity 是 `2 -> 1`。但作为 stack-based 的语言，一个 arity 为 `m -> n` 的函数其实也可以看作是 arity 为 `m + e -> n + e` 的函数，其中 e 为任意非负整数。但当前的实现中，arity 是固定的，因此只能在不同的  `m + e -> n + e` 中指定一个。这带来一些问题：比如说 `dup`，复制栈顶元素，它的 arity 是 `1 -> 2`。但栈顶原本的元素其实没有变化，因此它的 arity 也可以看作是 `0 -> 1`。到底该选 `1 -> 2` 还是 `0 -> 1`？

也许可以考虑把 arity 改成三个数，`(m,n,p)`，其中 `m>=n`。计算时，读取栈顶的 `m` 个元素，但只弹出 `n` 个元素，剩下的 `m-n` 个元素不变；计算完毕后，再把 `p` 个元素压入栈顶。比如说 `\add` 的 arity 是 `(2,2,1)`，而 `dup` 的 arity 是 `(1,0,1)`。

然后考虑两个函数的复合。设两个函数 `f` 和 `g` 的 arity 分别是 `(m1,n1,p1)` 和 `(m2,n2,p2)`。先计算 `f`，再计算 `g`。复合后的函数的 arity 是 `(max(m1,m2+n1-p1),max(n1,n1+n2-p1),max(p1+p2-n2,p2))`。

由此可以计算出，如果是多个函数的复合，比如说 `k` 个函数的 arity 分别是 `(m_1,n_1,p_1)`、`(m_2,n_2,p_2)`、……、`(m_k,n_k,p_k)`，那么复合后的函数的 arity 是 `(max(m_1,m_2+n_1-p_1,m_3+n_1+n_2-p_1-p_2,...,m_k+n_1+...+n_{k-1}-p_1-…-p_{k-1}),max(n_1,n_1+n_2-p_1,n_1+n_3+n_2-p_1-p_2,...,n_1+...+n_k-p_1-…-p_{k-1}),max(p_1+...+p_k-n_2-...-n_k,p_2+...+p_k-n_3-...-n_k,...,p_k))`。

然后助词 `\map` 的 arity check 就可以改成：

- 如果输入函数的 arity 是 `(m,n,1)`，`m > 0`，则输出函数的 arity 是 `(m,n,1)`。
- 如果输入函数的 arity 是 `(0,0,1)`，这是常值函数，此时 `\map` 被重载为将一个列表的每个元素都映射到这个常值，输出函数的 arity 是 `(1,1,1)`。
- 以后可以考虑增加对 `(m,n,p)`，`p>1` 的支持。此时，如果 `m > 0`，则输出函数的 arity 是 `(m,n,p)`；如果 `m = n = 0`，则输出函数的 arity 是 `(1,1,p)`。

更多情况还需进一步考虑。这是比较大的改动，需要仔细斟酌。也许还有其它更好的方案。以后再说。

## 文档

目前的文档还很不完善。需要增加更多的例子，以及更详细的说明。

- [ ] 为每个函数和助词增加例子，并把这些例子用作单元测试。目前函数的部分已完成，助词的部分还没有。
- [ ] 重写现有的 Tutorial。考虑分成多个文件，每个文件只讲一个主题。
- [ ] REPL 的 `\Info` 命令，可以考虑改成能一次输出多个函数的信息。
