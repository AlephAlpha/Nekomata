# 一些想法

## CodePage

此处总结一下 Code Page 中已有但还没有用上的字符：

```
.W`w
```

如果 CodePage 中的 256 个字符都用完了，可以考虑用像 05AB1E 那样，用 `.` 开头的字符来表示双字节的内置函数。现在的 CodePage 还远远没有用完，先不考虑这个问题，但 `.` 也先不要用掉。

LiberationMono 字体所支持的字符列举于[此文件](analysis/LiberationMonoGlyphs.txt)，以供选取。

## 来自统计结果的想法

写了个[脚本](analyze.py)来统计、分析现有的解答。得到一些有趣的结果（以下排名可能不是最新的）：

* 最常用的一个字符是 `{`，用来开启一个 block。
* 单个字符中，排第二、第三的分别是 `:`（`\dup`）和 `$`（`\swap`），都是常见的栈操作。其它 stack-based 的 golfing 语言中，这两个操作也排名很靠前。
* 四到七名分别是 `=`（`\eq`）、`+`（`\add`）、`∑`（`\sum`）、`←`（`\decrement`）。这些也都是常见的操作。
* 第八名是 `ᵐ`（`\map`），是最常用的助词。
* `1` 排到第十，是最常见的常量。
* `}` 排到第十二，出现次数不到 `{` 的一半。看来代码结尾可以省略 `}` 的设计是正确的。
* `a`（`\allValues`）排第十五。这是 non-deterministic 语言特有的操作，算是 Nekomata 的特色之一。
* `ç`（`\cons0`）能排到第十六。这个完全出乎我的意料，因为很多别的 golfing 语言中根本没有这个操作。它的作用是给列表加一个 0，通常是配合 `\head`、`\last`、`\minimum`、`\maximum` 等函数使用，来处理空列表的特殊情况。需要看一看其它语言是怎么处理空列表的。
* 在两个字符的组合中，排前五的分别是 `ᶦ{`、`ᵖ{`、`ʷ{`、`ᵑ{`、`ᶠ{`，`ˡ{` 排第七。也许可以像 Vyxal 等语言一样，让这些助词自动开启 block，省去一个 `{`。不过另外一些助词通常只修饰单个 built-in，自动开启 block 反而会额外需要一个 `}`。尤其是 `ᵖ`、`ᵑ`、`ᵐ`，两种用法都很常见，不好取舍。详见 [`analysis/particles.txt`](particles.txt)。
* `{$` 在两个字符的组合中排第六，`{:` 排第九。这两个组合没有特别的意义，只是因为 `\dup` 和 `\swap` 是最常用的两个操作，因此它们的组合也很常用。
* `1>` 在两个字符的组合中排第八。可以考虑增加一个 `\significant` 函数，用来检查一个数的绝对值是否大于 1。Jelly 中有与之相反的函数 `insignificant`。到底是添加 `\significant` 还是 `\insignificant`，还要考虑一下。可能两个都要加。名字也可以再改改，比如说叫 `\nontrivial` 和 `\trivial`，甚至叫 `\isBig` 和 `\isSmall`。
* `Ňᵖ{` 在三个字符的组合中排第一，也许值得为它增加一个助词。
* 统计结果与其它语言的差异，除了考虑到语言本身的特点之外，还要考虑到语言的使用者的偏好。目前 Nekomata 的使用者基本上只有我自己，解答的也主要是我感兴趣的题目类型，string 相关的题目较少，ascii-art 更是完全没有。

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

- [ ] `\ordering`：求一个列表的排序索引。比如说 `[3, 1, 2]` 的排序索引是 `[1, 2, 0]`。

### [How long to carry sort?](https://codegolf.stackexchange.com/q/259167/9288)

```
\ordering \enumerate \sub \cons0 \maximum
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

位运算还是很有用的。

### [Shortest distinguishable slice](https://codegolf.stackexchange.com/q/259707/9288)

```
\charToInt \pad \enumerate \subsequence \nonempty \predicate { \mapWith \nth \transpose } \allValues \shortest \dupDip \first \last \increment \pair
```

- [ ] `\pad`：输入一个列表的列表，将每个列表的长度补齐到最长的子列表的长度。比如说 `[[1, 2], [3, 4, 5]]` 补齐成 `[[1, 2, 0], [3, 4, 5]]`。

### [Is it a brainfuck instruction?](https://codegolf.stackexchange.com/q/203330/9288)

```
"+,-.<>[]" \elem
```

- [ ] `\elem`：检查一个元素是否在一个列表中。注意其参数顺序与 `\free` 相反。

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
2 \range1 \swap \polyPow
```

- [ ] `\polyPow`：将一个列表视为多项式的系数，求多项式的 n 次幂。感觉不会太常用。

## 关于字符串

从 v0.5.0.0 开始，Nekomata 将不再区分字符串和列表，增加了一个字符类型，当一个列表的全部元素都是字符时，就视为一个字符串。字符串和列表只在输入输出时有区别，其它时候都是一样的。

此外，一些常用的数学函数会自动把字符串按 Nekomata 的 code page 转换成数字。

但这带来了以下的新问题：

- [ ] 一些函数会自动把输入的数字或字符串转换成列表。数字会转换成 range。但如果增加字符类型的话，单个字符不知道该转换成什么。暂时先定为 Fail，以后可能会改成单个字符的列表，或者按 code page 转换成数字再 range。
- [ ] 空集和空字符串无法区分。应该输出 `[]` 还是 `""`？暂时先输出 `[]`。

等以后的版本再考虑这些问题。

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

更多情况还需进一步考虑。这是比较大的改动，需要仔细斟酌。留到以后的版本再说。
