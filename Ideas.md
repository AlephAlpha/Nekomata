# 一些想法

Nekomata 现在已经有了一个非常简单的解释器。不过已有的内置函数非常少，写不出什么有用的程序。准备等内置函数丰富一些之后再写文档和 REPL。

先在 [Code Golf Stack Exchange](https://codegolf.stackexchange.com/) 上随便找一些题目，看看要用 Nekomata 解答的话会是什么样子，需要加上哪些内置函数，或者别的什么功能。

尤其要参考 [Brachylog](https://github.com/JCumin/Brachylog)、[Vyxal](https://github.com/Vyxal/Vyxal)、[05AB1E](https://github.com/Adriandmen/05AB1E)、[Jelly](https://github.com/DennisMitchell/jellylanguage)、[Husk](https://github.com/barbuz/Husk) 等语言的解答，看看有什么可以借鉴的地方。

以下函数都写全称，不用缩写；但计算长度时还是用缩写，假设每个内建函数和助词都只占一个字节。

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
- [ ] `\sort`：把一个列表按照元素的大小排序。应该是一个比较常用的函数，但在 non-deterministic 语言可能不太好实现。
- [ ] `\absDiff`：求两个数的绝对值差；自动向量化。同样，也可以考虑拆成 `\sub` 和 `\abs` 两个函数。
- [ ] `\groupBy`：输入两个列表，其长度必须一致。根据第一个列表的元素，将第二个列表的元素分组。比如输入 `[1, 2, 1, 2]` 和 `[a, b, c, d]`，输出 `[[a, c], [b, d]]`。

其它语言里的 `\groupBy` 一般都是高阶函数，输入的是一个函数和一个列表，而非两个列表。我觉得输入两个列表更方便一些，能更好地利用自动向量化。

如果用高阶函数的话，`\groupBy` 就是一个助词，而不是一个普通的函数。大概看起来是这样：

```
\groupBy { \index \absDiff }
```

- [ ] `\index`：输入一个列表和一个元素，输出这个元素在列表中的索引。这个函数肯定是比较常用的，不过不知道要不要弄成 non-deterministic 的。

即使可以省略右括号（目前未实现），这个解答也比前面的要多一个字节。

## [Infinite Apple Dilemma](https://codegolf.stackexchange.com/q/257649/9288)

这道题告诉我们，Nekomata 一定要支持有理数。以下解答照抄现有的 Vyxal 解答：

```
\reciprocal \complement \product \div
```

- [ ] `\reciprocal`：求一个数的倒数。不支持有理数的话，这个函数就毫无意义。
- [ ] `\complement`：求 `1 - x`。感觉不一定常用，可以考虑拆成 `\decrement` 和 `\neg` 两个函数。`\neg` 已经有了。
- [ ] `\product`：求一个列表的乘积。这个函数应该是比较常用的。
- [ ] `\div`：求两个数的商。目前已经有了这个函数，但是只支持整数，不支持有理数。加上有理数之后，可能需要三种版本的 `\div`：`\div`、`\divInt` 和 `\divExact`。`\div` 就是普通的除法；`\divInt` 是先除再取整；`\divExact` 是整除，如果结果不是整数就返回 `Fail`。

或者抄现有的 05AB1E 解答：

```
\dup \decrement \div \product \mul
```

- [x] `\dup`：复制栈顶元素。已实现。
- [ ] `\decrement`：将一个数减 1。
- [ ] `\mul`：乘法。已实现，待添加有理数支持。

如果不支持有理数，这道题也不是不能解答，只是解答会比较长：

```
\dupDip { \product \mul } \decrement \product \div
```

- [ ] `\dupDip`：这是个助词，不知道叫什么名字好。其作用是调用完函数之后把原来的栈顶元素再压回去。其它语言里没见过这个东西，但好像挺有用的。

## [Time to shortest transposition](https://codegolf.stackexchange.com/q/257631/9288)

这道题比较复杂，Vyxal 和 05AB1E 的解答都超过了 20 个字节。

先试试：

```
\permutation \apply2 { 2 \chunks "\24\60" \bytes \zipWith \less 60 \fromBase } \sub \neg \allValues \minimum
```

凭空写的，大部分函数都没实现，不知道对不对。如果我没数错的话，这个解答有 20 个字节。

这里的 `"\24\60"` 表示字符串里两个字符的编码分别是 24 和 60。Nekomata 实际上尚未支持这种写法，而且其字符编码还没有对应 `24` 的字符。

- [ ] `\permutation`：求一个列表的任意一个排列。这个函数是 non-deterministic 的。
- [x] `\apply2`：助词，将一个一元函数应用到栈顶的两个元素上。已实现。
- [ ] `\chunks`：输入一个列表和一个整数，将列表分成若干个长度为该整数的子列表。
- [ ] `\bytes`：输入一个字符串，输出一个列表，列表的每个元素是字符串中的一个字符的编码。
- [ ] `\zipWith`：助词，将一个二元函数应用到两个列表的对应元素上。
- [ ] `\less`：比较两个数的大小。如果第一个数小于第二个数，返回第一个数；否则返回 `Fail` 。
- [ ] `\fromBase`：输入一个列表和一个整数，将列表中的元素看作是某个进制的数的各位，输出这个数的十进制表示。比如输入 `[1, 2, 3]` 和 `10`，输出 `123`。
- [x] `\sub`：求两个数的差。已实现。
- [x] `\neg`：求一个数的相反数。已实现。不清楚要不要加一个合并 `\neg` 和 `\sub` 的函数。
- [ ] `\allValues`：求一个 non-deterministic 值的所有可能取值，返回一个列表。为了实现这个函数，需要对现有的运算机制做一些修改。
- [ ] `\minimum`：求一个列表的最小值。

## [Smallest groups in an array](https://codegolf.stackexchange.com/q/79037/9288)

```
\splitIntoRuns \dup \map \length \minimumBy
```

- [ ] `\splitIntoRuns`：将一个列表从相邻元素不相等的地方拆开，得到一个列表的列表。比如输入 `[1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 6]`，输出 `[[1], [2, 2], [3, 3, 3], [4], [5, 5, 5, 5], [6]]`。
- [ ] `\map`：助词，将一个一元函数应用到一个列表的每个元素上。
- [ ] `\length`：求一个列表的长度。
- [ ] `\minimumBy`：输入两个列表，其长度必须一致。根据第一个列表中最小的元素的索引，返回第二个列表中对应的元素。由于最小的元素可能不止一个，所以这个函数是 non-deterministic 的。

和前面的 `\groupBy` 一样，`\minimumBy` 也是一个普通的函数而不是助词。但如果改成助词的话，解答会更短一些：

```
\splitIntoRuns \minimumBy \length
```

看来是用普通函数还是助词还需要斟酌。可能两种都要支持，不过名字怎样区分是个问题。

## [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288)

想到一种比较特别的解法：

```
\suffix \uncons \elem
```

加上 `-c` flag，表示输出可能的结果的个数。

- [ ] `\suffix`：求一个列表的一个后缀。这个函数是 non-deterministic 的。这个函数不一定常用，不过能很好地体现 Nekomata 的特色，所以还是实现一下吧。
- [ ] `\uncons`：输入一个列表，输出一个二元组，第一个元素是列表的第一个元素，第二个元素是列表的剩余部分。如果列表为空，返回 `Fail`。
- [ ] `\elem`：判断一个元素是否在一个列表中。

## [Running second maximum of a list](https://codegolf.stackexchange.com/q/138510/9288)

参考了现有的 Husk 解答：

```
\prefix \sort \reverse 2 \nth
```

- [ ] `\prefix`：求一个列表的一个前缀。这个函数是 non-deterministic 的。
- [ ] `\reverse`：倒转一个列表。
- [ ] `\nth`：输入一个列表和一个整数，返回列表中第 n 个元素。如果列表长度小于 n，返回 `Fail`。

不清楚要不要加上一个 `\nth-from-end` 函数，这样就可以写成 `\prefix \sort 2 \nth-from-end`。

## [The inverse Collatz Conjecture](https://codegolf.stackexchange.com/q/175248/9288)

有点麻烦，因为这需要一个 while 循环，循环里还有 if 判断。

```
\repeatNonDet { \dup \decrement \2 \divExact \swap 3 \mul \increment \choice \firstValue \nonZero }
```

有 13 个字节了，和其它语言的解答相比还是有点长的。

- [ ] `\repeatNonDet`：助词。输入一个函数，重复执行这个函数 non-deterministic 次。这个函数是 non-deterministic 的。这个助词比较复杂，不确定要不要实现。
- [ ] `\divExact`：输入两个整数，求它们的整除。如果不能整除，返回 `Fail`。
- [x] `\swap`：交换栈顶的两个元素。已实现。
- [ ] `\increment`：将一个数加 1。
- [ ] `\choice`：在栈顶的两个元素中选择一个。这个函数是 non-deterministic 的。
- [ ] `\firstValue`：求一个 non-deterministic 值的第一个可能取值。这个函数和 `\allValues` 一样，需要对现有的运算机制做一些修改。不清楚要不要把 `\choice` 和 `\firstValue` 合并成一个函数，这样可能可以更方便地模拟 if 判断。
- [ ] `\nonZero`：判断一个数是否不为 0。如果不为 0，返回这个数本身；否则返回 `Fail`。

## [Reversed Iota's](https://codegolf.stackexchange.com/q/199290/9288)

```
\range1 \range1 \map \reverse
```

- [ ] `\range1`：输入一个整数，输出一个从 1 到这个整数的列表。比如输入 `3`，输出 `[1, 2, 3]`。区别于 `\range0`，后者输出的是从 0 到这个整数减 1 的列表。这两个函数都是自动向量化的，所以可以直接省去一个 `\map`。

05AB1E 有相当于 `\map \reverse` 的函数，Jelly 的 `\reverse` 干脆就是向量化的，Brachylog 也有相当于 `\range1 \reverse` 的函数。不清楚要不要给 Nekomata 也加上。

## [Covering a Skyline with brush strokes](https://codegolf.stackexchange.com/q/179464/9288)

```
0 \cons \delta \filter \positive \sum
```

或者

```
0 \cons \delta 0 \max \sum
```

- [ ] `\cons`：输入一个元素和一个列表，将这个元素插入到列表的开头。
- [ ] `\delta`：输入一个列表，输出列表中相邻元素的差。比如输入 `[1, 2, 3, 4]`，输出 `[1, 1, 1]`。
- [ ] `\filter`：助词。将函数应用到一个列表的每个元素上，过滤掉返回 `Fail` 的元素。
- [ ] `\positive`：判断一个数是否大于 0。如果大于 0，返回这个数本身；否则返回 `Fail`。
- [ ] `\max`：求两个数中的最大值。自动向量化。
- [ ] `\sum`：求一个列表的和。

两个想法：

1. 要不要把 `0 \cons \delta` 这种组合写成一个函数。不过感觉不是很常用。
2. 要不要让 `\positive` 这种作用于单个元素的谓词作用于列表时自动 filter。这样就不用写 `\filter` 了。


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
- [ ] `\logicalNot`：逻辑非。如果输入的数为 0，返回 1；如果输入的数不为 0，返回 0。不过给一个根本没有布尔值的语言加上逻辑运算符有点奇怪。就当它是一个数学函数好了。

## [Delannoy numbers](https://codegolf.stackexchange.com/q/225203/9288)

参考最高赞的 Jelly 解答：

```
\increment \range0 \apply2 \binomial \mul 2 \fromBaseRev
```

- [ ] `\range0`：输入一个整数，输出一个从 0 到这个整数减 1 的列表。比如输入 `3`，输出 `[0, 1, 2]`。区别于 `\range1`。
- [ ] `\apply2`：助词，将一个一元函数应用到栈顶的两个元素上。目前已实现的版本仅支持一元函数，而这里的 `\binomial` 是二元函数，需要修改。
- [ ] `\binomial`：输入两个整数，求它们的二项式系数。自动向量化。
- [ ] `\fromBaseRev`：输入一个列表和一个基数，将这个列表看作一个以这个基数为底的数，求它的十进制表示。比如输入 `[1, 2, 3]` 和 `10`，输出 `321`。进制转换时的输入列表正着反着都挺常见，所以 `\fromBase` 和 `\fromBaseRev` 都要实现。
