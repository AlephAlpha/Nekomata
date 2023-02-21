# 一些想法

Nekomata 现在已经有了一个非常简单的解释器。不过已有的内置函数非常少，写不出什么有用的程序。准备等内置函数丰富一些之后再写文档。

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
- [ ] `\product`：求一个列表的乘积。这个函数应该是比较常用的。已实现整数版本，待添加有理数支持。
- [ ] `\div`：求两个数的商。目前已经有了这个函数，但是只支持整数，不支持有理数。加上有理数之后，可能需要三种版本的 `\div`：`\div`、`\divInt` 和 `\divExact`。`\div` 就是普通的除法；`\divInt` 是先除再取整；`\divExact` 是整除，如果结果不是整数就返回 `Fail`。

或者抄现有的 05AB1E 解答：

```
\dup \decrement \div \product \mul
```

- [x] `\dup`：复制栈顶元素。已实现。
- [x] `\decrement`：将一个数减 1。
- [ ] `\mul`：乘法。已实现，待添加有理数支持。

如果不支持有理数，这道题也不是不能解答，只是解答会比较长：

```
\product \mul \swap \decrement \product \div
```

- [x] `\dupDip`：这是个助词，不知道叫什么名字好。其作用是调用完函数之后把原来的栈顶元素再压回去。其它语言里没见过这个东西，但好像挺有用的。

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
- [x] `\zipWith`：助词，将一个二元函数应用到两个列表的对应元素上。
- [x] `\less`：比较两个数的大小。如果第一个数小于第二个数，返回第一个数；否则返回 `Fail` 。
- [x] `\fromBase`：输入一个列表和一个整数，将列表中的元素看作是某个进制的数的各位，输出这个数的十进制表示。比如输入 `[1, 2, 3]` 和 `10`，输出 `123`。
- [x] `\sub`：求两个数的差。已实现。
- [x] `\neg`：求一个数的相反数。已实现。不清楚要不要加一个合并 `\neg` 和 `\sub` 的函数。
- [x] `\allValues`：求一个 non-deterministic 值的所有可能取值，返回一个列表。为了实现这个函数，需要对现有的运算机制做一些修改。
- [ ] `\minimum`：求一个列表的最小值。

## [Smallest groups in an array](https://codegolf.stackexchange.com/q/79037/9288)

```
\splitRuns \dup \map \length \minimumBy
```

- [ ] `\splitRuns`：将一个列表从相邻元素不相等的地方拆开，得到一个列表的列表。比如输入 `[1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 6]`，输出 `[[1], [2, 2], [3, 3, 3], [4], [5, 5, 5, 5], [6]]`。
- [x] `\map`：助词，将一个一元函数应用到一个列表的每个元素上。
- [x] `\length`：求一个列表的长度。
- [ ] `\minimumBy`：输入两个列表，其长度必须一致。根据第一个列表中最小的元素的索引，返回第二个列表中对应的元素。由于最小的元素可能不止一个，所以这个函数是 non-deterministic 的。

和前面的 `\groupBy` 一样，`\minimumBy` 也是一个普通的函数而不是助词。但如果改成助词的话，解答会更短一些：

```
\splitRuns \minimumBy \length
```

看来是用普通函数还是助词还需要斟酌。可能两种都要支持，不过名字怎样区分是个问题。

## [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288)

```
\nub \setMinus \nub \length
```

- [ ] `\nub`：输入一个列表，将其中的重复元素去掉。比如输入 `[1, 2, 3, 2, 1]`，输出 `[1, 2, 3]`。不清楚要不要排序。这道题用不着排序，但其它题目可能会用到。
- [ ] `\setMinus`：输入两个列表，返回第一个列表中不在第二个列表中的元素。比如输入 `[1, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 3]`。如果第二个列表中的元素在第一个列表中有重复，那么只删除一个。比如输入 `[1, 2, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 2, 3]`。

## [Running second maximum of a list](https://codegolf.stackexchange.com/q/138510/9288)

参考了现有的 Husk 解答：

```
\prefix \sort \reverse 2 \nth
```

- [x] `\prefix`：求一个列表的一个前缀。这个函数是 non-deterministic 的。
- [x] `\reverse`：倒转一个列表。
- [x] `\nth`：输入一个列表和一个整数，返回列表中第 n 个元素。如果列表长度小于 n，返回 `Fail`。

不清楚要不要加上一个 `\nth-from-end` 函数，这样就可以写成 `\prefix \sort 2 \nth-from-end`。

## [The inverse Collatz Conjecture](https://codegolf.stackexchange.com/q/175248/9288)

有点麻烦，因为这需要一个 while 循环，循环里还有 if 判断。

```
\repeatNonDet { \nonzero \dup \decrement 2 \divExact \swap 3 \mul \increment \choice \oneValue }
```

有 13 个字节了，和其它语言的解答相比输得比较惨。可能还需要找到一些更好地模拟 while 循环和 if 判断的办法。关键是常规的 while 和 if 作为高阶函数都应该输入两个或三个函数，但 Nekomata 的助词只能输入一个函数。

- [x] `\repeatNonDet`：助词。输入一个函数，重复执行这个函数 non-deterministic 次。这个函数是 non-deterministic 的。
- [x] `\nonzero`：判断一个数是否不为 0。如果不为 0，返回这个数本身；否则返回 `Fail`。
- [x] `\divExact`：输入两个整数，求它们的整除。如果不能整除，返回 `Fail`。
- [x] `\swap`：交换栈顶的两个元素。已实现。
- [x] `\increment`：将一个数加 1。
- [x] `\choice`：在栈顶的两个元素中选择一个。这个函数是 non-deterministic 的。
- [x] `\oneValue`：求一个 non-deterministic 值的第一个可能取值。这个函数和 `\allValues` 一样，需要对现有的运算机制做一些修改。不清楚要不要把 `\choice` 和 `\oneValue` 合并成一个函数，这样可能可以更方便地模拟 if 判断。

## [Reversed Iota's](https://codegolf.stackexchange.com/q/199290/9288)

```
\range1 \range1 \map \reverse
```

- [x] `\range1`：输入一个整数，输出一个从 1 到这个整数的列表。比如输入 `3`，输出 `[1, 2, 3]`。区别于 `\range0`，后者输出的是从 0 到这个整数减 1 的列表。这两个函数都是自动向量化的，所以可以直接省去一个 `\map`。

05AB1E 有相当于 `\map \reverse` 的函数，Jelly 的 `\reverse` 干脆就是向量化的，Brachylog 也有相当于 `\range1 \reverse` 的函数。不清楚要不要给 Nekomata 也加上。

以上的解答输出的是列表的列表。也可以输出一个 non-deterministic 的列表：

```
\range1 \prefix \nonempty \reverse
```

- [x] `\nonempty`：判断一个列表是否为空。如果不为空，返回这个列表本身；否则返回 `Fail`。

## [Covering a Skyline with brush strokes](https://codegolf.stackexchange.com/q/179464/9288)

```
0 \cons \delta \filter \positive \sum
```

或者

```
0 \cons \delta 0 \max \sum
```

- [x] `\cons`：输入一个元素和一个列表，将这个元素插入到列表的开头。
- [ ] `\delta`：输入一个列表，输出列表中相邻元素的差。比如输入 `[1, 2, 3, 4]`，输出 `[1, 1, 1]`。
- [ ] `\filter`：助词。将函数应用到一个列表的每个元素上，过滤掉返回 `Fail` 的元素。
- [x] `\positive`：判断一个数是否大于 0。如果大于 0，返回这个数本身；否则返回 `Fail`。
- [x] `\max`：求两个数中的最大值。自动向量化。
- [x] `\sum`：求一个列表的和。

两个想法：

1. 要不要把 `0 \cons \delta` 这种组合写成一个函数。不过感觉不是很常用。这道题里甚至可以用 `0 \cons \sub` 来代替。
2. 要不要让 `\positive` 这种作用于单个元素的谓词作用于列表时自动 filter。这样就不用写 `\filter` 了。实现后又后悔了，改回来了。

想了一下还是不要自动 filter；自动 filter 只对单层的列表有用，对多层嵌套的列表没什么意义；但自动向量化什么时候都用得上。而且这种涉及到 Cut 的东西我自己都弄得不太清楚，容易出 bug。

目前可以写成：

```
0 \cons \sub \positive \removeFail \sum
```

如果实现了 `\absDiff`，可以写成：

```
0 \cons \max \absDiff \sum
```

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

## [Delannoy numbers](https://codegolf.stackexchange.com/q/225203/9288)

参考最高赞的 Jelly 解答：

```
\increment \range0 \apply2 \binomial \mul 2 \fromBaseRev
```

- [x] `\range0`：输入一个整数，输出一个从 0 到这个整数减 1 的列表。比如输入 `3`，输出 `[0, 1, 2]`。区别于 `\range1`。
- [x] `\apply2`：助词，将一个一元函数应用到栈顶的两个元素上。目前已实现的版本仅支持一元函数，而这里的 `\binomial` 是二元函数，需要修改。
- [ ] `\binomial`：输入两个整数，求它们的二项式系数。自动向量化。
- [ ] `\fromBaseRev`：输入一个列表和一个基数，将这个列表看作一个以这个基数为底的数，求它的十进制表示。比如输入 `[1, 2, 3]` 和 `10`，输出 `321`。进制转换时的输入列表正着反着都挺常见，所以 `\fromBase` 和 `\fromBaseRev` 都要实现。

## [Print all pandigital numbers](https://codegolf.stackexchange.com/q/257752/9288)

参考 05AB1E 解答：


```
\natural \predicate { \mul \swap \toBase \nub \swap \lengthIs }
```

9 个字节，输给了好几个 8 字节的解答。

- [x] `\natural`：non-deterministic 地输出任意一个自然数。
- [x] `\predicate`：助词，将一个函数应用到栈顶的一个元素上，如果这个函数返回 `Fail`，则返回 `Fail`；否则将这个元素保留。即使这个函数是多元函数，计算时也只会弹出栈顶的一个元素，其他的元素只用于计算，不会被弹出。
- [ ] `\toBase`：输入一个整数和一个基数，将这个整数转换成以这个基数为底的数。比如输入 `321` 和 `10`，输出 `[1, 2, 3]`。
- [x] `\lengthIs`：判断一个列表的长度是否等于一个整数。如果相等，返回这个列表本身；否则返回 `Fail`。

这里用了两次 `\swap`，感觉有点亏。不清楚要不要改变 `\toBase` 和 `\lengthIs` 的参数顺序。

另一个问题是，助词如果修饰的不是单个内置函数，一定要加括号；能不能把括号就当作助词的一部分，不用写出来？不过那样在助词修饰单个内置函数时反而需要多加一个右括号，而且 parser 也有点难写。

## [There's more than one way to skin a set](https://codegolf.stackexchange.com/q/247326/9288)

```
\subset \sum \allValues \dup \nub \setMinus \nub
```

参考了前面 [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288) 的解答。

- [x] `\subset`：求一个列表的任意一个子集。这个函数是 non-deterministic 的。

也许要给 Nekomata 加上一个输出时自动去重的选项。

## [Make a Court Transcriber](https://codegolf.stackexchange.com/q/252927/9288)

```
\anyOf \predicate { \unconcat \subset \eq }
```

- [x] `\anyOf`：non-deterministic 地输出任意一个列表中的元素。
- [ ] `\unconcat`：将一个列表拆成多个非空子列表。比如输入 `[1, 2, 3, 4, 5, 6, 7, 8, 9]`，其中一个可能的输出是 `[[1, 2], [3, 4, 5, 6], [7, 8, 9]]`。这个函数是 non-deterministic 的。
- [x] `\eq`：判断两个元素是否相等。如果相等，返回这个元素本身；否则返回 `Fail`。


## [Count edits accounting for grace period](https://codegolf.stackexchange.com/q/141949/9288)

想到两种解法，其中第一种解法胜过了 Jelly、05AB1E，仅输给于 Husk：

```
\repeatNonDet { \uncons 4 \add \greater \removeFail \nonempty }
```

- [x] `\uncons`：输入一个列表，输出一个二元组，第二个元素是列表的第一个元素，第一个元素是列表的剩余部分。如果列表为空，返回 `Fail`。
- [x] `\greater`：判断一个整数是否大于另一个整数。如果大于，返回这个整数本身；否则返回 `Fail`。
- [x] `\countValues`：求一个 non-deterministic 值的所有可能取值的个数。

需要用 `-n` flag 来输出结果的可能取值数目，而非结果本身。

另一种解法则比较长：

```
\unconcat \map { \minMax \swap \sub 5 \less } \allValues \last \length
```

- [ ] `\minMax`：求一个列表的最小值和最大值。不清楚应该哪个在前，哪个在后；如果是最大值在前，后面的 `\swap` 就可以省略。
- [ ] `\last`：求一个列表的最后一个元素。如果列表为空，返回 `Fail`。由于还没确定 `\unconcat` 中各种可能的取值按什么顺序排列，这里也有可能是 `\first`，此时可以和前面的 `\allValues` 合并成 `\oneValue`。

如果能支持递归就好了。但是递归会让函数的 arity 无法确定。

## [Construct the Identity Matrix](https://codegolf.stackexchange.com/q/70365/9288)

```
\outer \sub \logicalNot
```

- [x] `\outer`：助词，将一个二元函数作用到两个列表的所有元素上，返回一个列表的列表。比如输入 `[1, 2, 3]` 和 `[4, 5, 6]`，以及一个函数 `f`，输出 `[[f(1, 4), f(1, 5), f(1, 6)], [f(2, 4), f(2, 5), f(2, 6)], [f(3, 4), f(3, 5), f(3, 6)]]`。感觉这个助词实现起来有点复杂。

## [Split some points](https://codegolf.stackexchange.com/q/257870/9288)

参考现有的 Vyxal 解答：

```
\integer \allValues \dup \anyPair \lengthIs \predicate { 1 \neg \cons \swap \map { 1 \cons \reverse \dot \sign \nonzero } \sum 0 \eq }
```

- [x] `\integer`：non-deterministic 地输出任意一个整数，按 `0, 1, -1, 2, -2, 3, -3, ...` 的顺序。
- [ ] `\anyPair`：输入两个列表，输出一个二元组，第一个元素是第一个列表的任意一个元素，第二个元素是第二个列表的任意一个元素。这个函数是 non-deterministic 的。实现时要注意两个列表的长度都可能是无穷的。
- [x] `\dot`：求两个列表的点积。如果两个列表的长度不同，返回 `Fail`。
- [x] `sign`：求一个整数的符号。如果是正数，返回 `1`；如果是负数，返回 `-1`；如果是零，返回 `0`。

## [Fibonacci function or sequence](https://codegolf.stackexchange.com/q/85/9288)

参考 Brachylog 解答：

```
1 \dup \repeatNonDet { \swap \dupDip \add ｝
```

没有输入，输出一个 non-deterministic 的值，其可能取值是 `1, 1, 2, 3, 5, ...`。

另一种解答：

```
\while { \decrement \positive \dup \decrement \choice }
```

1-indexed 地输出数列 `1,1,2,3,5,...`。需要用 `-n` flag 来输出结果的可能取值数目，而非结果本身。

- [x] `\while`：助词。输入一个函数，重复执行这个函数，直到 fail 为止。

## [Chunk + Enumerate a list of digits](https://codegolf.stackexchange.com/q/189932/9288)

参考 Jonathan Allan 的 Jelly 解答：

```
\prefix \nonempty \splitRuns \length
```

几次用到 `\prefix` 都只需要用到非空的前缀。可以考虑让它只输出非空的前缀，省去 `\nonempty`。但如果碰上确实需要空前缀的情况，需要补上的字节数会更多。`\suffix` 和 `\subsequence` 也有类似的问题。

## [Recognize a counting tree](https://codegolf.stackexchange.com/q/257998/9288)

参考 Charcoal 的解答：

```
\subsequence \uncons \dupDip \lengthIs \range1 \reverse \swap \sub \map \positive \allValues \mul \swap \head \increment \lengthIs
```

16 个字节，而且只用到已实现的函数。于是这成了 Nekomata 在 code golf 上的[第一个解答](https://codegolf.stackexchange.com/a/258071/9288)。

- [x] `\subsequence`：求一个列表的任意一个连续的子序列。这个函数是 non-deterministic 的。
- [x] `\head`：求一个列表的第一个元素。如果列表为空，返回 `Fail`。

把 `\less` 向量化后还可以省三个字节：

```
\subsequence \uncons \dupDip \lengthIs \range1 \reverse \less \allValues \mul \swap \head \increment \lengthIs
```

## [Approximate a root of an odd degree polynomial](https://codegolf.stackexchange.com/q/258021/9288)

由于 Nekomata 目前仅支持整数，暂时还没办法解决这个问题。

假设 Nekomata 支持有理数，可以参考 Charcoal 的解答：

```
\abs \sum \dup \repeatNonDet { \dupDip { \dip \dup \sub \choice \predicate { \fromBase 0 \greaterEq } \oneValue } 2 \div } \swap
```

23 个字节，而且不知道对不对。也许还有改进的空间。

## [Mode (most common element) of a list](https://codegolf.stackexchange.com/q/42529/9288)

```
\map \count \maximumBy
```

需要 `-1` flag 来仅输出第一个结果。

- [ ] `\count`：求一个元素在一个列表中出现的次数。
- [ ] `\maximumBy`：输入两个列表，其长度必须一致。根据第一个列表中最大的元素的索引，返回第二个列表中对应的元素。由于最大的元素可能不止一个，所以这个函数是 non-deterministic 的。

## [Determine if an Array contains something other than 2](https://codegolf.stackexchange.com/q/120350/9288)

```
2 \sub \logicalNot \div
```

还是无法避免除以 0 这种 hack。


## [String rotation - output string repeatedly moving first character to the end](https://codegolf.stackexchange.com/q/177221/9288)

目前只用已实现的函数已经可以解答，但特别长：

```
\suffix \allValues \swap \prefix \allValues \zipWith \join \tail
```

看来还需要一些新的函数。

```
\length \iterate { \uncons \swap \join }
```

- [ ] `\iterate`：助词。将一个函数作用 n 次，n 为输入的整数。如果 n 为负数，返回 `Fail`。

或者干脆：

```
\unjoin \nonempty \swap \join
```

- [ ] `\unjoin`：non-deterministic 地将一个列表拆成两个列表

甚至干脆加一个 `\rotate` 函数：

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

另外，像 `\minimum` 这样的函数，有两种可能的比较方法：一是仅仅在整数和整数、字符串和字符串之间比较，对于列表则自动向量化；二是在任意类型之间比较，不自动向量化。目前的 `\min` 函数用的是第一种方法，`\minimum` 不知该用哪种方法。

另一个问题是 `\toBase` 如何向量化。如果输入是一个列表和一个整数，对列表向量化就行。如果输入是两个列表，用 `\zipWith` 来向量化不一定是最佳的选择，也许可以用 `\outer` 来向量化。不过目前还没遇到过这种情况。

## [Generate All 8 Knight's Moves](https://codegolf.stackexchange.com/q/247676/9288)

目前想到的最短的是 10 个字节，参考的是 Brachylog 的解答：

```
2 \range1 \dup \reverse \choice \map { \dup \neg \choice }
```

可能需要加一个相当于 `\dup f \choice` 的助词，这样 10 个字节的解答可以减少到 8 个字节。如果实现了 `\permutation` 函数，用于替代 `\dup \reverse \choice`，还可以少一个字节。

另一种方案是 15 个字节：

```
apply2 { 5 \range0 2 \sub \anyOf } \pair \predicate { \abs \sum 3 \eq }
```

如果实现了前面说过的 `\anyPair` 函数，15 个字节的解答可以减少到 11 个字节：

```
5 \range0 2 \sub \anyPair \predicate { \abs \sum 3 \eq }
```

## [Simplify a Cycle](https://codegolf.stackexchange.com/q/256920/9288)

目前能想到的最短的是 13 个字节：

```
\repeatNonDet { \uncons \swap \suffix \map { \swap \ne } \oneValue } \head
```
