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

- [x] `\permutation`：求一个列表的任意一个排列。这个函数是 non-deterministic 的。
- [x] `\apply2`：助词，将一个一元函数应用到栈顶的两个元素上。已实现。
- [ ] `\chunks`：输入一个列表和一个整数，将列表分成若干个长度为该整数的子列表。
- [x] `\bytes`：输入一个字符串，输出一个列表，列表的每个元素是字符串中的一个字符的编码。
- [x] `\zipWith`：助词，将一个二元函数应用到两个列表的对应元素上。
- [x] `\less`：比较两个数的大小。如果第一个数小于第二个数，返回第一个数；否则返回 `Fail` 。
- [x] `\fromBase`：输入一个列表和一个整数，将列表中的元素看作是某个进制的数的各位，输出这个数的十进制表示。比如输入 `[1, 2, 3]` 和 `10`，输出 `123`。
- [x] `\sub`：求两个数的差。已实现。
- [x] `\neg`：求一个数的相反数。已实现。不清楚要不要加一个合并 `\neg` 和 `\sub` 的函数。
- [x] `\allValues`：求一个 non-deterministic 值的所有可能取值，返回一个列表。为了实现这个函数，需要对现有的运算机制做一些修改。
- [x] `\minimum`：求一个列表的最小值。

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

- [x] `\nub`：输入一个列表，将其中的重复元素去掉。比如输入 `[1, 2, 3, 2, 1]`，输出 `[1, 2, 3]`。不清楚要不要排序。这道题用不着排序，但其它题目可能会用到。
- [ ] `\setMinus`：输入两个列表，返回第一个列表中不在第二个列表中的元素。比如输入 `[1, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 3]`。如果第二个列表中的元素在第一个列表中有重复，那么只删除一个。比如输入 `[1, 2, 2, 3, 4]` 和 `[2, 4]`，输出 `[1, 2, 3]`。

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

## [Print all pandigital numbers](https://codegolf.stackexchange.com/q/257752/9288)

参考 05AB1E 解答：


```
\natural \predicate { \mul \swap \toBase \nub \swap \lengthIs }
```

9 个字节，输给了好几个 8 字节的解答。

这里用了两次 `\swap`，感觉有点亏。不清楚要不要改变 `\toBase` 和 `\lengthIs` 的参数顺序。

另一个问题是，助词如果修饰的不是单个内置函数，一定要加括号；能不能把括号就当作助词的一部分，不用写出来？不过那样在助词修饰单个内置函数时反而需要多加一个右括号，而且 parser 也有点难写。

## [There's more than one way to skin a set](https://codegolf.stackexchange.com/q/247326/9288)

```
\subset \sum \allValues \dup \nub \setMinus \nub
```

参考了前面 [Count repetitions of an array](https://codegolf.stackexchange.com/q/180302/9288) 的解答。

- [x] `\subset`：求一个列表的任意一个子集。这个函数是 non-deterministic 的。

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
\prefix \nonempty \splitRuns \length
```

几次用到 `\prefix` 都只需要用到非空的前缀。可以考虑让它只输出非空的前缀，省去 `\nonempty`。但如果碰上确实需要空前缀的情况，需要补上的字节数会更多。`\suffix` 和 `\subsequence` 也有类似的问题。

## [Approximate a root of an odd degree polynomial](https://codegolf.stackexchange.com/q/258021/9288)

由于 Nekomata 目前仅支持整数，暂时还没办法解决这个问题。

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

## [String rotation - output string repeatedly moving first character to the end](https://codegolf.stackexchange.com/q/177221/9288)

目前只用已实现的函数已经可以解答，但特别长：

```
\suffix \allValues \swap \prefix \allValues \zipWith \join \tail
```

看来还需要一些新的函数。

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

## [Simplify a Cycle](https://codegolf.stackexchange.com/q/256920/9288)

用已有的函数，目前能想到的最短的是 13 个字节：

```
\iterate { \uncons \swap \suffix \map { \swap \ne } \oneValue } \head
```

加上新函数的话，可以：

```
\iterate { \uncons \dip \suffix \map2 \ne \oneValue } \head
```

- [ ] `\map2`：助词。将一个二元函数应用到第一个参数（列表）的每个元素上。`f \map2` 等价于 `\swap \map { \swap f }`。

甚至可以：

```
\iterate { \uncons \dip \suffix \free \oneValue } \head
```

- [ ] `\free`：检查一个列表是否不包含某个元素。如果包含，返回 `Fail`；否则返回列表本身。不清楚是不是还要一个 `\notElem` 函数，在不包含的时候返回的是元素而非列表。

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
- [ ] `\allEqual`：检查一个列表中的所有元素是否相等。
- [ ] `\delete`：从一个列表中删除所有等于某个元素的元素。
- [ ] `\isEmpty`：检查一个列表是否为空。

Vyxal、Jelly、05AB1E 都用到了类似于 fixed-point 的函数，但这个在 non-deterministic 语言中不好弄，语义也不是很明确。

## [Fold a List in Half](https://codegolf.stackexchange.com/q/136887/9288)

```
\unjoin \orApply { 0 \cons } \reverse \zipWith \add
```

- [ ] `\unjoin`：non-deterministic 地将一个列表拆成两部分。

```
\length \increment 2 \div \splitAt \reverse \add
```

- [ ] `\splitAt`：将一个列表从第 n 个元素处拆成两部分。


## [Find the Prime Signature](https://codegolf.stackexchange.com/q/256147/9288)

```
\factor \tally \sort \reverse
```

- [ ] `\factor`：将一个整数分解成质因数。比如说 `12` 分解成 `[2, 2, 3]`。
- [ ] `\tally`：统计一个列表中每个元素出现的次数。比如说 `[2, 2, 3]` 统计成 `[2, 3] [2, 1]`。

`\factor` 的实现可以用 arithmoi 包的 `factorise` 函数，不过这个函数的返回值是已经 tally 过的。如果 `\factor` 也直接返回已经 tally 过的结果，那么不但实现更方便，还可以减少一个字节。等遇到更多涉及到质因数分解的题目再说。

---

目前已发布 0.1.0.0 版。还有很多常用的 built-in 没有实现。另外，由于 code page 中字符的选取过于随意，在常见的字体中甚至做不到等宽；可能需要重新设计一下 code page。

不想让这篇文档太长，前面已实现的部分就删去了。

## [Shortest Code to Find the Smallest Missing Positive Integer](https://codegolf.stackexchange.com/q/258335/9288)

目前已有6个字节的解法。但如果实现了 `\free` 的话可以更短：

```
\natural \positive \predicate \free
```

## [Shortest Valid Parentheses](https://codegolf.stackexchange.com/questions/258511/shortest-valid-parentheses/258549#258549)

目前的解法有 17 个字节，输得比较惨。主要问题是缺乏字符串操作。比如说，如果有一个字符串替换的 built-in 的话，可以：

```
\subsequence \predicate { "()" \emptyString \stringReplaceRepeated \isEmpty } \length \allValues \maximum
```

- [ ] `\emptyString`：空字符串。
- [ ] `\stringReplaceRepeated`：重复地将一个字符串中的某个子串替换成另一个子串，直到没有子串可以替换为止。这个函数有点复杂，感觉用得不多，以后再说。

## [Is it a balanced number?](https://codegolf.stackexchange.com/q/94291/9288)

```
\ten \digits \dup \length \increment 2 \div \splitAt \apply2 \sum \equal
```

大量复用了 Fold a List in Half 一题的解法。不清楚这种把一个列表拆成两半的操作常不常见，是否需要增加专门的 built-in。

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
