# 一些想法

## CodePage

此处总结一下 Code Page 中已有但还没有用上的字符：

```
&'.VWXY`vwy|
```

有些是已经确定分配给什么函数的，比如说 `|` 给 `\bitOr`，`&` 给 `\bitAnd`，`X` 给 `\bitXor`。别的都还没想好。

如果 CodePage 中的 256 个字符都用完了，可以考虑用像 05AB1E 那样，用 `.` 开头的字符来表示双字节的内置函数。现在的 CodePage 还远远没有用完，先不考虑这个问题，但 `.` 也先不要用掉。

LiberationMono 字体所支持的字符列举于[此文件](analysis/LiberationMonoGlyphs.txt)，以供选取。

## 来自统计结果的想法

写了个[脚本](analyze.py)来统计、分析现有的解答。得到一些有趣的结果（以下排名可能不是最新的）：

* 最常用的一个字符是 `{`，用来开启一个 block。
* 单个字符中，排第二、第三的分别是 `$`（`\swap`）和 `:`（`\dup`），都是常见的栈操作。其它 stack-based 的 golfing 语言中，这两个操作也排名很靠前。
* 四到七名分别是 `∑`（`\sum`）、`+`（`\add`）、`=`（`\equal`）、`←`（`\decrement`）。这些也都是常见的操作。
* `}` 排到并列第七，出现次数不到 `{` 的一半。看来代码结尾可以省略 `}` 的设计是正确的。
* `a`（`\allValues`）排第九。这是 non-deterministic 语言特有的操作，算是 Nekomata 的特色之一。
* `ç`（`\cons0`）能排到第十。这个完全出乎我的意料，因为很多别的 golfing 语言中根本没有这个操作。它的作用是给列表加一个 0，通常是配合 `\head`、`\last`、`\minimum`、`\maximum` 等函数使用，来处理空列表的特殊情况。需要看一看其它语言是怎么处理空列表的。
* `R`（`\range1`）并列第十，出现次数比 `r`（`\range0`）多得多。这是因为很多函数和助词在参数是数字时会自动按 `\range0` 来转换成列表，所以不需要显式地写出 `\range0`。
* 在两个字符的组合中，排前七的分别是 `ᶦ{`、`ʷ{`、`ˡ{`、`ᵖ{`、`ᵑ{`、`ᵐ{`、`ᵏ{`。也许可以像 Vyxal 等语言一样，让这些助词自动开启 block，省去一个 `{`。不过另外一些助词通常只修饰单个 built-in，自动开启 block 反而会额外需要一个 `}`。尤其是 `ᵖ`、`ᵑ`、`ᵐ`，两种用法都很常见，不好取舍。详见 [`analysis/particles.txt`](particles.txt)。
* 多个字符的组合目前样本还不够多，暂时分析不出什么有用的信息。
* 统计结果与其它语言的差异，除了考虑到语言本身的特点之外，还要考虑到语言的使用者的偏好。目前 Nekomata 的使用者只有我自己，解答的也主要是我感兴趣的题目类型，string 相关的题目较少，ascii-art 更是完全没有。

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

### [How long to carry sort?](https://codegolf.stackexchange.com/q/259167/9288)

```
\ordering \enumerate \sub \cons0 \maximum
```

- [ ] `\ordering`：求一个列表的排序索引。比如说 `[3, 1, 2]` 的排序索引是 `[1, 2, 0]`。

### [Make a Custom Bayer Matrix](https://codegolf.stackexchange.com/q/259633/9288)

```
\range0 \binary \dup \outer { \dip \dup \bitxor \mul2 \add \cons0 4 \recip \fromBaseRev }
```

- [ ] `\bitxor`：按位异或。

### [Is it a completely even number?](https://codegolf.stackexchange.com/q/142534/9288)

```
\decrement \bitxor \less
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

### [Pseudofactorial](https://codegolf.stackexchange.com/q/82494/9288)

```
\range1 \foldl1 \lcm
```

- [ ] `\foldl1`：从左到右地将一个二元函数应用于一个列表中的所有元素。比如说 `[1, 2, 3] \foldl1 \add` 等价于 `1 2 \add 3 \add`。

### [Generate a Walsh Matrix](https://codegolf.stackexchange.com/q/162254/9288)

```
\powOf2 \dup \outer \bitand \popcount \neg1 \pow
```

- [ ] `\bitand`：按位与。
- [ ] `\popcount`：求一个数的二进制表示中 1 的个数。

位运算还是很有用的。

### [Shortest distinguishable slice](https://codegolf.stackexchange.com/q/259707/9288)

```
\charToInt \pad \transpose \enumerate \subsequence \nonempty \predicate { \nth \transpose \isUnique } \allValues \shortest \dupDip \first \last \increment \pair
```

- [ ] `\pad`：输入一个列表的列表，将每个列表的长度补齐到最长的子列表的长度。比如说 `[[1, 2], [3, 4, 5]]` 补齐成 `[[1, 2, 0], [3, 4, 5]]`。

### [Find Index of Rational Number in Calkin-Wilf Sequence](https://codegolf.stackexchange.com/q/260472/9288)

需要 `-n` flag。


```
\iterate { \recip \decrement \neg1 \divMod \add \neg \nonZero }
```

- [ ] `\divMod`：同时求商和余数。由于有两个返回值，还不知道怎么向量化。

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
### [Least Common Multiple](https://codegolf.stackexchange.com/q/94999/9288)

```
\foldl1 \lcm
```

### [Remove duplicates from my academic transcript](https://codegolf.stackexchange.com/q/256441/9288)

```
\map \first \groupBy \anyOf \dup \last \last \minimumBy
```

### [XOR of independent Bernoulli variables](https://codegolf.stackexchange.com/q/262140/9288)

```
\mul2 \complement \product \complement \div2
```

- [ ] `\complement`：1 减去一个数。

### [Generate list of numbers and their negative counterparts](https://codegolf.stackexchange.com/q/203797/9288)

```
\interval \orNeg
```

- [ ] `\interval`：输入两个数字，输出这两个数字之间的所有整数。比如输入 `3` 和 `7`，输出 `[3, 4, 5, 6, 7]`。