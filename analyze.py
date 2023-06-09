import os


def get_corpus(path: str) -> list[str]:
    corpus = []
    with open(path, "r") as f:
        for line in f:
            if line.strip() == "specEval":
                corpus.append(eval(next(f).strip()))
    return corpus


def ngram_freq(corpus: list[str], n: int) -> dict[str, int]:
    freq = {}
    for line in corpus:
        for i in range(len(line) - n + 1):
            ngram = line[i:i+n]
            if ngram not in freq:
                freq[ngram] = 0
            freq[ngram] += 1
    freq = dict(sorted(freq.items(), key=lambda item: item[1], reverse=True))
    return freq


testcase_path = "test/Eval.hs"
output_dir = "analysis/"

if not os.path.exists(output_dir):
    os.makedirs(output_dir)

corpus = get_corpus(testcase_path)

with open(output_dir + "corpus.txt", "w") as f:
    for line in corpus:
        f.write(line + "\n")

for n in range(1, 6):
    freq = ngram_freq(corpus, n)
    with open(output_dir + f"freq_{n}gram.txt", "w") as f:
        for ngram, count in freq.items():
            f.write(f"{ngram} : {count}\n")
