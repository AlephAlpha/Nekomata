import pathlib


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
            ngram = line[i : i + n]
            if ngram not in freq:
                freq[ngram] = 0
            freq[ngram] += 1
    freq = dict(sorted(freq.items(), key=lambda item: item[1], reverse=True))
    return freq


def particles_analysis(corpus: list[str]) -> dict[str, list[int]]:
    particles = "ᵃᶜᵈᵉᵋᶠʰᶦʲᴶᴷᵏˡᵐᵚᵑᵒᵖʳᵗʷˣʸᶻᶾ"
    freq = {}

    for particle in particles:
        freq[particle] = [0, 0]
        for line in corpus:
            freq[particle][0] += line.count(particle + "{")
            freq[particle][1] += line.count(particle)
    freq = dict(sorted(freq.items(), key=lambda item: item[1][0] / item[1][1]))
    return freq


current_dir = pathlib.Path(__file__).parent.absolute()
testcase_path = current_dir.parent / "test/Eval.hs"
output_dir = current_dir

corpus = get_corpus(testcase_path)

with open(output_dir / "corpus.txt", "w") as f:
    for line in corpus:
        f.write(line + "\n")

for n in range(1, 6):
    freq = ngram_freq(corpus, n)
    with open(output_dir / f"freq_{n}gram.txt", "w") as f:
        for ngram, count in freq.items():
            f.write(f"{ngram} : {count}\n")

particles = particles_analysis(corpus)
with open(output_dir / "particles.txt", "w") as f:
    f.write('particle : with "{" / total\n')
    for particle, count in particles.items():
        f.write(f"{particle} : {count[0]} / {count[1]}\n")
