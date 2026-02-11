import random, time, gc, statistics
from functools import lru_cache
import matplotlib.pyplot as plt


def accept_naive(s):
    n = len(s)

    def parseS(i):
        if i + 2 > n or s[i:i+2] != "aa":
            return set()
        res = set()

        def go(pos, xS):
            res.add((pos, xS))
            if pos + 2 <= n and s[pos:pos+2] == "aa":
                for endT, xT in parseT(pos + 2):
                    if xT > xS:
                        go(endT, (xS + 1) * xT)

        go(i + 2, 0)
        return res

    def parseT(i):
        res = set()

        def tail(pos, xT):
            res.add((pos, xT))
            if pos + 2 <= n and s[pos:pos+2] == "ab":
                for endS, xS in parseS(pos + 2):
                    tail(endS, max(xT, xS) + 1)

        core = {(i, 1)}
        if i < n and s[i] == "b":
            for mid, xInner in parseT(i + 1):
                if mid < n and s[mid] == "b":
                    core.add((mid + 1, xInner))

        for pos, x0 in core:
            tail(pos, x0)

        return res

    return any(end == n for end, _ in parseS(0))


def accept_fast(s):
    n = len(s)
    if n < 2 or s[:2] != "aa":
        return False

    @lru_cache(None)
    def parseS(i):
        if i + 2 > n or s[i:i+2] != "aa":
            return ()
        best = {i + 2: 0}
        stack = [(i + 2, 0)]
        while stack:
            pos, xS = stack.pop()
            if pos + 2 <= n and s[pos:pos+2] == "aa":
                for endT, xT in parseT(pos + 2):
                    if xT > xS:
                        nx = (xS + 1) * xT
                        prev = best.get(endT)
                        if prev is None or nx < prev:
                            best[endT] = nx
                            stack.append((endT, nx))
        return tuple(best.items())

    @lru_cache(None)
    def parseT(i):
        best = {i: 1}

        if i < n and s[i] == "b":
            for mid, xInner in parseT(i + 1):
                if mid < n and s[mid] == "b":
                    end = mid + 1
                    prev = best.get(end)
                    if prev is None or xInner < prev:
                        best[end] = xInner

        stack = list(best.items())
        while stack:
            pos, xT = stack.pop()
            if pos + 2 <= n and s[pos:pos+2] == "ab":
                for endS, xS in parseS(pos + 2):
                    nx = (xT if xT >= xS else xS) + 1
                    prev = best.get(endS)
                    if prev is None or nx < prev:
                        best[endS] = nx
                        stack.append((endS, nx))

        return tuple(best.items())

    for end, _ in parseS(0):
        if end == n:
            return True
    return False


def can_pos_len(L):
    if L < 20 or (L - 8) % 4 != 0:
        return False
    t = (L - 8) // 4
    n_max = (t - 2) // 3
    return n_max >= 1


def gen_pos_len(L):
    if not can_pos_len(L):
        return None
    t = (L - 8) // 4
    n_max = (t - 2) // 3
    n1 = random.randint(1, n_max)
    m1 = t - n1
    return "a" * 6 + "abaa" * n1 + "aa" + "abaa" * m1


def mutate_same_len(w):
    if not w:
        return w
    i = random.randrange(len(w))
    c = "a" if w[i] == "b" else "b"
    return w[:i] + c + w[i + 1:]


def gen_neg_len(L, tries=8000):
    for _ in range(tries):
        if can_pos_len(L):
            base = gen_pos_len(L)
            w = base
            for _ in range(random.randint(1, 3)):
                w = mutate_same_len(w)
            if not accept_fast(w):
                return w

        w = "".join(random.choice("ab") for _ in range(L))
        if not accept_fast(w):
            return w

    return "b" * (L - 1) + "a"


def build_pools(lengths, per_len=200):
    pos, neg = {}, {}
    for L in lengths:
        pl, nl = [], []
        for _ in range(per_len):
            w = gen_pos_len(L)
            if w is not None:
                pl.append(w)
        for _ in range(per_len):
            nl.append(gen_neg_len(L))
        if pl and nl:
            pos[L] = pl
            neg[L] = nl
    return pos, neg


def fuzz_check(pos, neg, trials=5000):
    lens = [L for L in sorted(pos.keys()) if L in neg and pos[L] and neg[L]]
    if not lens:
        print("no data for fuzz")
        return
    mism = 0
    ex = None
    for _ in range(trials):
        L = random.choice(lens)
        w = random.choice(pos[L]) if random.random() < 0.5 else random.choice(neg[L])
        a = accept_naive(w)
        b = accept_fast(w)
        if a != b:
            mism += 1
            if ex is None:
                ex = (w, a, b)
    print("fuzz mismatches:", mism)
    if ex:
        w, a, b = ex
        print("example:", w, "naive=", a, "fast=", b)


def bench(words, fn, repeats=9, warmup=2):
    for _ in range(warmup):
        for w in words:
            fn(w)

    old = gc.isenabled()
    gc.disable()

    xs = []
    try:
        for _ in range(repeats):
            t0 = time.perf_counter()
            for w in words:
                fn(w)
            t1 = time.perf_counter()
            xs.append((t1 - t0) / max(1, len(words)))
    finally:
        if old:
            gc.enable()

    return statistics.median(xs)


def plot_speed(pool_by_len, title, outname, per_len_used=160, repeats=9):
    xs = sorted(pool_by_len.keys())
    if not xs:
        print("no data for", outname)
        return
    yn, yf = [], []
    for L in xs:
        ws = pool_by_len[L][:per_len_used]
        yn.append(bench(ws, accept_naive, repeats=repeats))
        yf.append(bench(ws, accept_fast, repeats=repeats))

    plt.figure(figsize=(7.0, 3.2))
    ax = plt.gca()
    ax.plot(xs, yn, linewidth=1.7, label="naive")
    ax.plot(xs, yf, linewidth=1.7, label="optimized")
    ax.set_title(title)
    ax.set_xlabel("length")
    ax.set_ylabel("median sec/word")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.legend(frameon=False)
    plt.tight_layout()
    plt.savefig(outname, dpi=190)
    plt.close()
    print("saved", outname)


if __name__ == "__main__":
    random.seed(0)

    lengths = [L for L in range(28, 141, 4) if can_pos_len(L)]
    pos, neg = build_pools(lengths, per_len=300)

    fuzz_check(pos, neg, trials=12000)

    plot_speed(pos, "IN language", "speed_in.png", per_len_used=220, repeats=9)
    plot_speed(neg, "NOT in language", "speed_out.png", per_len_used=220, repeats=9)
