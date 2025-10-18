from collections import Counter, namedtuple
import random, math

random.seed(13)

ALPHABET = ("a", "b")
BASE_RULES = (("aaa","bab"), ("bbb","aaa"))
EVENS = (4,6,8,10,12,14)
ODDS  = (3,5,7,9,11,13)

def ev_rule(k):
    return ("b" + "a"*k, "a"*k + "b")

def od_rule(k):
    return ("bb" + "a"*k, "a"*k + "bb")

class TRS:
    def __init__(self, rules):
        self.rules = rules
    def step(self, w):
        out = set()
        for L, R in self.rules:
            i = 0
            m = len(L)
            while True:
                j = w.find(L, i)
                if j < 0: break
                out.add(w[:j] + R + w[j+m:])
                i = j + 1
            i = 0
            m = len(R)
            while True:
                j = w.find(R, i)
                if j < 0: break
                out.add(w[:j] + L + w[j+m:])
                i = j + 1
        out.discard(w)
        return out

def systems():
    extra = tuple(ev_rule(k) for k in EVENS) + tuple(od_rule(k) for k in ODDS)
    return TRS(BASE_RULES), TRS(BASE_RULES + extra)

def grams(word, kmax=4):
    c = Counter()
    n = len(word)
    for k in range(1, kmax + 1):
        for i in range(n - k + 1):
            c[(k, word[i:i+k])] += 1
    return c

Inv = namedtuple("Inv", "length F1 F2 F3 F4")

def invariants(w):
    c = grams(w, 4)
    L = len(w)
    f1 = (c[(1,"a")] + c[(2,"ab")] + c[(2,"ba")] + c[(3,"aaa")] + c[(3,"aab")] + c[(3,"baa")] + c[(3,"bab")]) & 1
    f2 = (c[(1,"a")] + c[(2,"ab")] + c[(2,"ba")] + c[(3,"aba")] + c[(3,"abb")] + c[(3,"bba")] + c[(3,"bbb")]) & 1
    f3 = (c[(2,"aa")] + c[(3,"aab")] + c[(3,"baa")] + c[(4,"aaaa")] + c[(4,"aaab")] + c[(4,"baaa")] + c[(4,"baab")]) & 1
    f4 = (c[(2,"aa")] + c[(2,"ab")] + c[(2,"ba")] + c[(3,"abb")] + c[(3,"bba")] + c[(4,"abba")] + c[(4,"abbb")] + c[(4,"bbba")] + c[(4,"bbbb")]) & 1
    return Inv(L, f1, f2, f3, f4)

def rand_word(lo=6, hi=18):
    n = random.randint(lo, hi)
    return "".join(random.choice(ALPHABET) for _ in range(n))

def walk(sys, w, lo=1, hi=8):
    path = [w]
    for _ in range(random.randint(lo, hi)):
        nxt = list(sys.step(path[-1]))
        if not nxt: break
        path.append(random.choice(nxt))
    return path

def check_path(path):
    base = invariants(path[0])
    for u in path[1:]:
        cur = invariants(u)
        if cur != base:
            return False, base, cur, u
    return True, base, base, None

def rulewise(sys):
    ok, bad = True, []
    for L, R in sys.rules:
        iL, iR = invariants(L), invariants(R)
        if iL != iR:
            ok = False
            bad.append((L, R, iL, iR))
    return ok, bad

def mean(a, n):
    return a / n if n else 0.0

def fuzz(sys, trials=2000, loL=6, hiL=18, loS=1, hiS=8, keep=5):
    rep = []
    ok = 0
    sL = 0
    Lmin = math.inf
    Lmax = 0
    sS = 0
    Smin = math.inf
    Smax = 0
    stuck = 0
    for _ in range(trials):
        w0 = rand_word(loL, hiL)
        L0 = len(w0)
        sL += L0
        Lmin = min(Lmin, L0)
        Lmax = max(Lmax, L0)
        p = walk(sys, w0, loS, hiS)
        k = len(p) - 1
        sS += k
        Smin = min(Smin, k)
        Smax = max(Smax, k)
        if k == 0: stuck += 1
        g, b, v, w = check_path(p)
        if g: ok += 1
        else: rep.append((w0, p, b, v, w))
    stats = {
        "trials": trials,
        "word_len": {"avg": mean(sL, trials), "min": 0 if Lmin is math.inf else Lmin, "max": Lmax},
        "ok": ok,
        "fail": trials - ok,
        "ok_rate": ok / trials if trials else 0.0,
        "steps": {"avg": mean(sS, trials), "min": 0 if Smin is math.inf else Smin, "max": Smax},
        "stuck": stuck,
        "stuck_rate": stuck / trials if trials else 0.0
    }
    ex = []
    for w0, path, base, got, where in rep[:keep]:
        ex.append({"w0": w0, "viol_at": where, "path_len": len(path)-1, "path": path, "base_inv": base, "got_inv": got})
    return stats, ex

def show_rulewise(name, ok, bad, n_rules, cap=4):
    print(f"[{name}] rules={n_rules} status={'OK' if ok else 'FAIL'}")
    if not ok:
        print(f"violations={len(bad)}")
        for L, R, iL, iR in bad[:cap]:
            print(f"{L!r} <-> {R!r}")
            print(f"lhs={iL}")
            print(f"rhs={iR}")

def show_fuzz(name, s):
    print(f"\n== Fuzz: {name} ==")
    print(f"trials: {s['trials']}")
    wl = s["word_len"]
    print(f"word length: avg={wl['avg']:.2f}, min={wl['min']}, max={wl['max']}")
    print(f"OK={s['ok']} FAIL={s['fail']} rate={s['ok_rate']*100:.2f}%")
    st = s["steps"]
    print(f"steps: avg={st['avg']:.2f}, min={st['min']}, max={st['max']}")
    print(f"stuck: {s['stuck']} ({s['stuck_rate']*100:.2f}%)")

def show_examples(ex, cap=3, max_len=140):
    if not ex:
        print("\nNo violations.")
        return
    print("\nViolations:")
    for i, e in enumerate(ex[:cap], 1):
        p = " -> ".join(e["path"])
        if len(p) > max_len:
            p = p[:max_len-3] + "..."
        print(f"#{i} |path|={e['path_len']} w0={e['w0']!r} at={e['viol_at']!r}")
        print(f"base={e['base_inv']}")
        print(f"got ={e['got_inv']}")
        print(f"path={p}")

if __name__ == "__main__":
    S, SP = systems()
    okT, badT = rulewise(S)
    okP, badP = rulewise(SP)
    print("== Rulewise ==")
    show_rulewise("T", okT, badT, len(S.rules))
    show_rulewise("T'", okP, badP, len(SP.rules))
    sA, exA = fuzz(S, trials=2500, loL=6, hiL=18, loS=1, hiS=8, keep=5)
    sB, exB = fuzz(SP, trials=2500, loL=6, hiL=18, loS=1, hiS=8, keep=5)
    show_fuzz("T", sA)
    show_fuzz("T'", sB)
    show_examples(exA or exB)
