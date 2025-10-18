from collections import deque, namedtuple
import random, math

random.seed(13)

ALPHABET = ("a", "b")
BASE_RULES = (("aaa","bab"), ("bbb","aaa"))
EVENS = (4,6,8,10,12,14)
ODDS  = (3,5,7,9,11,13)

Limits = namedtuple("Limits", "depth nodes")
Gen = namedtuple("Gen", "word_min word_max step_min step_max")

def ev_rule(k):
    return ("b" + "a"*k, "a"*k + "b")

def od_rule(k):
    return ("bb" + "a"*k, "a"*k + "bb")

class TRS:
    def __init__(self, rules):
        self.rules = rules
    def step(self, w):
        out = set()
        for a, b in self.rules:
            i = 0
            n = len(a)
            while True:
                j = w.find(a, i)
                if j < 0: break
                out.add(w[:j] + b + w[j+n:])
                i = j + 1
        out.discard(w)
        return out

def systems():
    extra = tuple(ev_rule(k) for k in EVENS) + tuple(od_rule(k) for k in ODDS)
    return TRS(BASE_RULES), TRS(BASE_RULES + extra)

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

def bfs(sys, src, dst, lim):
    if src == dst:
        return True, [src], 0
    q = deque([src])
    seen = {src}
    parent = {src: None}
    depth = {src: 0}
    used = 0
    while q and used < lim.nodes:
        u = q.popleft()
        d = depth[u]
        if d >= lim.depth:
            continue
        for v in sys.step(u):
            if v in seen:
                continue
            seen.add(v)
            parent[v] = u
            depth[v] = d + 1
            used += 1
            if v == dst:
                out = [v]
                k = v
                while parent[k] is not None:
                    k = parent[k]
                    out.append(k)
                out.reverse()
                return True, out, used
            q.append(v)
    return False, [], used

def mean(a, n):
    return a / n if n else 0.0

def run(trials=3000, seed=13, gen=Gen(6,18,1,8), lim=Limits(60,2_000_000)):
    rng = random.Random(seed)
    T, TP = systems()
    ok = fail = 0
    ex = []
    sL = 0
    Lmin = math.inf
    Lmax = 0
    sS = 0
    Smin = math.inf
    Smax = 0
    zS = 0
    sP = 0
    Pmin = math.inf
    Pmax = 0
    exp_sum = 0
    for _ in range(trials):
        w0 = rand_word(gen.word_min, gen.word_max)
        L0 = len(w0)
        sL += L0
        Lmin = min(Lmin, L0)
        Lmax = max(Lmax, L0)
        chain = walk(T, w0, gen.step_min, gen.step_max)
        k = len(chain) - 1
        sS += k
        Smin = min(Smin, k)
        Smax = max(Smax, k)
        if k == 0: zS += 1
        w1 = chain[-1]
        hit, path, expanded = bfs(TP, w0, w1, lim)
        exp_sum += expanded
        if hit:
            ok += 1
            p = len(path) - 1
            sP += p
            Pmin = min(Pmin, p)
            Pmax = max(Pmax, p)
        else:
            fail += 1
            if len(ex) < 10:
                ex.append({"w0": w0, "w1": w1, "chain": chain, "found": path})
    summary = {
        "trials": trials,
        "ok": ok,
        "fail": fail,
        "ok_rate": ok / trials if trials else 0.0,
        "word_len": {"avg": mean(sL, trials), "min": 0 if Lmin is math.inf else Lmin, "max": Lmax},
        "chain_steps": {"avg": mean(sS, trials), "min": 0 if Smin is math.inf else Smin, "max": Smax, "stuck": zS, "stuck_rate": zS / trials if trials else 0.0},
        "path_steps_on_hit": {"avg": mean(sP, ok), "min": 0 if Pmin is math.inf else Pmin, "max": Pmax},
        "bfs_expanded_avg": mean(exp_sum, trials)
    }
    return summary, ex

def show(summary, examples, cap=5, max_len=140):
    print("== Fuzz ==")
    print(f"trials: {summary['trials']}")
    print(f"OK={summary['ok']} FAIL={summary['fail']} rate={summary['ok_rate']*100:.2f}%")
    wl = summary["word_len"]
    print(f"word length: avg={wl['avg']:.2f}, min={wl['min']}, max={wl['max']}")
    cs = summary["chain_steps"]
    print(f"chain steps: avg={cs['avg']:.2f}, min={cs['min']}, max={cs['max']}, stuck={cs['stuck']} ({cs['stuck_rate']*100:.2f}%)")
    ps = summary["path_steps_on_hit"]
    print(f"path steps on hit: avg={ps['avg']:.2f}, min={ps['min']}, max={ps['max']}")
    print(f"bfs expanded avg: {summary['bfs_expanded_avg']:.2f}")
    if not examples:
        print("\nNo failures.")
        return
    print("\nFailures:")
    for i, e in enumerate(examples[:cap], 1):
        c = " -> ".join(e["chain"])
        if len(c) > max_len:
            c = c[:max_len-3] + "..."
        p = " -> ".join(e["found"]) if e["found"] else "NONE"
        if len(p) > max_len:
            p = p[:max_len-3] + "..."
        print(f"#{i} w0={e['w0']!r} w1={e['w1']!r}")
        print(f"chain: {c}")
        print(f"path:  {p}")

if __name__ == "__main__":
    summary, examples = run()
    show(summary, examples)
