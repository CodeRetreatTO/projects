def read_pic(fname):
    with open(fname, "r") as f:
        return f.read()

def index_file(contents):
    num = 0
    res = {}
    for char in contents:
        if char != '\n' and not char in res:
            res[char] = num
            num += 1
    return res

def process_line(score_table, ln):
    res = []
    for (i, c) in enumerate(ln):
        s = score_table[c]
        total = 0
        if i > 0:
            total += abs(s - score_table[ln[i-1]])
        if (i+1) < len(ln):
            total += abs(s - score_table[ln[i+1]])
        res.append(total)
    return res

def process_image(score_table, lines):
    pxScores = [process_line(score_table, ln) for ln in lines]
    res = []
    for (i, px) in enumerate(pxScores[0]):
        res.append((i, sum([ln[i] for ln in pxScores])))
    return sorted(res, key=lambda e: e[1])

def scale_image(fname, by):
    pic = read_pic(fname)
    lines = pic.split("\n")[0:-1]
    score_table = index_file(pic)
    cols = [p[0] for p in process_image(score_table, lines)[0:by]]
    for ln in lines:
        for (i, char) in enumerate(ln):
            if not i in cols:
                print char,
        print ""
