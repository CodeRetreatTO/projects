from collections import defaultdict
import copy
from collections import deque

quilt = [[3, 5, 5],
         [3, 4, 2],
         [1, 1, 2]]

quilt2 = [[1, 2],[1, 3]]

quilt3 = [[1, 2, 3],
          [1, 4, 3]]

def connections(quilt):
    """Return all possible connections"""
    res = defaultdict(set)
    rows = len(quilt)
    cols = len(quilt[0])
    for row_num, row in enumerate(quilt):
        for col_num, cell in enumerate(row):
            if row_num < rows - 1:
                res[cell].add(quilt[row_num + 1][col_num])
            if row_num > 0:
                res[cell].add(quilt[row_num - 1][col_num])
            if col_num < cols - 1:
                res[cell].add(quilt[row_num][col_num + 1])
            if col_num > 0:
                res[cell].add(quilt[row_num][col_num - 1])

    for key in res.keys():
        if key in res[key]:
            res[key].remove(key)

    return res

def stitch(quilt, a, b):

    new_quilt = [[col if col != b else a for col in row] for row in quilt]
    print("merging {} into {}".format(b, a))
    print("previous and new", quilt, new_quilt)
    return new_quilt

def sew(quilt):
    """return a list of quilts and quantity of seams
    with one fewer patches after sewing"""
    conns = connections(quilt)
    res = []
    for origin, neighbors in conns.items():
        for neighbor in neighbors:
            res.append(stitch(quilt, origin, neighbor))

    return res

def done(quilt):
    tester = quilt[0][0]
    for row in quilt:
        for cell in row:
            if cell != tester:
                return False

    return True


def find_solutions(quilt):
    """Return least number of seams to sew quilt"""
    quilts = deque()
    quilts.append((quilt, 0))
    while len(quilts) > 0:
        this_quilt, count = quilts.popleft()
        print(this_quilt, count)
        if done(this_quilt):
            return count
        quilts_to_add = sew(quilt)
        for quilt in quilts_to_add:
            quilts.append((quilt, count + 1))

# print(find_solutions(quilt))
# print(find_solutions(quilt2))
print(find_solutions(quilt3))