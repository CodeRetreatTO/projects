import itertools

def small_test():
    q = {
        "edges": [("a", "b"), ("a", "c"), ("b", "c")],
        "vertices": ["a", "b", "c"]
    }
    q["edges"] = list(map(set, q["edges"]))
    return q

def test_quilt():
    q = {
        "edges": [
            ("a", "b"), ("a", "c"),
            ("b", "d"), ("b", "e"),
            ("c", "d"), ("c", "h"), ("c", "f"), ("c", "g"),
            ("d", "h"), ("d", "e"),
            ("e", "h"),
            ("f", "g"),
            ("g", "h")
        ],
    "vertices": ["a", "b", "c", "d", "e", "f", "g", "h"]
    }
    q["edges"] = list(map(set, q["edges"]))
    return q

def _replace_vertex_in_edge(edge, old_v, new_v):
    res = edge.copy()
    res.remove(old_v)
    res.add(new_v)
    return res

def _new_edges(edges, v1, v2, new_v):
    res = []
    for edge in edges:
        if v1 in edge and v2 in edge:
            pass
        elif v1 in edge:
            new_edge = _replace_vertex_in_edge(edge, v1, new_v)
            if new_edge not in res:
                res.append(new_edge)
        elif v2 in edge:
            new_edge = _replace_vertex_in_edge(edge, v2, new_v)
            if new_edge not in res:
                res.append(new_edge)
        else:
            res.append(edge)
    return res

def combine_vertices(quilt_graph, v1, v2):
    new_v_name = v1 + v2
    filtered_vertices = [v for v in quilt_graph["vertices"] if v not in [v1, v2]]
    res = {
        "edges" : _new_edges(quilt_graph["edges"], v1, v2, new_v_name),
        "vertices" : [new_v_name] + filtered_vertices
    }
    return res

def compute_seams(quilt_graph, steps = None):
    if len(quilt_graph["vertices"]) == 1:
        return steps
    else:
        comb = itertools.combinations(quilt_graph["vertices"], 2)
        for (v1, v2) in comb:
            if not None:
                s = [(v1, v2)]
            else:
                s = steps + [(v1, v2)]
            return compute_seams(combine_vertices(quilt_graph, v1, v1), s)

def main():
    compute_seams(small_test())
