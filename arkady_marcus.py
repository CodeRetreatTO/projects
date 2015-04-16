def island_find(bitmap):
    unvisited = [(x, y) for x in range(len(bitmap[0])) for y in range(len(bitmap))]
    count = 0
    while (len(unvisited) > 0):
        source = unvisited.pop(0)
        if not source:
            continue
        else:
            # We're dealing with land
            island = dfs(source, unvisited, bitmap)
            count += 1
    return count

def dfs(source, unvisited, bitmap):
    stack = [source]
    while len(stack) > 0:
        currentNode = stack.pop()
        # remove currentNode from unvisited
        print currentNode, get_neighbors(currentNode, unvisited)
        if currentNode in unvisited:
            unvisited.pop(unvisited.index(currentNode))
        neighbors = get_neighbors(currentNode, unvisited)
        for neighbor in neighbors:
            if not neighbor in stack:
                stack.append(neighbor)

def get_neighbors(source, unvisited):
    neighbors = ((source[0] - 1, source[1] - 1), \
                (source[0] - 1, source[1]), \
                (source[0] - 1, source[1] + 1), \
                (source[0], source[1] - 1),  \
                (source[0], source[1] + 1), \
                (source[0] + 1, source[1] - 1), \
                (source[0] + 1, source[1]),  \
                (source[0] + 1, source[1] + 1))

    filtered = [neighbor for neighbor in neighbors if neighbor in unvisited]

    return filtered 

if __name__ == "__main__":
    bitmap = [[1, 1, 0], [1, 1, 0], [1, 1, 0]]
    print island_find(bitmap)
    bitmap = [[1, 0, 1], [1, 0, 1], [1, 0, 1]]
    print island_find(bitmap)
