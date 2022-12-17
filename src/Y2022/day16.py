from collections import defaultdict

def dfs(valve, graph, visited, max_pressure):
    visited.add(valve)
    for neighbor in graph[valve]:
        if neighbor not in visited:
            max_pressure =  max_pressure * graph[valve][neighbor]
            max_pressure = dfs(neighbor, graph, visited, max_pressure)
    return max_pressure

def maximum_pressure(valve, graph):
    visited = set()
    max_pressure = 1
    return dfs(valve, graph, visited, max_pressure)

# Create a graph to represent the valves and tunnels
graph = defaultdict(dict)
graph['AA']['DD'] = 0
graph['AA']['II'] = 0
graph['AA']['BB'] = 0
graph['BB']['CC'] = 13
graph['BB']['AA'] = 13
graph['CC']['DD'] = 2
graph['CC']['BB'] = 2
graph['DD']['CC'] = 20
graph['DD']['AA'] = 20
graph['DD']['EE'] = 20
graph['EE']['FF'] = 3
graph['EE']['DD'] = 3
graph['FF']['EE'] = 0
graph['FF']['GG'] = 0
graph['GG']['FF'] = 0
graph['GG']['HH'] = 0
graph['HH']['GG'] = 22
graph['II']['AA'] = 0
graph['II']['JJ'] = 0
graph['JJ']['II'] = 21

# Find the maximum pressure released
valve = 'AA'
print(maximum_pressure(valve, graph))
