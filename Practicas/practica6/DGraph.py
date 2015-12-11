from Graph import *

"""
Class for a directed graph
"""
class DGraph(Graph):

    """
    Returns True if the graph is directed
    """
    def directed(self):
        return True

    """
    Returns a list with the edges of the graph
    """
    def edges(self):
        edges = []
        for k,v in self.ver.iteritems():
            for e in v.edges:
                edges.append(e)
        return edges

    """
    Connects vertices u and v with a w-weight edge
    """
    def connect(self, w, u, v):
        u = self.ver.get(u)
        v = self.ver.get(v)
        u.add_edge(w, u, v)

    """
    __str__ method for the graph
    """
    def __str__(self):
        s = "Dirigida? = " + str(self.directed()) + "\n\nVertices: { "
        for k,v in self.ver.iteritems():
            s += str(v) + " "
        s += "}\n\nAristas:\n"
        for e in self.edges() :
            s += "  " + e.str(True) + "\n\n"
        s += "Ciclos? = " + str(self.has_cycles())
        return s
