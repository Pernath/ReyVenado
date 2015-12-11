from Graph import *

class DGraph(Graph):
            
    def directed(self):
        return True

    def edges(self):
        edges = []
        for k,v in self.ver.iteritems():
            for e in v.edges:
                edges.append(e)
        return edges

    def conect(self, w, u, v):
        u = self.ver.get(u)
        v = self.ver.get(v)
        u.add_edge(w, u, v)

    def __str__(self):
        s = "Dirigida? = " + str(self.directed()) + "\n\nVertices: { "
        for k,v in self.ver.iteritems():
            s += str(v) + " "
        s += "}\n\nAristas:\n"
        for e in self.edges() :
            s += "  " + e.str(True) + "\n\n"
        s += "Ciclos? = " + str(self.has_cycles())
        return s
