class Graph:
    
    class Vertex:
            
        def __init__(self, label):
            self.edges = []
            self.label = label
            self.visited = False
            
        def __str__(self):
            return str(self.label)
            
        def neighbours(self):
            n = []
            for e in self.edges:
                n.append(e.tvertex)
            return n

        def degree(self):
            return len(self.edges)   

        def add_edge(self, weight, svertex, tvertex):
            e = Graph.Edge(weight, svertex, tvertex)
            self.edges.append(e)
            
    class Edge:
        
        def __init__(self, weight, svertex, tvertex):
            self.weight = weight
            self.svertex = svertex
            self.tvertex = tvertex

        def str(self,bool):
            if bool:
                return str(self.svertex) + " --" + str(self.weight) + "--> " + str(self.tvertex)
            return str(self.svertex) + " <--" + str(self.weight) + "--> " + str(self.tvertex)
            
        def svertex(self):
            return self.svertex

        def tvertex(self):
            return self.tvertex
            
        def weight(self):
            return self.weight

    def __init__(self):
        self.ver = {}
    
    def directed(self):
        return False

    def vertices(self):
        l = []
        for k,v in self.ver.iteritems():
            l.append(v)
        return l

    def check_edges(self, l, u, v):
        for e in l:
            if e.tvertex is u and e.svertex is v:
                return True
        return False
                    
    def edges(self):
        edges = []
        for k,v in self.ver.iteritems():
            for e in v.edges:
                if not self.check_edges(edges, e.svertex, e.tvertex):
                    edges.append(e)
        return edges

    def add_vertex(self, label):
        if self.ver.get(label) is None:
            self.ver[label] = self.Vertex(label)
        
    def conect(self, w, u, v):
        u = self.ver.get(u)
        v = self.ver.get(v)
        u.add_edge(w, u, v)
        v.add_edge(w, v, u)

    def has_cycles(self):
        s = []
        v = self.ver[self.ver.keys()[0]]
        v.visited = True
        s.append(v)
        fv = v
        while len(s) > 0:
            v = s.pop()
            for e in v.edges:
                if e.tvertex.visited == False:
                    e.tvertex.visited = True
                    s.append(e.tvertex)
                else:
                    if e.tvertex is not fv:
                        return True
            fv = v
        return False
        
    def __str__(self):
        s = "Dirigida? = " + str(self.directed()) + "\n\nVertices: { "
        for k,v in self.ver.iteritems():
            s += str(v) + " "
        s += "}\n\nAristas:\n"
        for e in self.edges() :
            s += "  " + e.str(False) + "\n\n"
        s += "Ciclos? = " + str(self.has_cycles())
        return s
