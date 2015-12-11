from Graph import *
from DGraph import *
import xml.etree.ElementTree as ET
import csv
import json

class GraphReader:

    def graph(self,f, form):
        if form == "xml":
            return self.xml(f)
        elif form == "csv":
            return self.csv(f)
        return self.json(f)

    def xml(self, f):
        tree = ET.parse(f)
        root = tree.getroot()
        directed = int(root.attrib.get('direct'))
        g = Graph()
        if directed == 1:
            g = DGraph()
        for vertex in root.findall('vertex'):
            g.add_vertex(vertex.get('label'))        
        for edge in root.findall('edge'):
            g.conect(edge.get('weight'), edge.get('source'), edge.get('target'))
        return g

    def csv(self, f):
        reader = csv.reader(open(f), delimiter=",")
        directed = next(reader)[0]
        directed = int(directed[7:])        
        g = Graph()
        if directed == 1:
            g = DGraph()
        for u,v,w in reader:
            g.add_vertex(u)
            g.add_vertex(v[2:3])
            g.conect(w,u,v[2:3])
        return g

    def json(self, f):
        data = json.load(open(f))
        directed = data["direct"]
        g = Graph()
        if directed == 1:
            g = DGraph()
        for v in data["vertices"]:
            g.add_vertex(v)
        for e in data["edges"]:
            g.conect(e[2], e[0], e[1]) 
        return g
