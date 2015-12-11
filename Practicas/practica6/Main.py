from GraphReader import *
import sys

class Main:

    f = sys.argv[1]
    form = f.split(".")[1]
    gr = GraphReader()
    print "Archivo: " + f + "\n"
    print gr.graph(f,form)
