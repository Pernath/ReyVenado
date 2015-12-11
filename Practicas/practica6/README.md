####Instrucciones  
	Para correr el programa escribir en la consola:
	>python Main.py <Archivo>
	donde <Archivo> es el nombre del archivo con formato json, csv o xml con la grafica a leer

####Formato de impresión
	 Se imprime la grafica leida con el siguiente formato:
	 Archivo: <Archivo> *Nombre del archivo

	 Dirigida?: = <False o True> *Es o no dirigida la gráfica

	 Vertices: {<vertices>} *Lista de vertices

	 Aristas:
	     <vertice origen> <--<peso>--> <vertice destino> *Si la grafica no es dirigida
	     <vertice origen> --<peso>--> <vertice destino> *Si la grafica es dirigida
	     
	 Ciclos? = <False o True> *Tiene o no ciclos la grafica

####Usar Python 2