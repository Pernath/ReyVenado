import java.util.*;
class Prueba{
    
    public static int proofflazy(int i) {
	//int x = wrongar()
	int x = 2/0;
	
	if(i == 0)
	    return 2+3;
	if(i == 1)
	    return x;
	else
	    return 42;
    }
    public static void main(String args[]){
	try {
        System.out.println(proofflazy(0));
	} catch(Exception e) {e.printStackTrace();}
	
        try {
        System.out.println(proofflazy(0));
	} catch(Exception e) {e.printStackTrace();}

	try {
	    System.out.println(proofflazy(33));
	} catch(Exception e) {e.printStackTrace();}

    }

}
