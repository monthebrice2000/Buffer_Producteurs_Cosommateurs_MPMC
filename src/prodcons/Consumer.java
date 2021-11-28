package prodcons;

import com.tontonlaforce.Operation;
import prodcons.v1.ProdConsBuffer;

public class Consumer implements Runnable{
    ProdConsBuffer prodConsBuffer;
    String name;
    Operation operation;

    public Consumer( String name, ProdConsBuffer prodConsBuffer, Operation operation) {
        this.prodConsBuffer = prodConsBuffer;
        this.name = name;
        this.operation = operation;
        System.out.println( "Initialisation consommateur "+ this.name );
    }

    @Override
    public void run() {

        try {
            while( true ){
                System.out.println("le consommateur " + this.name+ " consomme a nouveau le message");
                Message message = this.prodConsBuffer.get();
                System.out.println( "le consommateur " + this.name + " a consommé le message " + message.toString() );
            }

        } catch (InterruptedException e) {
            //e.printStackTrace();
            System.out.println( "Consommateur "+ this.name + " dit que "+ e.getMessage() );
        }
        System.out.println("Consommateur "+ this.name +" terminé");
    }
}
