package prodcons;


import com.tontonlaforce.Main;
import com.tontonlaforce.Operation;
import prodcons.v1.ProdConsBuffer;

public class Producer implements Runnable{

    Message message;
    ProdConsBuffer prodConsBuffer;
    Operation operation;

    public Producer( String name, ProdConsBuffer prodConsBuffer, Operation operation) {
        this.message = new Message( name );
        this.prodConsBuffer = prodConsBuffer;
        this.operation = operation;
        System.out.println( "Initialisation " + message.toString());
    }

    @Override
    public void run() {
        try {

            this.prodConsBuffer.put( message );
            System.out.println("Le Producteur " + message + " a produit le message ");
            this.operation.decrement();
            //System.out.println( "après production" + this.prodConsBuffer.toString() );
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.out.println( e + " +++++++++++++");
        }
    }
}
