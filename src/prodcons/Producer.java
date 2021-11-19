package prodcons;

import org.omg.PortableServer.THREAD_POLICY_ID;
import prodcons.v1.ProdConsBuffer;

public class Producer implements Runnable{

    Message message;
    ProdConsBuffer prodConsBuffer;

    public Producer( String name, ProdConsBuffer prodConsBuffer) {
        this.message = new Message( name );
        this.prodConsBuffer = prodConsBuffer;
        System.out.println( "Initialisation " + message.toString());
    }

    @Override
    public void run() {
        Thread.yield();
        while ( this.prodConsBuffer.msgs() == 3 ){
            System.out.println( message.toString() + "est en attente pour la production");
            Thread.yield();
        }

        try {
            this.prodConsBuffer.put( message );
            System.out.println("Le Producteur " + message + " a produit le message ");
            System.out.println( "après production" + this.prodConsBuffer.toString() );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
