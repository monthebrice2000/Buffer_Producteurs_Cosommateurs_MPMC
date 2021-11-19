package prodcons;

import prodcons.v1.ProdConsBuffer;

public class Consumer implements Runnable{
    ProdConsBuffer prodConsBuffer;
    String name;

    public Consumer( String name, ProdConsBuffer prodConsBuffer) {
        this.prodConsBuffer = prodConsBuffer;
        this.name = name;
        System.out.println( "Initialisation consommateur "+ this.name );
    }

    @Override
    public void run() {
        Thread.yield();
        while ( this.prodConsBuffer.msgs() == 0 ){
            System.out.println( "le consommateur " + this.name + " est en attente de consommation de message");
            Thread.yield();
        }

        try {
            Message message = this.prodConsBuffer.get();
            System.out.println( "le consommateur " + this.name + " a consommé le message" + message.toString() );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println( "Après consommation"+ this.prodConsBuffer.toString() );
        Thread.yield();
    }
}
