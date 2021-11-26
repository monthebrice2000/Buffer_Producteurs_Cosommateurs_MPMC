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

        try {
            Message message = this.prodConsBuffer.get();
            System.out.println( "le consommateur " + this.name + " a consommé le message " + message.toString() );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
