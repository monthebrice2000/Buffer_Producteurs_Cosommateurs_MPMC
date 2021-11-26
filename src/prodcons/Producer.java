package prodcons;


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
        try {
            System.out.println("Le Producteur " + message + " a produit le message ");
            this.prodConsBuffer.put( message );
            //System.out.println("Le Producteur " + message + " a produit le message ");
            //System.out.println( "après production" + this.prodConsBuffer.toString() );
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.out.println( e + " +++++++++++++");
        }
    }
}
