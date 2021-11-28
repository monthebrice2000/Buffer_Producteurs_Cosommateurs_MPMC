package com.tontonlaforce;

public class SemaphoreConsommateur {
    private int maxRessource;
    //private Operation operation;

    public SemaphoreConsommateur( ) {

        this.maxRessource = 0;
        //this.operation = operation;
    }

    public synchronized void P() throws InterruptedException {
        while ( this.maxRessource == 0 ){
//            if( this.operation.getNb_producer() ){
//                throw new InterruptedException("Il n' y' a plus de producteurs ");
//            }
            wait();
        }
        this.maxRessource--;
    }

    public synchronized void V(){
        this.maxRessource++;
        notifyAll();

    }
}
