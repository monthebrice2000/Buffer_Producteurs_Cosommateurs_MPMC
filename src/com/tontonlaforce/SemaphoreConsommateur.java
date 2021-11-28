package com.tontonlaforce;

public class SemaphoreConsommateur {
    private int maxRessource;

    public SemaphoreConsommateur() {
        this.maxRessource = 0;
    }

    public synchronized void P() throws InterruptedException {
        while ( this.maxRessource == 0){
            wait();
        }
        this.maxRessource--;
    }

    public synchronized void V(){
        this.maxRessource++;
        notifyAll();

    }
}
