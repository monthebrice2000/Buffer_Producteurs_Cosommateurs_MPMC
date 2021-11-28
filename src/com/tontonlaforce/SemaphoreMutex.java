package com.tontonlaforce;

public class SemaphoreMutex {
    int maxResource;

    public SemaphoreMutex(int maxResource) {
        this.maxResource = maxResource;
    }

    public synchronized void P() throws InterruptedException {
        while ( this.maxResource == 0){
            wait();
        }
        this.maxResource--;
    }

    public synchronized void V(){
        this.maxResource++;
        notifyAll();

    }
}
