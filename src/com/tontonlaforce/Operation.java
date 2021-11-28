package com.tontonlaforce;

public class Operation {

    private int nb_producer;

    public Operation(int nb_producer) {
        this.nb_producer = nb_producer;
    }

    synchronized public void decrement(){
        this.nb_producer--;
    }

    synchronized public boolean getNb_producer(){
        return this.nb_producer == 0;
    }
}
