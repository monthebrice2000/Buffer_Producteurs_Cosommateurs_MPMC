package com.tontonlaforce;

public class Producer {

    Thread producer;

    public Producer() {
        this.producer = new Thread();
    }

    public Thread getProducer() {
        return producer;
    }

    public void setProducer(Thread producer) {
        this.producer = producer;
    }
}
