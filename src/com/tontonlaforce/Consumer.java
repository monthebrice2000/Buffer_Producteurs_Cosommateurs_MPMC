package com.tontonlaforce;

public class Consumer {

    Thread consumer;

    public Consumer() {
        this.consumer = new Thread();
    }

    public Thread getConsumer() {
        return consumer;
    }

    public void setConsumer(Thread consumer) {
        this.consumer = consumer;
    }
}
