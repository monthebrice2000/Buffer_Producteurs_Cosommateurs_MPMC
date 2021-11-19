package com.tontonlaforce;

import prodcons.Consumer;
import prodcons.Producer;
import prodcons.v1.ProdConsBuffer;

public class Main {

    public static void main(String[] args) {
        ProdConsBuffer prodConsBuffer = new ProdConsBuffer( 3 );
        Thread[] producersThreads = new Thread[4];
        Thread[] consumersThreads = new Thread[4];

        for( int i = 0; i< consumersThreads.length; i++){
            consumersThreads[i] = new Thread(new Consumer( "thread "+ i, prodConsBuffer ), "thread " + i);
            consumersThreads[i].start();
        }

        for( int i = 0; i< producersThreads.length; i++){
            producersThreads[i] = new Thread(new Producer( "thread "+ i, prodConsBuffer ), "thread " + i);
            producersThreads[i].start();
        }


    }
}
