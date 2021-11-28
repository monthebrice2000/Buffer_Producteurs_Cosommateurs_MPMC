package com.tontonlaforce;

import prodcons.Consumer;
import prodcons.Producer;
import prodcons.v1.ProdConsBuffer;

public class Main {


    public static void main(String[] args) throws InterruptedException {
        System.out.println(" Nombre de processeurs : "+ Runtime.getRuntime().availableProcessors() );
        Operation operation = new Operation( 5);
        ProdConsBuffer prodConsBuffer = new ProdConsBuffer( 2, operation );

        Thread[] producersThreads = new Thread[5];
        Thread[] consumersThreads = new Thread[2];

        for( int i = 0; i< consumersThreads.length; i++){
            consumersThreads[i] = new Thread(new Consumer( "thread "+ i, prodConsBuffer, operation), "thread " + i);
            consumersThreads[i].start();
        }

        for( int i = 0; i< producersThreads.length; i++){
            producersThreads[i] = new Thread(new Producer( "thread "+ i, prodConsBuffer , operation ), "thread " + i );
            producersThreads[i].start();
        }


    }
}
