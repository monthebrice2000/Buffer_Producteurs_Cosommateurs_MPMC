package prodcons.v1;

import prodcons.IProdConsBuffer;
import prodcons.Message;

import java.util.Arrays;
import java.util.concurrent.Semaphore;

public class ProdConsBuffer implements IProdConsBuffer {

    private int bufferSz;
    private Message buffer[];
    Semaphore notFull;
    Semaphore notEmpty;
    Semaphore mutex;
    int in = 0;
    int out = 0;
    /**
     * retourne le nombre de message actuel dans le buffer
     */
    int tomsg = 0;

    /**
     * retourne le nombre total de message entrés dans le buffer
     */
    int totmsg = 0;

    public ProdConsBuffer(int bufferSz) {
        this.bufferSz = bufferSz;
        buffer = new Message[ bufferSz ];
        notFull = new Semaphore( bufferSz );
        notEmpty = new Semaphore( 0 );
        mutex = new Semaphore( 1 );
    }

    @Override
    public synchronized void put(Message m) throws InterruptedException {
//        if( this.in == this.out ){
//            return ;
//        }
        /**
         * incrémente le nombre de message dans le buffer avec
         * le nouveau message arrivé
         */
        this.tomsg++;
        /**
         * Incrémente le nombre totale de message
         */
        this.totmsg++;
        this.buffer[ this.in ] = m;
        this.in = ( this.in + 1 ) % this.bufferSz ;
    }

    @Override
    public synchronized Message get() throws InterruptedException {
        /**
         * Décrémenter le nombre de message dans le buffer
         */
        this.tomsg--;
        Message m = this.buffer[ this.out ];
        this.buffer[ this.out ] = null;
        this.out = ( this.out + 1 ) % this.bufferSz ;
        return m;
    }

    @Override
    public int msgs() {
        return this.tomsg;
    }

    @Override
    public int totmsg() {
        return this.totmsg;
    }

    public int getIn() {
        return in;
    }

    public void setIn(int in) {
        this.in = in;
    }

    public int getOut() {
        return out;
    }

    public void setOut(int out) {
        this.out = out;
    }

    @Override
    public String toString() {
        return "ProdConsBuffer{" +
                "buffer=" + Arrays.toString(buffer) +
                '}';
    }
}
