package prodcons.v1;

import com.tontonlaforce.Operation;
import prodcons.IProdConsBuffer;
import prodcons.Message;

import javax.management.monitor.Monitor;
import java.util.Arrays;
import java.util.concurrent.Semaphore;

public class ProdConsBuffer implements IProdConsBuffer {

    private int bufferSz;
    private Message buffer[];
    private static final Object monitorProd = new Object();
    private static final Object monitorCons = new Object();
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

    Operation operation;

    public ProdConsBuffer(int bufferSz, Operation operation) {
        this.bufferSz = bufferSz;
        buffer = new Message[ bufferSz ];
        notFull = new Semaphore( bufferSz );
        notEmpty = new Semaphore( 0 );
        mutex = new Semaphore( 1 );
        this.operation = operation;
    }

    @Override
    public void put(Message m) throws InterruptedException {
//        if( this.in == this.out ){
//            return ;
//        }
        synchronized ( monitorProd ){
            while ( this.tomsg == 2 ){
                System.out.println( "Le producteur " + m.toString() + " est en attente pour la production");
                monitorProd.wait();
            }
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
            //this.nb_producer--;
            monitorProd.notifyAll();
            synchronized ( monitorCons ){
                monitorCons.notifyAll();
            }
        }
        //System.out.println("Liberation du verrou coté producteur");

    }

    @Override
    public Message get() throws InterruptedException {

        synchronized ( monitorCons ){
            while( this.tomsg == 0 ){
                System.out.println( "le consommateur  est en attente de consommation de message");
                if( this.operation.getNb_producer() ){
                    throw new InterruptedException("Il n' y a plus de producteurs ");
                }
                monitorCons.wait();
            }
            /**
             * Décrémenter le nombre de message dans le buffer
             */
            this.tomsg--;
            Message m = this.buffer[ this.out ];
            this.buffer[ this.out ] = null;
            this.out = ( this.out + 1 ) % this.bufferSz ;
            synchronized (monitorProd){
                monitorProd.notifyAll();
                System.out.println("reveil des producateurs bloqués après " + m.toString() + " "+ this.tomsg + " "+ this.buffer.length);
            }
            monitorCons.notifyAll();
            return m;
        }

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

    public int getTomsg() {
        return tomsg;
    }

    @Override
    public String toString() {
        return "ProdConsBuffer{" +
                "buffer=" + Arrays.toString(buffer) +
                '}';
    }
}
