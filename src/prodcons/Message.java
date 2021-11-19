package prodcons;

public class Message {

    private String thread_producer;

    public Message(String thread_producer) {
        this.thread_producer = thread_producer;
    }

    public String getThread_producer() {
        return thread_producer;
    }

    public void setThread_producer(String thread_producer) {
        this.thread_producer = thread_producer;
    }

    @Override
    public String toString() {
        return "Creator Of Message {" +
                "Producer=" + thread_producer +
                '}';
    }
}
