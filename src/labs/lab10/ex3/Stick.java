package ex3;

import java.util.concurrent.Semaphore;

public class Stick {
    private final Semaphore available = new Semaphore(1);

    void take() throws InterruptedException {
        available.acquire();
    }

    void release() {
        available.release();
    }
}