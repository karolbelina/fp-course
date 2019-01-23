package ex2b;

import java.util.concurrent.Semaphore;

class Count extends Thread {
    static IntCell n = new IntCell();
    private static Semaphore semaphore = new Semaphore(1);

    @Override
    public void run() {
        int temp;
        for(int i = 0; i < 200000; i++) {
            try {
                semaphore.acquire();
            } catch(InterruptedException ignore) {
            }

            temp = n.getN();
            n.setN(temp + 1);
            semaphore.release();
        }
    }

    public static void main(String[] args) {
        Count p = new Count();
        Count q = new Count();

        p.start();
        q.start();

        try {
            p.join();
            q.join();
        } catch(InterruptedException ignore) {
        }

        System.out.println("The value of n is " + n.getN());
    }
}