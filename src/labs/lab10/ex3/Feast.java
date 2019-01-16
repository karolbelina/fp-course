package ex3;

import java.util.Random;
import java.util.concurrent.Semaphore;

public class Feast {
    private static final int numberOfPhilosophers = 5;
    private static final Random random = new Random();
    private static final Semaphore doorman = new Semaphore(4, true);
    private static final Stick[] sticks = new Stick[5];

    public static void main(String[] args) {
        for(int i = 0; i < 5; i++) {
            sticks[i] = new Stick();
        }

        Philosopher[] philosophers = new Philosopher[numberOfPhilosophers];

        for(int i = 0; i < 5; i++) {
            philosophers[i] = new Philosopher(i, numberOfPhilosophers, sticks, doorman, random);
            philosophers[i].start();
        }
    }
}