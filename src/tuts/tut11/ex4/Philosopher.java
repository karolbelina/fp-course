package ex4;

import java.util.Random;
import java.util.concurrent.Semaphore;

public class Philosopher extends Thread {
  private final Stick left, right;
  private final Semaphore doorman;
  private static final Random random = new Random();

  Philosopher(int number, int numberOfPhilosophers, Stick[] sticks, Semaphore doorman) {
    super("philosopher " + number);
    right = sticks[number];
    left = sticks[(number + 1) % numberOfPhilosophers];
    this.doorman = doorman;
  }

  void meditate() throws InterruptedException {
    System.out.println(getName() + " is meditating");
    sleep(random.nextInt(10000));

    System.out.println(getName() + " has finished meditating");
    eat();
  }

  void eat() throws InterruptedException {
    doorman.acquire();

    left.take();
    right.take();

    System.out.println(getName() + " is eating");
    sleep(random.nextInt(10000));

    left.release();
    right.release();

    doorman.release();

    meditate();
  }

  @Override
  public void run() {
    try {
      meditate();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}