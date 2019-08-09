package ex2;

import java.util.concurrent.Semaphore;

public class Account {
  private int state;
  private Semaphore lock = new Semaphore(1);

  public Account(int startState) {
    state = startState;
  }

  public void setState(int value) {
    this.state = value;
  }

  public int getState() {
    return state;
  }

  public void lock() {
    try {
      lock.acquire();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  public void unlock() {
    lock.release();
  }
}