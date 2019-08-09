package ex2;

import java.util.Random;

public class Withdrawer extends Thread {
  private int totalWithdrawedMoney = 0;
  private final Account account;
  private long duration;

  public Withdrawer(Account account, long duration) {
    this.account = account;
    this.duration = duration;
  }

  @Override
  public void run() {
    Random random = new Random();
    long startTime = System.currentTimeMillis();

    while(System.currentTimeMillis() - startTime < duration) {
      account.lock();
      int accountState = account.getState();

      try {
        Thread.sleep(random.nextInt(2000));
      }
      catch (InterruptedException e) {
        e.printStackTrace();
      }

      account.setState(accountState - 100);
      totalWithdrawedMoney += 100;
      System.out.println("current state of the account: " + account.getState());
      account.unlock();

      try {
        Thread.sleep(random.nextInt(2000) + 2000);
      }
      catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }

  public int getTotalWithdrawedMoney() {
    return totalWithdrawedMoney;
  }
}