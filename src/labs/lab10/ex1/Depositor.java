package ex1;

import java.util.Random;

public class Depositor extends Thread {
    private int totalDepositedMoney = 0;
    private final Account account;
    private long duration;

    public Depositor(Account account, long duration) {
        this.account = account;
        this.duration = duration;
    }

    @Override
    public void run() {
        Random random = new Random();
        long startTime = System.currentTimeMillis();

        while(System.currentTimeMillis() - startTime < duration) {
            int accountState = account.getState();

            try {
                Thread.sleep(random.nextInt(2000));
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }

            account.setState(accountState + 100);
            totalDepositedMoney += 100;
            System.out.println("Current state of the account: " + account.getState());

            try {
                Thread.sleep(random.nextInt(2000) + 2000);
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

    }

    public int getTotalDepositedMoney() {
        return totalDepositedMoney;
    }
}