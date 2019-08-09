package ex2;

public class Main {
  public static void main(String[] args) throws InterruptedException {
    final int startState = 1000;

    Account account = new Account(startState);
    Withdrawer withdrawer = new Withdrawer(account, 20000);
    Depositor depositor = new Depositor(account, 20000);

    withdrawer.start();
    depositor.start();

    withdrawer.join();
    depositor.join();

    System.out.println("money deposited: " + depositor.getTotalDepositedMoney());
    System.out.println("money withdrawed: " + withdrawer.getTotalWithdrawedMoney());
    System.out.println("expected state of the account: " + (startState + depositor.getTotalDepositedMoney() - withdrawer
        .getTotalWithdrawedMoney()));
    System.out.println("final state of the account: " + account.getState());
  }
}