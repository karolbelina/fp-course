package ex1;

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

        System.out.println("Money deposited: " + depositor.getTotalDepositedMoney());
        System.out.println("Money withdrawed: " + withdrawer.getTotalWithdrawedMoney());
        System.out.println("Expected state of the account: " + (startState + depositor.getTotalDepositedMoney() - withdrawer
                .getTotalWithdrawedMoney()));
        System.out.println("Final state of the account: " + account.getState());
    }
}