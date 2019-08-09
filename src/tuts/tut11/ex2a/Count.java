package ex2a;

class Count extends Thread {
  private static IntCell n = new IntCell();

  @Override
  public void run() {
    int temp;

    for(int i = 0; i < 200000; i++) {
      temp = n.getN();
      n.setN(temp + 1);
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

    System.out.println("the value of n is " + n.getN());
  }
}