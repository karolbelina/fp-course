package ex2a;

class IntCell {
  private int n = 0;
  private boolean safeToRead = true;

  public synchronized int getN() {
    while(!safeToRead) {
      try {
        wait();
      } catch(InterruptedException ignore) {
      }
    }

    safeToRead = false;
    notifyAll();
    return n;
  }

  public synchronized void setN(int n) {
    while(safeToRead) {
      try {
        wait();
      } catch(InterruptedException ignore) {
      }
    }

    safeToRead = true;
    notifyAll();
    this.n = n;
  }
}