public class Main {
  public static void main(String[] args) {
    try {
      ArrayQueue<Integer> q = new ArrayQueue<>(3);
      q.enqueue(1);
      q.enqueue(2);
      q.enqueue(3);
      System.out.println(q.first());
      q.dequeue();
      q.dequeue();
      System.out.println(q.first());
    }
    catch(FullException e) {
      System.out.println("the queue is full");
    }
    catch(EmptyException e) {
      System.out.println("the queue is empty");
    }
  }
}