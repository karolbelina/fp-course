import java.util.ArrayList;

public class ArrayQueue<E> implements MyQueue<E> {
    private int f;
    private int r;
    private ArrayList<E> arr;

    public ArrayQueue(int n) {
        f = 0;
        r = 0;
        arr = new ArrayList<E>(n + 1);

        for(int i = 0; i < n + 1; i++) {
            arr.add(null);
        }
    }

    @Override
    public void enqueue(E x) throws FullException {
        if(isFull()) {
            throw new FullException();
        }

        arr.set(r, x);
        r = (r + 1) % arr.size();
    }

    @Override
    public void dequeue() {
        if(!isEmpty()) {
            f = (f + 1) % arr.size();
        }
    }

    @Override
    public E first() throws EmptyException {
        if(isEmpty()) {
            throw new EmptyException();
        }

        return arr.get(f);
    }

    @Override
    public boolean isEmpty() {
        return f == r;
    }

    @Override
    public boolean isFull() {
        return (r + 1) % arr.size() == f;
    }
}