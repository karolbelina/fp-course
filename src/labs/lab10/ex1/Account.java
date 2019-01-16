package ex1;

public class Account {
    private int state;

    public Account(int startState) {
        state = startState;
    }

    public void setState(int value) {
        this.state = value;
    }

    public int getState() {
        return state;
    }
}