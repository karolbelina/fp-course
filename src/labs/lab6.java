import java.util.Arrays;

public class lab6 {
	// ex 1
	// insert an element to an already sorted array
    public static int[] insert(int x, int[] xs) {
        int n = xs.length;
        int i = 0;
        
        while(i < n && x > xs[i]) {
            i++;
        }
        
        int[] retVal = new int[n + 1];
        System.arraycopy(xs, 0, retVal, 0, i);
        retVal[i] = x;
        System.arraycopy(xs, i, retVal, i + 1, n - i);
        
        return retVal;
    }
    
    public static void main(String[] args) {
        System.out.println(Arrays.toString(insert(4, new int[]{1, 3, 5, 7})));
        System.out.println(Arrays.toString(insert(1, new int[]{})));
        System.out.println(Arrays.toString(insert(100, new int[]{2, 4, 6, 20, 40})));
    }
}