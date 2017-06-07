import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

/**
 * Created by ezugonwosu on 4/12/17.
 */
public class ReservoirSampler {
    private static final String TAG = ReservoirSampler.class.getSimpleName();

    public static void main(String[] args) {
        // Get the data file from the computer
        File sampleFile = new File("/Users/ezugonwosu/Desktop/NudgeNew.txt");

        // Generate an input stream
        InputStream in = null;

        // Read the data file as an input stream
        try {
            in = new FileInputStream(sampleFile);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));

        // Create an array list to store the data (from the file)
        List<String> fileContent = new ArrayList<String>();
        String line = null;
        try {
            while ((line = reader.readLine()) != null) {
                fileContent.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        Iterator it = fileContent.iterator();
        // Debug statement
        // System.out.print("contents of fileContent: " + fileContent.get(0));

        int n = fileContent.size();
        int k = 20;

        // Call the function that will perform the reservoir sampling
        selectKItems(fileContent, n, k);
    }

    // A function to randomly select k items from stream[0..n-1].
    static void selectKItems(List theFileContent, int n, int k)
    {
        Random rand = new Random();
        // Used to generate a new line to separate content
        String newLine = System.getProperty("line.separator");

        int i; // index for elements in stream[]
        // reservoir[] is the output Arraylist.
        List<String> reservoir = new ArrayList<String>();

        // Initialize it with first k elements from theFileContent[]
        for (i = 0; i < k; i++){
            reservoir.add((String) theFileContent.get(i));
        }

        // Print the initial 20 elements in the reservoir
        System.out.print("==========================================Initial 20 Elements in the Reservoir==========================================");
        System.out.println(newLine);
        for(String line : reservoir) {
            System.out.println(line);
        }

        // Iterate from the (k+1)th element to nth element
        for (i=k+1; i < n; i++){
            // Pick a random index from 0 to i where i is index of current item in stream[].
            int j = rand.nextInt(i);

            // If the randomly picked index is smaller than k, then replace the element present at
            // the index with new element from stream
            if (j < k){
                reservoir.set(j, (String) theFileContent.get(i));
            }
        }

        // Print the final 20 elements in the reservoir after the reservoir sampling technique has been applied
        System.out.println(newLine);
        System.out.print("==========================================Final 20 Elements in the Reservoir==========================================");
        System.out.println(newLine);
        for(String newReservioir : reservoir) {
            System.out.println(newReservioir);
        }
    }
}