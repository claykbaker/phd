// author: ChatGPT
// modified by: Clayton Kevin Baker
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PossibleWorlds 
{
    
	static int countPossibleWorlds = 0;
	
	public static void main(String[] args) 
	{
		List<List<String>> listOfLists = new ArrayList<>();
		listOfLists.add(Arrays.asList("TT"));
		listOfLists.add(Arrays.asList("TF"));
		listOfLists.add(Arrays.asList("FT"));
		listOfLists.add(Arrays.asList("FF"));

		List<List<List<String>>> arrangements = generateListOfListsArrangements(listOfLists);

		// Print all arrangements
		for (List<List<String>> arrangement : arrangements) 
		{
		    System.out.println(arrangement);
		}
	}
	
	public void display(List<List<List<String>>> input)
	{
		// Print all arrangements
		for (List<List<String>> arrangement : input) 
		{
		    System.out.println(arrangement);
		    countPossibleWorlds++;
		}   

	}
    
	static void getArrangements()
	{
		System.out.println("Total possible worlds: " + countPossibleWorlds);
	}

	public static List<List<List<String>>> generateListOfListsArrangements(List<List<String>> listOfLists) 
	{
		List<List<List<String>>> arrangements = new ArrayList<>();
		int size = listOfLists.size();

		// Generate all permutations of indices
		int[] indices = new int[size];
		for (int i = 0; i < size; i++) {
		    indices[i] = i;
		}

		do {
		    List<List<String>> arrangement = new ArrayList<>();
		    for (int index : indices) {
			arrangement.add(listOfLists.get(index));
		    }
		    arrangements.add(arrangement);
		} while (nextPermutation(indices));

		return arrangements;
	}

	public static boolean nextPermutation(int[] array) 
	{
		int i = array.length - 2;
		while (i >= 0 && array[i] >= array[i + 1]) {
		    i--;
		}
		if (i < 0) {
		    return false;
		}

		int j = array.length - 1;
		while (array[j] <= array[i]) {
		    j--;
		}

		int temp = array[i];
		array[i] = array[j];
		array[j] = temp;

		reverseArray(array, i + 1);
		return true;
	}

    public static void reverseArray(int[] array, int start) 
    {
        int i = start;
        int j = array.length - 1;
        while (i < j) 
	{
            int temp = array[i];
            array[i] = array[j];
            array[j] = temp;
            i++;
            j--;
        }
    }
}
