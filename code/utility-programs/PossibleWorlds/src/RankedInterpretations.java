// author: ChatGPT
// modified by: Clayton Kevin Baker
import java.util.ArrayList;
import java.util.List;

public class RankedInterpretations {
	
	static int countPartitions = 0;
	
	// Function to generate all partitions
	static void partition(List<String> set, int index, List<List<String>> ans) {
		// If we have considered all elements
		// in the set, print the partition
		if (index == set.size()) {
			printPartition(ans);
			return;
		}

		// For each subset in the partition,
		// add the current element to it and recall
		for (int i = 0; i < ans.size(); i++) {
			ans.get(i).add(set.get(index));
			partition(set, index + 1, ans);
			ans.get(i).remove(ans.get(i).size() - 1);
		}

		// Add the current element as a singleton subset and recall
		List<String> newSubset = new ArrayList<>();
		newSubset.add(set.get(index));
		ans.add(newSubset);
		partition(set, index + 1, ans);
		ans.remove(ans.size() - 1);
	}
	
	static void partitionArrangements(List<String> set, int index, List<List<String>> ans) 
	{
		// If we have considered all elements
		// in the set, print the partition
		
		if (index == set.size()) {
			printPartitionArrangements(ans);
			return;
		}

		// For each subset in the partition,
		// add the current element to it and recall
		for (int i = 0; i < ans.size(); i++) {
			ans.get(i).add(set.get(index));
			partitionArrangements(set, index + 1, ans);
			ans.get(i).remove(ans.get(i).size() - 1);
		}

		// Add the current element as a singleton subset and recall
		List<String> newSubset = new ArrayList<>();
		newSubset.add(set.get(index));
		ans.add(newSubset);
		partitionArrangements(set, index + 1, ans);
		ans.remove(ans.size() - 1);
	}

	// Function to generate all partitions for a given set
	static void allPartitions(List<String> set) 
	{
		List<List<String>> partitions = new ArrayList<>();
		partition(set, 0, partitions);
		System.out.println();
		System.out.println("Total ranked interpretations: " + countPartitions);
		System.out.println("------------------------------------------------------------");
		System.out.println();
	}
	
	// Function to generate all arrangements of partitions for a given set
	static void allPartitionArrangements(List<String> set) 
	{
		List<List<String>> partitions = new ArrayList<>();
		partitionArrangements(set, 0, partitions);
		System.out.println();
		PossibleWorlds.getArrangements();
		
		
	}
	
	
	// Function to print a partition
	static void printPartition(List<List<String>> ans) 
	{
		//int subsetCount=0;
		for (List<String> subset : ans) 
		{
			//subsetCount = ans.size();
			System.out.print("[ ");
			
			for (String element : subset)
			{
				System.out.print(element + " ");
			}
			System.out.print("] ");
			
		}
		System.out.println();
		countPartitions++;
	}
	
	// function to print all arrangements of partitions
	static void printPartitionArrangements(List<List<String>> ans) 
	{

		
		PossibleWorlds obj = new PossibleWorlds();
		List<List<String>> listOfLists = new ArrayList<>();
		for(List<String> subset: ans)
		{
			listOfLists.add(subset);
		}
		List<List<List<String>>> arrangements = PossibleWorlds.generateListOfListsArrangements(listOfLists);
		obj.display(arrangements);

	}
}
// This code was contirbuted by codearcade
