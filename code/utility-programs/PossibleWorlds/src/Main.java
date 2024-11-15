import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

public class Main 
{
	
	public static void main(String[] args)
	{

		try
		{
			// declaration of variables and objects
			List<String> propositions = new ArrayList<>(); 
			List<String> set = new ArrayList<>();
			File f = new File("output.txt");
			PrintStream ps = new PrintStream(f);
			System.setOut(ps);
			
			
			// initialise list of propositions
			propositions.add("p");
			propositions.add("b");
			//propositions.add("w");
			//propositions.add("f");
			//propositions.add("R");
		
	
			// display propositions
			System.out.println("Propositions");
			System.out.println();
			for(String x : propositions)
			{
				System.out.println(x + " ");
			}
			System.out.println();
			System.out.println("Total propositions: " + propositions.size());
			System.out.println("------------------------------------------------------------");
			
			// generate interpretations
			System.out.println();
			System.out.println("Interpretations");
			System.out.println();
			List<List<Boolean>> interpretations = 
			Interpretations.generateInterpretations(propositions);
			
			// write interpretations to file 
			String out;
			for (List<Boolean> interpretation : interpretations) 
			{
				out = "";
				for(Boolean val: interpretation)
				{
					if(val == true)
					{
						out += "T";
						//System.out.print("T");
					}
					else
					{	
						out += "F";
						//System.out.print("F");
					}
				}
				set.add(out);
				System.out.println(out);
			}
			System.out.println();
			Interpretations obj = new Interpretations();
			System.out.println("Total interpretations: " + obj.getInterpretations());
			System.out.println("------------------------------------------------------------");
			System.out.println();
			
			// generate all ranked intepretations and write to file
			
			System.out.println("Ranked interpretations ");
			System.out.println();
			RankedInterpretations.allPartitions(set);
		

			// generate and all arrangements of partitions of the set and write to file
			System.out.println("Possible worlds");
			System.out.println();
			RankedInterpretations.allPartitionArrangements(set);
			System.out.println("------------------------------------------------------------");
			
		}
		catch(IOException e)
		{
			System.err.println(e.getMessage());
			System.exit(0);
		}
		

	}


}
