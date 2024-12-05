import java.util.Scanner;

public class Survey 
{
	Scanner scan = new Scanner(System.in);
	String answer = "";
	
	
	String [] knowledge_base =	{"- Birds have wings", 
					"- Birds fly", "- Tweety is bird"};
	
	String [] new_information =	{"- Penguins are birds"};
	String [] conclusion =		{"- Penguins fly"};
	
	int [] rank_revision = new int[3];
	int [] rank_human    = new int[3];
	
	
	public void print_knowledge_base()
	{
		System.out.println("Assume that you believe the following statements:");
		for(int i = 0; i < knowledge_base.length; i++)
		{
			System.out.println(knowledge_base[i]);
		}
		System.out.println();
	
	}
	
	public void collect_ranks()
	{
		System.out.println("New information: ");
		System.out.println(new_information[0]);
		System.out.println();
		System.out.println("Conclusion: ");
		System.out.println(conclusion[0]);
		System.out.println();

	}
	
	public void validateAnswer(String input)
	{
		if(!input.equals("Y") && !input.equals("N"))
		{
			System.err.println("Input not valid.");
			System.exit(0);
		}
		return;
	}
	
	public void revision()
	{
		
		System.out.println("Does the conclusion follow from your beliefs after learning the new information?");
		System.out.print("(Y/N) ");
		answer = scan.nextLine();
		validateAnswer(answer);
		System.out.println();
	}
	
	public void check()
	{
		System.out.println("Result:");
		System.out.println("Contradiction / No contradiction detected.");
		System.out.println("Your reasoning does not align / aligns with AGM belief revision.");
	}
	
	public static void main(String [] args)
	{
		Survey obj = new Survey();
		System.out.println();
		System.out.println("ROUND 1");
		System.out.println();
		obj.print_knowledge_base();
		obj.collect_ranks();
		obj.revision();
		obj.check();
		System.exit(0);
	}	
}
