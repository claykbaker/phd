// author: ChatGPT

/*
This program defines a 'generateInterpretations' method that takes a list of propositions and returns a list of all possible interpretations (assignments of truth values) to those propositions. 

It uses a recursive helper method, 'generateInterpretationsHelper', to explore all possible combinations of truth values for each proposition. Finally, it prints out each interpretation in a human-readable format.

You can modify the 'propositions' list to include the propositions you want to generate interpretations for. The program will then generate and print all possible interpretations for those propositions.
*/

import java.util.ArrayList;
import java.util.List;

public class Interpretations 
{

    static int countInterpretations=0;
    
    public static List<List<Boolean>> generateInterpretations(List<String> propositions) 
    {
        List<List<Boolean>> interpretations = new ArrayList<>();
        generateInterpretationsHelper(propositions, 0, new ArrayList<>(), interpretations);
        return interpretations;
    }

    private static void generateInterpretationsHelper(List<String> propositions, int index,
                                                     List<Boolean> currentInterpretation,
                                                     List<List<Boolean>> interpretations) 
    {
        if (index == propositions.size()) {
            // All propositions have been assigned truth values, add this interpretation
            interpretations.add(new ArrayList<>(currentInterpretation));
	    countInterpretations++;
            return;
        }

        // Generate interpretations with the current proposition set to true
        currentInterpretation.add(true);
        generateInterpretationsHelper(propositions, index + 1, currentInterpretation, interpretations);
        currentInterpretation.remove(currentInterpretation.size() - 1); // Backtrack

        // Generate interpretations with the current proposition set to false
        currentInterpretation.add(false);
        generateInterpretationsHelper(propositions, index + 1, currentInterpretation, interpretations);
        currentInterpretation.remove(currentInterpretation.size() - 1); // Backtrack
    }
    
    public int getInterpretations()
    {
	    return countInterpretations;
    }
}
