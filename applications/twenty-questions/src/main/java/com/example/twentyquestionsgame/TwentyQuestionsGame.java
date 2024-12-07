package com.example.twentyquestionsgame;

import java.util.*;

public class TwentyQuestionsGame {
    List<GameObject> gameObjects;
    private List<String> questions;
    private Map<String, List<GameObject>> possibleObjects;

    public TwentyQuestionsGame() {
        this.gameObjects = new ArrayList<>();
        this.questions = new ArrayList<>();
        this.possibleObjects = new HashMap<>();

        // Define the objects and their properties
        setupObjects();
    }

    // Setup objects in the game
    private void setupObjects() {
        // Example objects (you can expand this)
        GameObject dog = new GameObject("Dog", "A domesticated carnivorous mammal.");
        dog.addAttribute("isAnimal", true);
        dog.addAttribute("hasFur", true);
        dog.addAttribute("canFly", false);

        GameObject bird = new GameObject("Bird", "A warm-blooded egg-laying vertebrate animal.");
        bird.addAttribute("isAnimal", true);
        bird.addAttribute("hasFur", false);
        bird.addAttribute("canFly", true);

        gameObjects.add(dog);
        gameObjects.add(bird);
    }

    // Generate questions based on properties
    private void generateQuestions() {
        Set<String> attributesSet = new HashSet<>();
        for (GameObject obj : gameObjects) {
            for (String key : obj.attributes.keySet()) {
                attributesSet.add(key);
            }
        }
        questions.addAll(attributesSet);
    }

    // Update possible objects based on a question response
    private void updatePossibleObjects(String question, boolean answer) {
        possibleObjects.clear();
        for (GameObject obj : gameObjects) {
            Boolean attributeValue = obj.getAttribute(question);
            if (attributeValue != null && attributeValue == answer) {
                possibleObjects.put(obj.getName(), Arrays.asList(obj));
            }
        }
    }

    // Get the next question to ask
    public String getNextQuestion() {
        if (questions.isEmpty()) {
            generateQuestions();
        }
        return questions.get(0);
    }

    // Make a guess
    public String makeGuess() {
        if (possibleObjects.size() == 1) {
            return possibleObjects.keySet().iterator().next();
        } else {
            return "I can't guess yet.";
        }
    }

    // Simulate a response to a question
    public void processAnswer(String question, boolean answer) {
        updatePossibleObjects(question, answer);
    }
}
