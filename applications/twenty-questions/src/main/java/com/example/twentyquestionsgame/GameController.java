package com.example.twentyquestionsgame;

import javafx.fxml.FXML;
import javafx.scene.control.Label;

public class GameController {
    private TwentyQuestionsGame game;

    @FXML
    private Label questionLabel;

    public GameController() {
        this.game = new TwentyQuestionsGame();
        System.out.println("Starting Game");
        System.out.println("Loading FXML...");

    }

    // This method is triggered when the 'Next Question' button is clicked
    @FXML
    private void handleNextQuestion() {
        String nextQuestion = game.getNextQuestion();
        questionLabel.setText(nextQuestion);
    }
}

