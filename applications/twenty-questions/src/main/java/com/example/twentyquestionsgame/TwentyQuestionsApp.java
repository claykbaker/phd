package com.example.twentyquestionsgame;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.*;

public class TwentyQuestionsApp extends Application {
    private TwentyQuestionsGame game;
    private Label questionLabel;
    private TextField answerField;
    private Button submitButton;
    private Label feedbackLabel;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        System.out.println("Starting Game");
        System.out.println("Loading FXML...");

        game = new TwentyQuestionsGame();

        // Initialize the UI elements
        questionLabel = new Label("Let's start! Please ask your first question.");
        answerField = new TextField();
        submitButton = new Button("Submit Answer");
        feedbackLabel = new Label();

        // Set up event handling for the submit button
        submitButton.setOnAction(e -> handleSubmitAnswer());

        // Layout setup
        VBox layout = new VBox(10);
        layout.getChildren().addAll(questionLabel, answerField, submitButton, feedbackLabel);

        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/com/example/twentyquestionsgame/twentyquestions.fxml"));

        // Setup Scene
        Scene scene = new Scene(layout, 400, 300);
        primaryStage.setTitle("20 Questions Game");
        primaryStage.setScene(scene);
        primaryStage.show();

        // Start the game
        nextQuestion();
    }

    private void nextQuestion() {
        String question = game.getNextQuestion();
        questionLabel.setText(question);
    }

    private void handleSubmitAnswer() {
        String answer = answerField.getText().toLowerCase();
        boolean response = answer.equals("yes");

        // Process the answer and update the game state
        game.processAnswer(game.getNextQuestion(), response);
        feedbackLabel.setText("You answered: " + (response ? "Yes" : "No"));

        // Show next question or make a guess
        String guess = game.makeGuess();
        if (guess.equals("I can't guess yet.")) {
            nextQuestion();
        } else {
            questionLabel.setText("I guess it's: " + guess);
        }
    }
}
