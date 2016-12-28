package org.genericsystem.quiz.app;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.model.UserAnswer;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.security.model.User;

public class QuizUtils {

	public static class SAVE_QUIZ_RESULT implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {

			Generic quiz = context.getGeneric();
			Generic sUser = context.find(User.class).getInstance("Anti-Seche");
			Generic loggedUser = tag.getLoggedUserProperty(context).getValue();

			context.flush();

			ScoreUtils.setResult(context, quiz, sUser, loggedUser);

			ScoreUtils.getResult(context, quiz, loggedUser);
			System.out.println("Scores -> ");
			ScoreUtils.getScores(context, quiz).forEach(System.out::println);

			System.out.println("Note de " + loggedUser + " -> " + ScoreUtils.calculateSimpleGrade(context, quiz, loggedUser) + " / 20");

		}

	}

	public static class ScoreUtils {

		private static int calculateResult(Context context, Generic quiz, Generic sUser, Generic loggedUser) {

			Snapshot<Generic> questions = quiz.getHolders(context.find(Question.class));
			List<Generic> listQuestions = questions.stream().collect(Collectors.toList());

			System.out.println("**************************************************************************");
			System.out.println("Quiz " + quiz);
			System.out.println();

			int total = 0;

			for (Generic question : listQuestions) {

				List<Generic> listAnswers = (question.getHolders(context.find(Answer.class))).stream().collect(Collectors.toList());

				int pointByQuestion = 0;

				System.out.println(question);

				for (Generic answer : listAnswers) {
					Generic goodAnswer = answer.getLink(context.find(UserAnswer.class), sUser);
					Generic userAnswer = answer.getLink(context.find(UserAnswer.class), loggedUser);

					System.out.println("\t" + answer);
					System.out.println("\tGood answer -> \t" + goodAnswer.getValue());
					System.out.println("\t" + loggedUser + "'s answer -> \t" + userAnswer.getValue());

					pointByQuestion = (goodAnswer.getValue().equals(userAnswer.getValue())) ? ++pointByQuestion : --pointByQuestion;

				}

				System.out.println(pointByQuestion + " points sur " + listAnswers.size() + " possibles.\n");

				if (pointByQuestion == listAnswers.size())
					++total;

			}
			System.out.println("Résultat du test : " + total + " bonne(s) réponse(s)");
			System.out.println("Précédent résultat : " + quiz.getLink(context.find(ScoreUserQuiz.class), loggedUser));

			return total;
		}

		public static int getResult(Context context, Generic quiz, Generic user) {
			return (int) quiz.getLink(context.find(ScoreUserQuiz.class), user).getValue();
		}

		public static void setResult(Context context, Generic quiz, int result, Generic user) {
			quiz.setLink(context.find(ScoreUserQuiz.class), result, user);
			context.flush();
		}

		public static void setResult(Context context, Generic quiz, Generic sUser, Generic loggedUser) {
			setResult(context, quiz, calculateResult(context, quiz, sUser, loggedUser), loggedUser);
		}

		// Retourne un snapshot de tous les résultats pour un quiz donné
		public static Snapshot<Generic> getResults(Context context, Generic quiz) {
			return quiz.getHolders(context.find(ScoreUserQuiz.class));
		}

		// Retourne le résultat moyen de bonnes réponses pour un quiz donné
		public static Double getAverageResult(Context context, Generic quiz) {
			Snapshot<Generic> results = getResults(context, quiz);
			return results.stream().mapToInt(result -> (Integer) result.getValue()).average().getAsDouble();
		}

		// Rate = taux de bonnes réponses (entre 0 et 1)
		public static Double getRate(Context context, Generic quiz, double result) {
			Snapshot<Generic> questions = quiz.getHolders(context.find(Question.class));
			return getRate(questions.size(), result);
		}

		public static Double getRate(double xmax, double result) {
			return xmax != 0 ? (Double) (result / xmax) : null;
		}

		// Retourne le taux moyen de bonnes réponses pour un quiz donné (Entre 0 et 1)
		public static Double getAverageRate(Context context, Generic quiz) {
			return getAverageRate(getAverageResult(context, quiz), quiz.getHolders(context.find(Question.class)).size());
		}

		public static Double getAverageRate(double averageResult, int nQuestion) {
			return averageResult / nQuestion;
		}

		// Retourne un score (sur 100) par rapport à la moyenne des résultats
		public static double getScore(double userRate, double averageRate) {
			return (userRate - averageRate) * 100;
		}

		public static double getScore(Context context, Generic quiz, Generic user) {
			return getScore(getRate(context, quiz, getResult(context, quiz, user)), getAverageRate(context, quiz));
		}

		// Renvoie un Stream de Double trié contenant tous les scores pour un quiz donné
		public static Stream<Double> getScores(Context context, Generic quiz) {
			return getResults(context, quiz).stream().map(result -> getScore(getRate(context, quiz, (int) result.getValue()), getAverageRate(context, quiz))).sorted();
		}

		public static Double getAverageScore(Context context, Generic quiz) {
			return getAverageScore(getScores(context, quiz));
		}

		public static Double getAverageScore(Stream<Double> numberStream) {
			return numberStream.mapToDouble(score -> (Double) score).average().getAsDouble();
		}

		public static Double getAverageScore(Double averageRate) {
			return averageRate * 100;
		}

		// Calcul de la note sur 20 en fonction des résultats précédents
		public static Double calculateSimpleGrade(Context context, Generic quiz, Generic user) {
			List<Double> scores = getScores(context, quiz).collect(Collectors.toList());

			Double scoreMin = scores.get(0);
			Double scoreMax = scores.get(scores.size() - 1);
			Double average = getAverageScore(getScores(context, quiz));

			System.out.println("Score minimal = " + scoreMin);
			System.out.println("Score max = " + scoreMax);
			System.out.println("Score moyen = " + average);

			// Définir l'equation y = ax + b... y est le score du User
			Double y = getScore(context, quiz, user);

			// Point de la droite pour x = 0 (quand y = average) -> calcul de b
			Double b = average;
			Double a = null;

			if (Math.abs(scoreMax) >= Math.abs(scoreMin)) {
				a = getA(scoreMax, b, 10d);
				System.out.println("a = aMax");
			} else {
				a = getA(scoreMin, b, -10d);
				System.out.println("a = aMin");
			}

			System.out.println("Formule :\n\ty = " + a + " * x + " + b);

			return calculateSimpleGrade(a, y, b);
		}

		public static Double calculateSimpleGrade(Double a, Double y, Double b) {
			return ((y - b) / a) + 10;
		}

		public static Double getA(Double yMax, Double b, Double yRef) {
			return (yMax - b) / yRef;
		}

	}
}
