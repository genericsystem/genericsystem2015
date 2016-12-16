package org.genericsystem.quiz.app;

import java.util.List;
import java.util.stream.Collectors;

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

			saveScore(context, tag, quiz, sUser, loggedUser);

			context.flush();

		}

		private int getScore(Context context, Tag tag, Generic quiz, Generic sUser, Generic loggedUser) {

			Snapshot<Generic> questions = quiz.getHolders(context.find(Question.class));
			List<Generic> listQuestions = questions.stream().collect(Collectors.toList());

			System.out.println("**************************************************************************");
			System.out.println("Quiz " + quiz);
			System.out.println();

			int totalScore = 0;

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
					++totalScore;

			}
			System.out.println("Résultat du test : " + totalScore + " bonne(s) réponse(s)");

			System.out.println("Précédent résultat : " + quiz.getLink(context.find(ScoreUserQuiz.class), loggedUser));

			return totalScore;
		}

		private void saveScore(Context context, Tag tag, Generic quiz, Generic sUser, Generic loggedUser) {

			quiz.setLink(context.find(ScoreUserQuiz.class), getScore(context, tag, quiz, sUser, loggedUser), loggedUser);
			context.flush();

		}
	}

}
