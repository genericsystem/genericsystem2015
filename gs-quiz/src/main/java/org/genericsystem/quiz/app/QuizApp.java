package org.genericsystem.quiz.app;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.quiz.app.QuizApp.QuizzScript;
import org.genericsystem.quiz.app.pages.AppPage;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.model.UserAnswer;
import org.genericsystem.quiz.utils.QuizTagSwitcher;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.security.model.User;

@RunScript(QuizzScript.class)
@DependsOnModel({ Quiz.class, Question.class, Answer.class, User.class, UserAnswer.class, ScoreUserQuiz.class })
@Children(AppPage.class)
// @Children({ HomePage.class, QuizPage.class, ResultPage.class })
@Style(name = "background-color", value = "grey")
public class QuizApp extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, QuizApp.class, "/QuizzApp");
	}

	public QuizApp() {
		createNewInitializedProperty(QuizTagSwitcher.PAGE, c -> QuizTagSwitcher.HOME_PAGE);

		// createNewInitializedProperty("HomePage", c -> true);
		// createNewInitializedProperty("QuestionPage", c -> false);
		// createNewInitializedProperty("ResultPage", c -> false);
		// createNewInitializedProperty("QuizDone", context -> false);
	}

	public static class QuizzScript implements Script {

		@Override
		public void run(Root engine) {
			// Create user
			Generic user = engine.find(User.class);
			Generic robert = user.setInstance("Robert DJ");
			Generic sUser = user.setInstance("Anti-Seche");

			// Create Quiz
			Generic quiz = engine.find(Quiz.class);
			Generic quizTest = quiz.setInstance("Petit Test");
			Generic quiz2 = quiz.setInstance("Quiz n°2");

			// Create Questions (Question.class is a component of Quiz.class)
			Generic question = engine.find(Question.class);
			Generic q01 = quizTest.setHolder(question, "Quel est le résultat de la séquence : \nArrayList<String> mots ;\nmots.add('azer') ;");
			Generic q02 = quizTest.setHolder(question, "Portée des attributs : laquelle de ces affirmations est vraie ?");
			// quizTest.setHolder(question, "laquelle de ces affirmations est vraie ?");
			Generic q03 = quiz2.setHolder(question, "Question 1 !");
			Generic q04 = quiz2.setHolder(question, "Question 2 !");
			Generic q05 = quiz2.setHolder(question, "Question 3 !");
			Generic q06 = quiz2.setHolder(question, "Question 4 !");
			Generic q07 = quiz2.setHolder(question, "Question 5 !");
			Generic q08 = quiz2.setHolder(question, "Question 6 !");

			// Create Answers (Answer.class is a component of Question.class)
			String answerD = "Aucune de ces réponses";
			Generic answer = engine.find(Answer.class);
			Generic answer0101 = q01.setHolder(answer, "la chaine 'azer' est ajoutée à la liste");
			Generic answer0102 = q01.setHolder(answer, "un ArrayOutOfBoundsException");
			Generic answer0103 = q01.setHolder(answer, "un NullPointerException");
			Generic answer0104 = q01.setHolder(answer, answerD);
			Generic answer0201 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe");
			Generic answer0202 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles seulement dans les méthodes déclarées après l'attribut");
			Generic answer0203 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe seulement si leur visibilité est public");
			Generic answer0204 = q02.setHolder(answer, answerD);

			Generic answer0301 = q03.setHolder(answer, "Answer 01");
			Generic answer0302 = q03.setHolder(answer, "Answer 02");
			Generic answer0303 = q03.setHolder(answer, "Answer 03");
			Generic answer0304 = q03.setHolder(answer, answerD);
			Generic answer0401 = q04.setHolder(answer, "Answer 01");
			Generic answer0402 = q04.setHolder(answer, "Answer 02");
			Generic answer0403 = q04.setHolder(answer, "Answer 03");
			Generic answer0404 = q04.setHolder(answer, answerD);
			Generic answer0501 = q05.setHolder(answer, "Answer 01");
			Generic answer0502 = q05.setHolder(answer, "Answer 02");
			Generic answer0503 = q05.setHolder(answer, "Answer 03");
			Generic answer0504 = q05.setHolder(answer, answerD);
			Generic answer0601 = q06.setHolder(answer, "Answer 01");
			Generic answer0602 = q06.setHolder(answer, "Answer 02");
			Generic answer0603 = q06.setHolder(answer, "Answer 03");
			Generic answer0604 = q06.setHolder(answer, answerD);
			Generic answer0701 = q07.setHolder(answer, "Answer 01");
			Generic answer0702 = q07.setHolder(answer, "Answer 02");
			Generic answer0703 = q07.setHolder(answer, "Answer 03");
			Generic answer0704 = q07.setHolder(answer, answerD);
			Generic answer0801 = q08.setHolder(answer, "Answer 01");
			Generic answer0802 = q08.setHolder(answer, "Answer 02");
			Generic answer0803 = q08.setHolder(answer, "Answer 03");
			Generic answer0804 = q08.setHolder(answer, answerD);

			// Create UserAnswer
			Generic userAnswer = engine.find(UserAnswer.class);
			answer0101.setLink(userAnswer, true, robert);
			answer0102.setLink(userAnswer, false, robert);
			answer0103.setLink(userAnswer, true, robert);
			answer0104.setLink(userAnswer, false, robert);
			answer0201.setLink(userAnswer, true, robert);
			answer0202.setLink(userAnswer, false, robert);
			answer0203.setLink(userAnswer, false, robert);
			answer0204.setLink(userAnswer, false, robert);

			answer0101.setLink(userAnswer, false, sUser);
			answer0102.setLink(userAnswer, true, sUser);
			answer0103.setLink(userAnswer, false, sUser);
			answer0104.setLink(userAnswer, false, sUser);
			answer0201.setLink(userAnswer, true, sUser);
			answer0202.setLink(userAnswer, false, sUser);
			answer0203.setLink(userAnswer, false, sUser);
			answer0204.setLink(userAnswer, false, sUser);

			answer0301.setLink(userAnswer, false, sUser);
			answer0302.setLink(userAnswer, true, sUser);
			answer0303.setLink(userAnswer, false, sUser);
			answer0304.setLink(userAnswer, false, sUser);
			answer0401.setLink(userAnswer, true, sUser);
			answer0402.setLink(userAnswer, false, sUser);
			answer0403.setLink(userAnswer, false, sUser);
			answer0404.setLink(userAnswer, false, sUser);
			answer0501.setLink(userAnswer, true, sUser);
			answer0502.setLink(userAnswer, false, sUser);
			answer0503.setLink(userAnswer, false, sUser);
			answer0504.setLink(userAnswer, false, sUser);
			answer0601.setLink(userAnswer, false, sUser);
			answer0602.setLink(userAnswer, true, sUser);
			answer0603.setLink(userAnswer, false, sUser);
			answer0604.setLink(userAnswer, false, sUser);
			answer0701.setLink(userAnswer, true, sUser);
			answer0702.setLink(userAnswer, false, sUser);
			answer0703.setLink(userAnswer, false, sUser);
			answer0704.setLink(userAnswer, false, sUser);
			answer0801.setLink(userAnswer, true, sUser);
			answer0802.setLink(userAnswer, false, sUser);
			answer0803.setLink(userAnswer, false, sUser);
			answer0804.setLink(userAnswer, false, sUser);

			engine.getCurrentCache().flush();
		}

	}
}
