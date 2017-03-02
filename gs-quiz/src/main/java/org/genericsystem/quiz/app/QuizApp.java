package org.genericsystem.quiz.app;

import java.io.File;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.quiz.app.QuizApp.QuizzScript;
import org.genericsystem.quiz.app.pages.AppPage;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Description;
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
@DependsOnModel({ Quiz.class, Question.class, Answer.class, User.class, UserAnswer.class, ScoreUserQuiz.class, Description.class })
@Children(AppPage.class)
@Style(name = "background-color", value = "#F5F5F5")
public class QuizApp extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, QuizApp.class, File.separator + "QuizzApp");
	}

	public QuizApp() {
		createNewInitializedProperty(QuizTagSwitcher.PAGE, c -> QuizTagSwitcher.HOME_PAGE);
	}

	public static class QuizzScript implements Script {

		@Override
		public void run(Root engine) {

			// Create user (sUser est l'utilisateur qui possède toutes les bonnes réponses)
			Generic user = engine.find(User.class);
			Generic sUser = user.setInstance("Anti-Seche");

			// Create Quiz
			Generic quiz = engine.find(Quiz.class);
			Generic quizTest = quiz.setInstance("Petit Test");
			Generic quiz2 = quiz.setInstance("Quiz n°2");

			// Create Description for each Quiz
			Generic description = engine.find(Description.class);
			quizTest.setHolder(description, "<em>Testez vos connaissances en JEE...</em>");
			quiz2.setHolder(description, "<em>Blind Test !</em>");

			// Create Questions (Question.class is a component of Quiz.class)
			Generic question = engine.find(Question.class);
			Generic q01 = quizTest.setHolder(question, "Quel est le résultat de la séquence : <em><br>ArrayList<String> mots ;<br>mots.add('azer') ;</em>");
			Generic q02 = quizTest.setHolder(question, "Portée <br>des attributs : laquelle de ces affirmations est vraie ?");
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

			// QUIZ SUR JAVA
			Generic qJava = quiz.setInstance("Quiz Java");
			qJava.setHolder(description, "<em>Testez vos connaissances en Java<br>" + "Attention, chaque question peut contenir plusieurs bonnes réponses !</em>");

			Generic jQ01 = qJava.setInstance("Parmi les modificateurs d'accès suivants, le(s)quel(s) permet(tent) un accès depuis le package ?");
			Generic aJQ0101 = jQ01.setHolder(answer, "public");
			Generic aJQ0102 = jQ01.setHolder(answer, "private");
			Generic aJQ0103 = jQ01.setHolder(answer, "protected");
			Generic aJQ0104 = jQ01.setHolder(answer, "modificateur vide (valeur par défaut, aucun mot-clé)");
			aJQ0101.setLink(userAnswer, true, sUser);
			aJQ0102.setLink(userAnswer, false, sUser);
			aJQ0103.setLink(userAnswer, true, sUser);
			aJQ0104.setLink(userAnswer, true, sUser);

			Generic jQ02 = qJava.setInstance("Cocher les affirmations justes :");
			Generic ajQ0201 = jQ02.setHolder(answer, "Une classe abstraite doit contenir des méthodes abstraites");
			Generic ajQ0202 = jQ02.setHolder(answer, "Depuis Java 8, une méthode abstraite peut posséder un corps");
			Generic ajQ0203 = jQ02.setHolder(answer, "Une classe abstraite ne peut être instanciée que si elle est déclarés <em>public</em>");
			Generic ajQ0204 = jQ02.setHolder(answer, "Une méthode abstraite ne peut exister que dans une classe abstraite");
			ajQ0201.setLink(userAnswer, false, sUser);
			ajQ0202.setLink(userAnswer, false, sUser);
			ajQ0203.setLink(userAnswer, false, sUser);
			ajQ0204.setLink(userAnswer, true, sUser);

			Generic jQ03 = qJava.setInstance("Le polymorphisme en java sert à :");
			Generic ajQ0301 = jQ03.setHolder(answer, "Rendre abstraite une classe concrète");
			Generic ajQ0302 = jQ03.setHolder(answer, "Se passer des interfaces");
			Generic ajQ0303 = jQ03.setHolder(answer, "Standardiser les relations entre objets de nature distincte");
			Generic ajQ0304 = jQ03.setHolder(answer, answerD);
			ajQ0301.setLink(userAnswer, false, sUser);
			ajQ0302.setLink(userAnswer, false, sUser);
			ajQ0303.setLink(userAnswer, true, sUser);
			ajQ0304.setLink(userAnswer, false, sUser);

			Generic jQ04 = qJava.setInstance("Quelle(s) interface(s) étend(ent) <em>Collection</em>");
			Generic ajQ0401 = jQ04.setHolder(answer, "Set, Queue");
			Generic ajQ0402 = jQ04.setHolder(answer, "HashMap, List");
			Generic ajQ0403 = jQ04.setHolder(answer, "Map");
			Generic ajQ0404 = jQ04.setHolder(answer, answerD);
			ajQ0401.setLink(userAnswer, true, sUser);
			ajQ0402.setLink(userAnswer, false, sUser);
			ajQ0403.setLink(userAnswer, false, sUser);
			ajQ0404.setLink(userAnswer, false, sUser);

			Generic jQ05 = qJava.setInstance("Cocher les affirmations justes :");
			Generic ajQ0501 = jQ05.setHolder(answer, "Les <em>List</em> autorisent la présence de doublon");
			Generic ajQ0502 = jQ05.setHolder(answer, "Les <em>Set</em> autorisent la présence de doublon");
			Generic ajQ0503 = jQ05.setHolder(answer, "Les <em>List</em> ont besoin des méthodes <em>equals()</em> et <em>hashCode()</em> pour retrouver un élément dans la <em>List</em>");
			Generic ajQ0504 = jQ05.setHolder(answer, "Les <em>Map</em> ont besoin des méthodes <em>equals()</em> et <em>hashCode()</em> pour retrouver un élément dans la <em>Map</em>");
			ajQ0501.setLink(userAnswer, true, sUser);
			ajQ0502.setLink(userAnswer, false, sUser);
			ajQ0503.setLink(userAnswer, false, sUser);
			ajQ0504.setLink(userAnswer, true, sUser);

			Generic jQ06 = qJava.setInstance("Une méthode <em>static</em> :");
			Generic ajQ0601 = jQ06.setHolder(answer, "Peut être appelée sans créer d'instance de la classe dans laquelle elle est contenue");
			Generic ajQ0602 = jQ06.setHolder(answer, "Doit être contenue dans une classe static");
			Generic ajQ0603 = jQ06.setHolder(answer, "Ne peut effectuer d'opérations que sur des variables <em>static</em>");
			Generic ajQ0604 = jQ06.setHolder(answer, answerD);
			ajQ0601.setLink(userAnswer, true, sUser);
			ajQ0602.setLink(userAnswer, false, sUser);
			ajQ0603.setLink(userAnswer, false, sUser);
			ajQ0604.setLink(userAnswer, false, sUser);

			Generic jQ07 = qJava.setInstance("Quelle(s) différence(s) y a-t-il entre <em>HashMap</em> et <em>HashTable</em> ?");
			Generic ajQ0701 = jQ07.setHolder(answer, "<em>HashTable</em> est synchronisé, <em>HashMap</em> ne l'est pas");
			Generic ajQ0702 = jQ07.setHolder(answer, "<em>HashMap</em> implémente l'interface <em>Map</em>, <em>HashTable</em> implémente l'interface <em>Set</em>");
			Generic ajQ0703 = jQ07.setHolder(answer, "<em>HashMap</em> autorise la présence de valeur <em>null</em>, <em>HashTable</em> ne l'autorise pas");
			Generic ajQ0704 = jQ07.setHolder(answer, answerD);
			ajQ0701.setLink(userAnswer, true, sUser);
			ajQ0702.setLink(userAnswer, false, sUser);
			ajQ0703.setLink(userAnswer, true, sUser);
			ajQ0704.setLink(userAnswer, false, sUser);

			Generic jQ08 = qJava.setInstance("Java est un langage :");
			Generic ajQ0801 = jQ08.setHolder(answer, "Interprété");
			Generic ajQ0802 = jQ08.setHolder(answer, "Compilé");
			Generic ajQ0803 = jQ08.setHolder(answer, answerD);
			ajQ0801.setLink(userAnswer, true, sUser);
			ajQ0802.setLink(userAnswer, true, sUser);
			ajQ0803.setLink(userAnswer, false, sUser);

			Generic jQ09 = qJava.setInstance("L'<em>ArrayList</em> :");
			Generic ajQ0901 = jQ09.setHolder(answer, "Peut contenir des types primitifs et des objets");
			Generic ajQ0902 = jQ09.setHolder(answer, "Ne peut contenir que des objets");
			Generic ajQ0903 = jQ09.setHolder(answer, "Possède une taille dynamique");
			Generic ajQ0904 = jQ09.setHolder(answer, "Possède une taille fixe définie lors de l'instanciation");
			ajQ0901.setLink(userAnswer, false, sUser);
			ajQ0902.setLink(userAnswer, true, sUser);
			ajQ0903.setLink(userAnswer, true, sUser);
			ajQ0904.setLink(userAnswer, false, sUser);

			engine.getCurrentCache().flush();
		}

	}
}
