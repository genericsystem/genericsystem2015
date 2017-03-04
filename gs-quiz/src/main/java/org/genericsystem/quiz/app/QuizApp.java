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
		ApplicationServer.startSimpleGenericApp(mainArgs, QuizApp.class, File.separator + "QuizApp");
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
			String tab = "&emsp;&emsp;";
			String tab2 = tab + tab;
			String tab3 = tab2 + tab;

			Generic qJava = quiz.setInstance("Quiz Java");
			qJava.setHolder(description, "<em>Testez vos connaissances en Java<br>" + "Attention, chaque question peut contenir plusieurs bonnes réponses !</em>");

			Generic jQ01 = qJava.setHolder(question, "Parmi les modificateurs d'accès suivants, le(s)quel(s) permet(tent) un accès depuis le package ?");
			Generic aJQ0101 = jQ01.setHolder(answer, "public");
			Generic aJQ0102 = jQ01.setHolder(answer, "private");
			Generic aJQ0103 = jQ01.setHolder(answer, "protected");
			Generic aJQ0104 = jQ01.setHolder(answer, "modificateur vide (valeur par défaut, aucun mot-clé)");
			aJQ0101.setLink(userAnswer, true, sUser);
			aJQ0102.setLink(userAnswer, false, sUser);
			aJQ0103.setLink(userAnswer, true, sUser);
			aJQ0104.setLink(userAnswer, true, sUser);

			Generic jQ02 = qJava.setHolder(question, "Cocher les affirmations justes : ");
			Generic ajQ0201 = jQ02.setHolder(answer, "Une classe abstraite doit contenir des méthodes abstraites");
			Generic ajQ0202 = jQ02.setHolder(answer, "Depuis Java 8, une méthode abstraite peut posséder un corps");
			Generic ajQ0203 = jQ02.setHolder(answer, "Une classe abstraite ne peut être instanciée que si elle est déclarée <em>public</em>");
			Generic ajQ0204 = jQ02.setHolder(answer, "Une méthode abstraite ne peut exister que dans une classe abstraite");
			ajQ0201.setLink(userAnswer, false, sUser);
			ajQ0202.setLink(userAnswer, false, sUser);
			ajQ0203.setLink(userAnswer, false, sUser);
			ajQ0204.setLink(userAnswer, true, sUser);

			Generic jQ03 = qJava.setHolder(question, "Le polymorphisme en java sert à :");
			Generic ajQ0301 = jQ03.setHolder(answer, "Rendre abstraite une classe concrète");
			Generic ajQ0302 = jQ03.setHolder(answer, "Se passer des interfaces");
			Generic ajQ0303 = jQ03.setHolder(answer, "Standardiser les relations entre objets de nature distincte");
			Generic ajQ0304 = jQ03.setHolder(answer, answerD);
			ajQ0301.setLink(userAnswer, false, sUser);
			ajQ0302.setLink(userAnswer, false, sUser);
			ajQ0303.setLink(userAnswer, true, sUser);
			ajQ0304.setLink(userAnswer, false, sUser);

			Generic jQ04 = qJava.setHolder(question, "Quelle(s) interface(s) étend(ent) <em>Collection</em>");
			Generic ajQ0401 = jQ04.setHolder(answer, "Set, Queue");
			Generic ajQ0402 = jQ04.setHolder(answer, "HashMap, List");
			Generic ajQ0403 = jQ04.setHolder(answer, "Map");
			Generic ajQ0404 = jQ04.setHolder(answer, answerD);
			ajQ0401.setLink(userAnswer, true, sUser);
			ajQ0402.setLink(userAnswer, false, sUser);
			ajQ0403.setLink(userAnswer, false, sUser);
			ajQ0404.setLink(userAnswer, false, sUser);

			Generic jQ05 = qJava.setHolder(question, "Cocher les affirmations justes :");
			Generic ajQ0501 = jQ05.setHolder(answer, "Les <i>List</i> autorisent la présence de doublon");
			Generic ajQ0502 = jQ05.setHolder(answer, "Les <i>Set</i> autorisent la présence de doublon");
			Generic ajQ0503 = jQ05.setHolder(answer, "Les <i>List</i> ont besoin des méthodes <i>equals()</i> et <i>hashCode()</i> pour retrouver un élément dans la <i>List</i>");
			Generic ajQ0504 = jQ05.setHolder(answer, "Les <i>Map</i> ont besoin des méthodes <i>equals()</i> et <i>hashCode()</i> pour retrouver un élément dans la <i>Map</i>");
			ajQ0501.setLink(userAnswer, true, sUser);
			ajQ0502.setLink(userAnswer, false, sUser);
			ajQ0503.setLink(userAnswer, false, sUser);
			ajQ0504.setLink(userAnswer, true, sUser);

			Generic jQ06 = qJava.setHolder(question, "Une méthode <em>static</em> :");
			Generic ajQ0601 = jQ06.setHolder(answer, "Peut être appelée sans créer d'instance de la classe dans laquelle elle est contenue");
			Generic ajQ0602 = jQ06.setHolder(answer, "Doit être contenue dans une classe static");
			Generic ajQ0603 = jQ06.setHolder(answer, "Ne peut effectuer d'opérations que sur des variables <em>static</em>");
			Generic ajQ0604 = jQ06.setHolder(answer, answerD);
			ajQ0601.setLink(userAnswer, true, sUser);
			ajQ0602.setLink(userAnswer, false, sUser);
			ajQ0603.setLink(userAnswer, false, sUser);
			ajQ0604.setLink(userAnswer, false, sUser);

			Generic jQ07 = qJava.setHolder(question, "Quelle(s) différence(s) y a-t-il entre <em>HashMap</em> et <em>HashTable</em> ?");
			Generic ajQ0701 = jQ07.setHolder(answer, "<em>HashTable</em> est synchronisé, <em>HashMap</em> ne l'est pas");
			Generic ajQ0702 = jQ07.setHolder(answer, "<em>HashMap</em> implémente l'interface <em>Map</em>, <em>HashTable</em> implémente l'interface <em>Set</em>");
			Generic ajQ0703 = jQ07.setHolder(answer, "<em>HashMap</em> autorise la présence de valeur <em>null</em>, <em>HashTable</em> ne l'autorise pas");
			Generic ajQ0704 = jQ07.setHolder(answer, answerD);
			ajQ0701.setLink(userAnswer, true, sUser);
			ajQ0702.setLink(userAnswer, false, sUser);
			ajQ0703.setLink(userAnswer, true, sUser);
			ajQ0704.setLink(userAnswer, false, sUser);

			Generic jQ08 = qJava.setHolder(question, "Java est un langage :");
			Generic ajQ0801 = jQ08.setHolder(answer, "Interprété");
			Generic ajQ0802 = jQ08.setHolder(answer, "Compilé");
			Generic ajQ0803 = jQ08.setHolder(answer, answerD);
			ajQ0801.setLink(userAnswer, true, sUser);
			ajQ0802.setLink(userAnswer, true, sUser);
			ajQ0803.setLink(userAnswer, false, sUser);

			Generic jQ09 = qJava.setHolder(question, "L'<em>ArrayList</em> :");
			Generic ajQ0901 = jQ09.setHolder(answer, "Peut contenir des types primitifs et des objets");
			Generic ajQ0902 = jQ09.setHolder(answer, "Ne peut contenir que des objets");
			Generic ajQ0903 = jQ09.setHolder(answer, "Possède une taille dynamique");
			Generic ajQ0904 = jQ09.setHolder(answer, "Possède une taille fixe définie lors de l'instanciation");
			ajQ0901.setLink(userAnswer, false, sUser);
			ajQ0902.setLink(userAnswer, true, sUser);
			ajQ0903.setLink(userAnswer, true, sUser);
			ajQ0904.setLink(userAnswer, false, sUser);

			Generic jQ10 = qJava.setHolder(question, "Une classe :");
			Generic ajQ1001 = jQ10.setHolder(answer, "Peut implémenter plusieurs interfaces mais ne peut étendre qu'une seule classe");
			Generic ajQ1002 = jQ10.setHolder(answer, "Peut implémenter plusieurs classes mais doit étendre une seule interface");
			Generic ajQ1003 = jQ10.setHolder(answer, "Peut implémenter plusieurs classes et peut étendre plusieurs interfaces");
			Generic ajQ1004 = jQ10.setHolder(answer, "Doit implémenter une seule interface et étendre une seule classe");
			ajQ1001.setLink(userAnswer, true, sUser);
			ajQ1002.setLink(userAnswer, false, sUser);
			ajQ1003.setLink(userAnswer, false, sUser);
			ajQ1004.setLink(userAnswer, false, sUser);

			Generic jQ11 = qJava.setHolder(question, "La signature d'une méthode est définie par :");
			Generic ajQ1101 = jQ11.setHolder(answer, "Le type de retour, le nom de la méthode et le type des paramètres");
			Generic ajQ1102 = jQ11.setHolder(answer, "Le type de retour, le nom de la méthode, le nombre, l’ordre et le type des paramètres");
			Generic ajQ1103 = jQ11.setHolder(answer, "Le nom de la méthode, le nombre et le type des paramètres");
			Generic ajQ1104 = jQ11.setHolder(answer, "Le nom de la méthode, le nombre, l'ordre et le type des paramètres");
			ajQ1101.setLink(userAnswer, false, sUser);
			ajQ1102.setLink(userAnswer, false, sUser);
			ajQ1103.setLink(userAnswer, false, sUser);
			ajQ1104.setLink(userAnswer, true, sUser);

			Generic jQ12 = qJava.setHolder(question, "Quelle(s) différence(s) y a-t-il entre ArrayList et LinkedList ?");
			Generic ajQ1201 = jQ12.setHolder(answer, "L’ajout d’un élément est plus rapide dans l’ArrayList");
			Generic ajQ1202 = jQ12.setHolder(answer, "L’ajout d’un élément est plus rapide dans la LinkedList");
			Generic ajQ1203 = jQ12.setHolder(answer, "La recherche d’un élément est plus rapide dans l’ArrayList");
			Generic ajQ1204 = jQ12.setHolder(answer, "La recherche d’un élément est plus rapide dans la LinkedList");
			ajQ1201.setLink(userAnswer, false, sUser);
			ajQ1202.setLink(userAnswer, true, sUser);
			ajQ1203.setLink(userAnswer, true, sUser);
			ajQ1204.setLink(userAnswer, false, sUser);

			Generic jQ13 = qJava.setHolder(question, "Parmi ces réponses, cocher l’intrus :");
			Generic ajQ1301 = jQ13.setHolder(answer, "stream");
			Generic ajQ1302 = jQ13.setHolder(answer, "lambda");
			Generic ajQ1303 = jQ13.setHolder(answer, "default method");
			Generic ajQ1304 = jQ13.setHolder(answer, "autoboxing");
			ajQ1301.setLink(userAnswer, false, sUser);
			ajQ1302.setLink(userAnswer, false, sUser);
			ajQ1303.setLink(userAnswer, false, sUser);
			ajQ1304.setLink(userAnswer, true, sUser);

			Generic jQ14 = qJava.setHolder(question, "Comment fonctionne le garbage collector ?");
			Generic ajQ1401 = jQ14.setHolder(answer, "Lorsqu’il n’y a plus de référence sur un objet, le garbage collector le détruit immédiatement");
			Generic ajQ1402 = jQ14.setHolder(answer, "Lorsqu’il n’y a plus de référence sur un objet, l’objet devient éligible à la garbage collection");
			Generic ajQ1403 = jQ14.setHolder(answer, "Il faut appeler manuellement le garbage collector pour détruire un objet");
			Generic ajQ1404 = jQ14.setHolder(answer, answerD);
			ajQ1401.setLink(userAnswer, false, sUser);
			ajQ1402.setLink(userAnswer, true, sUser);
			ajQ1403.setLink(userAnswer, false, sUser);
			ajQ1404.setLink(userAnswer, false, sUser);

			Generic jQ15 = qJava.setHolder(question, "À quel moment est appelée la méthode finalize() ?");
			Generic ajQ1501 = jQ15.setHolder(answer, "À la fin d’une transaction");
			Generic ajQ1502 = jQ15.setHolder(answer, "Après un bloc try… catch, quel que soit le résultat du try");
			Generic ajQ1503 = jQ15.setHolder(answer, "Lors du passage du garbage collector, avant le relâchement de l’objet en mémoire");
			Generic ajQ1504 = jQ15.setHolder(answer, answerD);
			ajQ1501.setLink(userAnswer, false, sUser);
			ajQ1502.setLink(userAnswer, false, sUser);
			ajQ1503.setLink(userAnswer, true, sUser);
			ajQ1504.setLink(userAnswer, false, sUser);

			Generic jQ16 = qJava.setHolder(question, "Laquelle de ces affirmations est vraie ?");
			Generic ajQ1601 = jQ16.setHolder(answer, "Les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe");
			Generic ajQ1602 = jQ16.setHolder(answer, "Les attributs déclarés dans une classe sont visibles seulement dans les méthodes déclarées après l’attribut");
			Generic ajQ1603 = jQ16.setHolder(answer, "Les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe seulement si leur visibilité est public");
			Generic ajQ1604 = jQ16.setHolder(answer, answerD);
			ajQ1601.setLink(userAnswer, true, sUser);
			ajQ1602.setLink(userAnswer, false, sUser);
			ajQ1603.setLink(userAnswer, false, sUser);
			ajQ1604.setLink(userAnswer, false, sUser);

			Generic jQ17 = qJava.setHolder(question, "Combien d’instance(s) de la classe A crée le code suivant ?<br><br><font size=+1>" + tab + "A x, u, v;<br>" + tab + "x = new A();<br>" + tab + "A y = x;<br>" + tab + "A z = new A();</font>");
			Generic ajQ1701 = jQ17.setHolder(answer, "Aucune");
			Generic ajQ1702 = jQ17.setHolder(answer, "5");
			Generic ajQ1703 = jQ17.setHolder(answer, "3");
			Generic ajQ1704 = jQ17.setHolder(answer, "2");
			ajQ1701.setLink(userAnswer, false, sUser);
			ajQ1702.setLink(userAnswer, false, sUser);
			ajQ1703.setLink(userAnswer, false, sUser);
			ajQ1704.setLink(userAnswer, true, sUser);

			Generic jQ18 = qJava.setHolder(question,
					"Pour la classe Middle définie comme suit :<br><br><font size=+1>" + tab + "class Middle {<br>" + tab2 + "public Middle() {<br>" + tab3 + "System.out.print(\"Au Revoir\");<br>" + tab2 + "}<br>" + tab2 + "public Middle(String nom) {<br>"
							+ tab3 + "this();<br>" + tab3 + "System.out.println(\"Bienvenue\" + nom);<br>" + tab2 + "}<br>" + tab + "}<br><br>Qu'affichera l'instruction :<br><br><font size=+1>" + tab + "Middle monMiddle = new Middle(\"Invité\") ?</font>");
			Generic ajQ1801 = jQ18.setHolder(answer, "Erreur de compilation");
			Generic ajQ1802 = jQ18.setHolder(answer, "Erreur d’exécution");
			Generic ajQ1803 = jQ18.setHolder(answer, "Au RevoirBienvenue Invité");
			Generic ajQ1804 = jQ18.setHolder(answer, "Bienvenue Invité");
			ajQ1801.setLink(userAnswer, false, sUser);
			ajQ1802.setLink(userAnswer, false, sUser);
			ajQ1803.setLink(userAnswer, true, sUser);
			ajQ1804.setLink(userAnswer, false, sUser);

			Generic jQ19 = qJava.setHolder(question,
					"Qu’obtient-on en exécutant ce code ?<br><br><font size=+1>" + tab + "String str = null;<br>" + tab + "String str1 = \"abc\";<br>" + tab + "System.out.println(str1.equals(\"abc\") | str.equals(null));</font>");
			Generic ajQ1901 = jQ19.setHolder(answer, "true");
			Generic ajQ1902 = jQ19.setHolder(answer, "false");
			Generic ajQ1903 = jQ19.setHolder(answer, "abc");
			Generic ajQ1904 = jQ19.setHolder(answer, answerD);
			ajQ1901.setLink(userAnswer, false, sUser);
			ajQ1902.setLink(userAnswer, false, sUser);
			ajQ1903.setLink(userAnswer, false, sUser);
			ajQ1904.setLink(userAnswer, true, sUser);

			Generic jQ20 = qJava.setHolder(question, "Quel est le résultat de la séquence suivante ?<br><br><font size=+1>" + tab + "List<String> myList = Arrays.asList(\"a1\", \"a2\", \"b2\", \"b1\", \"c2\", \"c1\");<br>" + tab + "myList.stream()<br>"
					+ tab2 + ".filter(s → s.startsWith(\"b\"))<br>" + tab2 + ".map(String::toUpperCase)<br>" + tab2 + ".sorted()<br>" + tab2 + ".forEach(System.out::print);</font>");
			Generic ajQ2001 = jQ20.setHolder(answer, "B2B1C2C1");
			Generic ajQ2002 = jQ20.setHolder(answer, "B1B2C1C2");
			Generic ajQ2003 = jQ20.setHolder(answer, "B1B2");
			Generic ajQ2004 = jQ20.setHolder(answer, answerD);
			ajQ2001.setLink(userAnswer, false, sUser);
			ajQ2002.setLink(userAnswer, false, sUser);
			ajQ2003.setLink(userAnswer, true, sUser);
			ajQ2004.setLink(userAnswer, false, sUser);

			engine.getCurrentCache().flush();
		}

	}
}
