package org.genericsystem.quiz.app;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.quiz.app.QuizApp.QuizzScript;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.security.model.User;

@RunScript(QuizzScript.class)
@DependsOnModel({ Quiz.class, Question.class, Answer.class, User.class })
@Children(QuestionDiv.class)
// @Children(Responsive.class)
// @Children(path = Responsive.class, value = TitledInstancesTable.class)
@Style(name = "background-color", value = "grey")
// @DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = Question.class)
public class QuizApp extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, QuizApp.class, "/QuizzApp");
	}

	public QuizApp() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}

	public static class QuizzScript implements Script {

		@Override
		public void run(Root engine) {
			// Create user
			Generic user = engine.find(User.class);
			Generic robert = user.setInstance("Robert DJ");

			// Create Quiz
			Generic quiz = engine.find(Quiz.class);
			Generic quizTest = quiz.setInstance("Petit Test");

			// Create Questions (Question.class is a component of Quiz.class)
			Generic question = engine.find(Question.class);
			Generic q01 = quizTest.setHolder(question, "Quel est le résultat de la séquence : \nArrayList<String> mots ;\nmots.add('azer') ;");
			Generic q02 = quizTest.setHolder(question, "Portée des attributs : laquelle de ces affirmations est vraie ?");
			Generic q03 = quizTest.setHolder(question, "laquelle de ces affirmations est vraie ?");

			// Create Answers (Answer.class is a component of Question.class)
			Generic answer = engine.find(Answer.class);
			Generic a01 = q01.setHolder(answer, "la chaine 'azer' est ajoutée à la liste");
			Generic a02 = q01.setHolder(answer, "un ArrayOutOfBoundsException");
			Generic a03 = q01.setHolder(answer, "un NullPointerException");
			Generic a04 = q01.setHolder(answer, "Aucune de ces réponses");
			Generic a05 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe");
			Generic a06 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles seulement dans les méthodes déclarées après l'attribut");
			Generic a07 = q02.setHolder(answer, "les attributs déclarés dans une classe sont visibles dans toutes les méthodes de la classe seulement si leur visibilité est public");
			Generic a08 = q02.setHolder(answer, "Aucune de ces réponses");

			engine.getCurrentCache().flush();
		}

	}
}
