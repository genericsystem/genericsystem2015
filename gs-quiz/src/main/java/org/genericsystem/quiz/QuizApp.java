package org.genericsystem.quiz;

import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.Responsive;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.quiz.QuizApp.QuizScript;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.quiz.model.UserAnswer;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.CustomAnnotations;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.security.model.User;

/**
 * @author Nicolas Feybesse
 *
 */
@CustomAnnotations(InheritStyle.class)
@RunScript(QuizScript.class)
@DependsOnModel({ Quiz.class, Question.class, Answer.class, UserAnswer.class, User.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "Java Appraisal")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Quiz.class, Question.class, Answer.class, UserAnswer.class, User.class })
public class QuizApp extends RootTagImpl {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, QuizApp.class, "/quiz");
	}

	public QuizApp() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}

	public static class QuizScript implements Script {

		@Override
		public void run(Root engine) {
			Generic user = engine.find(User.class);
			Generic nicolas = user.setInstance("Nicolas Feybesse");
			Generic fabienne = user.setInstance("Fabienne Ducroquet");

			Generic quiz = engine.find(Quiz.class);
			Generic frenchJavaQuiz = quiz.setInstance("French java quiz");

			Generic question = engine.find(Question.class);
			Generic varVisibility = frenchJavaQuiz.setHolder(question, "A propos de la visibilité des variables dans des classes");

			Generic answer = engine.find(Answer.class);
			Generic answer0 = varVisibility.setHolder(answer, "Il existe au total 3 niveaux de visibilité");
			Generic answer1 = varVisibility.setHolder(answer, "La visibilité protected permet aux classes filles d'accéder à la variable");

			Generic userAnswer = engine.find(UserAnswer.class);
			answer0.setLink(userAnswer, false, nicolas);
			answer1.setLink(userAnswer, true, nicolas);
			answer0.setLink(userAnswer, false, fabienne);
			answer1.setLink(userAnswer, false, fabienne);

			engine.getCurrentCache().flush();
		}

	}
}
