package org.genericsystem.quiz.app;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.app.QuizAppPage.QuizButton;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

import javafx.collections.ObservableList;

@Children({ QuizLogin.class, QuestionDiv.class, QuizButton.class })
@DirectSelect(Quiz.class)
public class QuizAppPage extends HtmlDiv {

	@ForEach(ObservableListExtractor.SUBINSTANCES.class)
	@BindText
	@BindAction(SET_SELECTION.class)
	public static class QuizButton extends HtmlButton {

	}

	//////////////////////////////// Traitements \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	public static class ANSWERS_EXTRACTOR implements ObservableListExtractor {
		// generics[0] est l'element courant.
		// Le getRoot permet d'utiliser la methode find (Le root donne accès à tous les éléments du context)
		// TODO Rendre la méthode générique -> lui faire trouver les enfants d'un generic ssi il y a un unique enfant
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Answer.class));
		}
	}

}
