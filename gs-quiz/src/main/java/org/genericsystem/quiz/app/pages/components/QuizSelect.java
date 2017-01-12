package org.genericsystem.quiz.app.pages.components;

import org.genericsystem.quiz.app.pages.components.QuizSelect.QuizChoice;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children(QuizChoice.class)
public class QuizSelect extends HtmlDiv {

	@Children(QuizButton.class)
	@DirectSelect(Quiz.class)
	public static class QuizChoice extends HtmlDiv implements SelectionDefaults {

	}

	@ForEach(ObservableListExtractor.SUBINSTANCES.class)
	@BindText
	// @BindAction(SET_SELECTION.class)
	public static class QuizButton extends HtmlButton {

	}

}
