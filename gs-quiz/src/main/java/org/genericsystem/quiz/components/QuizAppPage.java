package org.genericsystem.quiz.components;

import org.genericsystem.quiz.components.QuizAppPage.QuizButton;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children({ QuizLogin.class, QuestionDiv.class, QuizButton.class })
@DirectSelect(Quiz.class)
public class QuizAppPage extends HtmlDiv {

	@Switch(TagSwitcher.LOGGED_USER.class)
	@ForEach(ObservableListExtractor.SUBINSTANCES.class)
	@BindText
	@BindAction(SET_SELECTION.class)
	public static class QuizButton extends HtmlButton {

	}

}
