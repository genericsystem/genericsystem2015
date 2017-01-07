package org.genericsystem.quiz.components;

import org.genericsystem.quiz.components.QuizAppPage.QuizChoice;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.quiz.utils.QuizTagSwitcher;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

import javafx.beans.property.Property;

@Children({ QuizLogin.class, QuestionDiv.class, QuizChoice.class, QuizResult.class })
@DirectSelect(Quiz.class)
public class QuizAppPage extends HtmlDiv {

	@Switch({ TagSwitcher.LOGGED_USER.class, QuizTagSwitcher.NO_ACTIVE_QUIZ.class })
	@Children(QuizButton.class)
	public static class QuizChoice extends HtmlDiv implements SelectionDefaults {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				Property<Boolean> property = getProperty("QuizDone", context);
				if (property == null)
					createNewInitializedProperty("QuizDone", c -> false);
				else
					property.setValue(false);
			});
		}
	}

	@ForEach(ObservableListExtractor.SUBINSTANCES.class)
	@BindText
	@BindAction(SET_SELECTION.class)
	public static class QuizButton extends HtmlButton {

	}

}
