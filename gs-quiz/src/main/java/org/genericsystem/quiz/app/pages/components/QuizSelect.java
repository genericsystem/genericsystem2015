package org.genericsystem.quiz.app.pages.components;

import org.genericsystem.quiz.app.pages.components.QuizSelect.QuizChoice;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.quiz.utils.QuizExtractors.DESCRIPTION_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizTagSwitcher.QUIZ_NOT_DONE;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH4;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

@Children(QuizChoice.class)
@DirectSelect(Quiz.class)
public class QuizSelect extends HtmlDiv {

	@Children({ FlexDiv.class, FlexRow.class })
	@Children(path = FlexDiv.class, pos = 0, value = { HtmlH4.class, HtmlP.class })
	@Children(path = FlexRow.class, value = { QuizButton.class, ResultButton.class })
	@ForEach(ObservableListExtractor.SUBINSTANCES.class)
	@ForEach(path = { FlexDiv.class, HtmlP.class }, value = DESCRIPTION_EXTRACTOR.class)
	//
	@BindText(path = { FlexDiv.class, HtmlH4.class })
	@BindText(path = { FlexDiv.class, HtmlP.class })
	//
	@Style(name = "width", value = "100%")
	@Style(path = FlexDiv.class, pos = 0, name = "flex", value = "1")
	@Style(path = FlexRow.class, name = "min-width", value = "250px")
	@Style(path = FlexRow.class, name = "flex", value = "0")
	@Style(path = FlexRow.class, name = "justify-content", value = "space-around")
	@StyleClass(path = { FlexRow.class, QuizButton.class }, value = { "selectQButton", "greenButton" })
	@StyleClass(path = { FlexRow.class, ResultButton.class }, value = { "selectQButton", "redButton" })
	public static class QuizChoice extends FlexRow implements SelectionDefaults {

	}

	@SetText("Commencer")
	@Switch(QUIZ_NOT_DONE.class)
	public static class QuizButton extends HtmlButton {

	}

	@SetText("Scores")
	@Style(name = "margin-left", value = "auto")
	public static class ResultButton extends HtmlButton {

	}

}
