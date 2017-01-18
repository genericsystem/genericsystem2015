package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.app.pages.components.QuizSelect;
import org.genericsystem.quiz.app.pages.components.QuizSelect.QuizButton;
import org.genericsystem.quiz.app.pages.components.QuizSelect.QuizChoice;
import org.genericsystem.quiz.app.pages.components.QuizSelect.ResultButton;
import org.genericsystem.quiz.utils.QuizContextAction.CALL_QUESTION_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CALL_RESULT_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.SELECT_QUIZ;
import org.genericsystem.quiz.utils.QuizTagSwitcher.HOME_PAGE;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.context.TagSwitcher.NO_LOGGED_USER;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

@Switch(HOME_PAGE.class)
@Children({ HtmlH1.class, HtmlP.class, HtmlP.class, QuizSelect.class })
@Switch(path = QuizSelect.class, value = LOGGED_USER.class)
@Switch(path = HtmlP.class, pos = 1, value = NO_LOGGED_USER.class)
//
@BindAction(path = { QuizSelect.class, QuizChoice.class, FlexRow.class, QuizButton.class }, value = { SET_SELECTION.class, CALL_QUESTION_PAGE.class })
@BindAction(path = { QuizSelect.class, QuizChoice.class, FlexRow.class, ResultButton.class }, value = { SELECT_QUIZ.class, CALL_RESULT_PAGE.class })
//
@SetText(path = HtmlH1.class, value = "ACCUEIL")
@SetText(path = HtmlP.class, pos = 0, value = "Bienvenue sur le quiz de GenericSystem !")
@SetText(path = HtmlP.class, pos = 1, value = "Veuillez vous connecter pour passer un quiz")
//
@Style(path = { QuizSelect.class, QuizChoice.class }, name = "border-top", value = "2px solid dimgray")
public class HomePage extends HtmlDiv {

}
