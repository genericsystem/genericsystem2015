package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.components.QuestionDiv;
import org.genericsystem.quiz.components.QuestionDiv.FooterDiv;
import org.genericsystem.quiz.components.QuestionDiv.FooterDiv.FinishBtn;
import org.genericsystem.quiz.utils.QuizContextAction.CALL_RESULT_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.SAVE_QUIZ_RESULT;
import org.genericsystem.quiz.utils.QuizTagSwitcher.QUESTION_PAGE;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@Switch(QUESTION_PAGE.class)
@Children({ HtmlH1.class, HtmlDiv.class })
@Children(path = HtmlDiv.class, value = QuestionDiv.class)
@Switch(path = HtmlDiv.class, value = { LOGGED_USER.class })
//
@BindAction(path = { HtmlDiv.class, QuestionDiv.class, FooterDiv.class, FinishBtn.class }, value = { SAVE_QUIZ_RESULT.class, CALL_RESULT_PAGE.class })
//
@SetText(path = HtmlH1.class, value = "QUESTIONNAIRE")
public class QuizPage extends HtmlDiv {

}
