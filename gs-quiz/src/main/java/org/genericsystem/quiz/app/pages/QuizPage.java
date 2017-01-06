package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.utils.QuizTagSwitcher.QUESTION_PAGE;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@Switch(QUESTION_PAGE.class)
@Children({ HtmlH1.class, HtmlDiv.class })
@Children(path = HtmlDiv.class, value = HtmlDiv.class)
@SetText(path = HtmlH1.class, value = "QUESTIONNAIRE")
@SetText(path = { HtmlDiv.class, HtmlDiv.class }, value = "Division avec Stepper et Questions")
@Switch(path = HtmlDiv.class, value = LOGGED_USER.class)
public class QuizPage extends HtmlDiv {

}
