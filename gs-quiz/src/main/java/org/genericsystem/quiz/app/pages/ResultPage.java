package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.app.pages.components.QuizResult;
import org.genericsystem.quiz.utils.QuizTagSwitcher.RESULT_PAGE;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@Switch(RESULT_PAGE.class)
@Children({ HtmlH1.class, QuizResult.class })
//
@SetText(path = HtmlH1.class, value = "RESULTATS")
public class ResultPage extends HtmlDiv {

}
