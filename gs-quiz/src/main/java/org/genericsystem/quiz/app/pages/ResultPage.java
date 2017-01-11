package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.components.QuizResult;
import org.genericsystem.quiz.utils.QuizTagSwitcher.RESULT_PAGE;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@Switch(RESULT_PAGE.class)
@Children({ HtmlH1.class, HtmlDiv.class, HtmlDiv.class, QuizResult.class })
@Children(path = HtmlDiv.class, pos = 0, value = HtmlDiv.class)
@Switch(path = HtmlDiv.class, pos = 0, value = LOGGED_USER.class)
//
@SetText(path = HtmlH1.class, value = "RESULTATS")
@SetText(path = { HtmlDiv.class, HtmlDiv.class }, pos = { 0, 0 }, value = "Division contenant mes résultats (N'apparait que si l'utilisateur est loggé)")
@SetText(path = HtmlDiv.class, pos = 1, value = "Division contenant TOUS les résultats")
public class ResultPage extends HtmlDiv {

}
