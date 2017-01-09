package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.utils.QuizContextAction.CALL_QUESTION_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CLEAR_PAGES;
import org.genericsystem.quiz.utils.QuizTagSwitcher.HOME_PAGE;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.context.TagSwitcher.NO_LOGGED_USER;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

@Switch(HOME_PAGE.class)
@Children({ HtmlH1.class, HtmlP.class, HtmlDiv.class, HtmlP.class })
@Children(path = HtmlDiv.class, value = HtmlButton.class)
@SetText(path = HtmlH1.class, value = "ACCUEIL")
@SetText(path = HtmlP.class, pos = 0, value = "Bienvenue sur le quiz de GenericSystem !")
@SetText(path = HtmlDiv.class, value = "Choix du Quiz (Ne doit apparaître que si l'utilisateur est loggé)")
@SetText(path = { HtmlDiv.class, HtmlButton.class }, value = "Quiz")
@SetText(path = HtmlP.class, pos = 1, value = "Veuillez vous connecter pour passer un quiz")
@BindAction(path = { HtmlDiv.class, HtmlButton.class }, value = { CLEAR_PAGES.class, CALL_QUESTION_PAGE.class })
@Switch(path = HtmlDiv.class, value = { HOME_PAGE.class, LOGGED_USER.class })
@Switch(path = HtmlP.class, pos = 1, value = { HOME_PAGE.class, NO_LOGGED_USER.class })
public class HomePage extends HtmlDiv {

}
