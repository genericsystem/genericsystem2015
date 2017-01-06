package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.utils.QuizContextAction.CALL_HOME_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CALL_RESULT_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CLEAR_PAGES;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;

@Children({ HtmlDiv.class, HtmlDiv.class })
@Children(path = HtmlDiv.class, pos = 0, value = { HtmlHyperLink.class, HtmlHyperLink.class })
@Children(path = HtmlDiv.class, pos = 1, value = HtmlHyperLink.class)
@SetText(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 0, 0 }, value = "Accueil")
@SetText(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 0, 1 }, value = "Liste de Résultats")
@SetText(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 1, 0 }, value = "Mes scores")
@BindAction(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 0, 0 }, value = { CLEAR_PAGES.class, CALL_HOME_PAGE.class })
@BindAction(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 0, 1 }, value = { CLEAR_PAGES.class, CALL_RESULT_PAGE.class })
@BindAction(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 1, 0 }, value = { CLEAR_PAGES.class, CALL_RESULT_PAGE.class })
@Switch(path = { HtmlDiv.class, HtmlHyperLink.class }, pos = { 1, 0 }, value = LOGGED_USER.class)
public class Nav extends HtmlDiv {

}
