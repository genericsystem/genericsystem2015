package org.genericsystem.quiz.app.pages.components;

import org.genericsystem.quiz.utils.QuizContextAction.CALL_HOME_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CALL_RESULT_PAGE;
import org.genericsystem.quiz.utils.QuizContextAction.CLEAR_QUIZ;
import org.genericsystem.quiz.utils.QuizContextAction.CLEAR_QUIZCONTEXT_PROPERTIES;
import org.genericsystem.quiz.utils.QuizContextAction.SELECT_LOGGEDUSER;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLi;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlUl;

@Children(HtmlUl.class)
@Children(path = HtmlUl.class, value = { HtmlLi.class, HtmlLi.class, HtmlLi.class })
@Children(path = { HtmlUl.class, HtmlLi.class }, value = HtmlHyperLink.class)
@Switch(path = { HtmlUl.class, HtmlLi.class }, pos = { 0, 2 }, value = LOGGED_USER.class)
//
@BindAction(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 0, 0 }, value = CALL_HOME_PAGE.class)
@BindAction(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 1, 0 }, value = { CLEAR_QUIZCONTEXT_PROPERTIES.class, CALL_RESULT_PAGE.class })
@BindAction(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 2, 0 }, value = { CLEAR_QUIZ.class, SELECT_LOGGEDUSER.class, CALL_RESULT_PAGE.class })
//
@Style(name = "justify-content", value = "center")
@Style(path = HtmlUl.class, name = "padding", value = "0px")
@StyleClass(path = { HtmlUl.class, HtmlLi.class }, value = "navQLi")
@StyleClass(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, value = { "navQA", "vertical-align" })
//
@SetText(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 0, 0 }, value = "<strong>ACCUEIL</strong>")
@SetText(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 1, 0 }, value = "<strong>RESULTATS</strong>")
@SetText(path = { HtmlUl.class, HtmlLi.class, HtmlHyperLink.class }, pos = { 0, 2, 0 }, value = "<strong>MES SCORES</strong>")
public class QuizNav extends FlexRow {

}
