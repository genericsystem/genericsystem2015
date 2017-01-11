package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.components.QuizLogin;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@Children({ Logo.class, FlexDiv.class, QuizLogin.class })
@Children(path = FlexDiv.class, pos = 1, value = { AppTitleDiv.class, Nav.class })
//
@SetText(path = { FlexDiv.class, AppTitleDiv.class, HtmlH1.class }, pos = { 1, 0, 0 }, value = "GS Quiz")
//
@Style(name = "justify-content", value = "space-between")
@Style(name = "background-color", value = "lightgrey")
//
@Style(path = HtmlDiv.class, name = "max-width", value = "25%")
@Style(path = Logo.class, name = "max-width", value = "25%")
@Style(path = Logo.class, name = "min-width", value = "200px")
@Style(path = Logo.class, name = "flex", value = "0")
@Style(path = Logo.class, name = "justify-content", value = "center")
@Style(path = QuizLogin.class, name = "max-width", value = "25%")
@Style(path = QuizLogin.class, name = "min-width", value = "200px")
@Style(path = FlexDiv.class, pos = 1, name = "min-width", value = "45%")
@Style(path = FlexDiv.class, pos = 1, name = "flex", value = "1")
@Style(path = { FlexDiv.class, AppTitleDiv.class }, pos = { 1, 0 }, name = "flex", value = "1")
public class QuizHeader extends FlexRow {

}
