package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.app.pages.components.QuizHeader;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children({ QuizHeader.class, HomePage.class, QuizPage.class, ResultPage.class })
@Style(name = "color", value = "black")
public class AppPage extends HtmlDiv {

}
