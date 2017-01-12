package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.app.pages.components.QuizHeader;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children({ QuizHeader.class, HomePage.class, QuizPage.class, ResultPage.class })
//
@StyleClass("appQ")
@StyleClass(path = HomePage.class, value = "mainDivBodyQ")
@StyleClass(path = QuizPage.class, value = "mainDivBodyQ")
@StyleClass(path = ResultPage.class, value = "mainDivBodyQ")
public class AppPage extends HtmlDiv {

}
