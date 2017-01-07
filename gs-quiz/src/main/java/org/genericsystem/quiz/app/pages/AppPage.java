package org.genericsystem.quiz.app.pages;

import org.genericsystem.quiz.components.QuizLogin;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children({ QuizLogin.class, Nav.class, HomePage.class, QuizPage.class, ResultPage.class })
@StyleClass("white")
public class AppPage extends HtmlDiv {

}
