package org.genericsystem.quiz.app;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Children({ QuizLogin.class, QuestionDiv.class })
public class QuizAppPage extends HtmlDiv {

}
