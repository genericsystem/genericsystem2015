package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import io.reactivex.Observable;
import javafx.beans.property.Property;

public class QuizTagSwitcher {

	public final static String PAGE = "Page";
	public final static String HOME_PAGE = "HomePage";
	public final static String QUESTION_PAGE = "QuestionPage";
	public final static String RESULT_PAGE = "ResultPage";

	public static class QUIZ_NOT_DONE implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUser = tag.getLoggedUserProperty(context);
			Generic quiz = context.getGeneric();

			if (loggedUser == null || !context.getRootContext().find(Quiz.class).getClass().isAssignableFrom(quiz.getClass()))
				return null;

			Generic scoreUser = quiz.getLink(context.getRootContext().find(ScoreUserQuiz.class), loggedUser.getValue());

			return RxJavaHelpers.optionalValuesOf(loggedUser).map(optUser -> scoreUser == null);
		}
	}

	// ************* NAVIGATION ENTRE LES PAGES **********************

	public static class HOME_PAGE implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return RxJavaHelpers.valuesOf(pageProperty).map(page -> HOME_PAGE.equals(page));
		}
	}

	public static class QUESTION_PAGE implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return RxJavaHelpers.valuesOf(pageProperty).map(page -> QUESTION_PAGE.equals(page));
		}
	}

	public static class RESULT_PAGE implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return RxJavaHelpers.valuesOf(pageProperty).map(page -> RESULT_PAGE.equals(page));
		}
	}
}
