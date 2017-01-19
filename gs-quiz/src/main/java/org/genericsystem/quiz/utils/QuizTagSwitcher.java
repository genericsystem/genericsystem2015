package org.genericsystem.quiz.utils;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class QuizTagSwitcher {

	public final static String PAGE = "Page";
	public final static String HOME_PAGE = "HomePage";
	public final static String QUESTION_PAGE = "QuestionPage";
	public final static String RESULT_PAGE = "ResultPage";

	// ************* NAVIGATION ENTRE LES PAGES **********************

	public static class HOME_PAGE implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getProperty(PAGE, context);
			return Bindings.createBooleanBinding(() -> HOME_PAGE.equals(pageProperty.getValue()), pageProperty);
		}
	}

	public static class QUESTION_PAGE implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getProperty(PAGE, context);
			return Bindings.createBooleanBinding(() -> QUESTION_PAGE.equals(pageProperty.getValue()), pageProperty);
		}
	}

	public static class RESULT_PAGE implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getProperty(PAGE, context);
			return Bindings.createBooleanBinding(() -> RESULT_PAGE.equals(pageProperty.getValue()), pageProperty);
		}
	}
}
