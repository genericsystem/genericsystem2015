package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
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

	// ******************* Filtres (n'affichent qu'une partie du ForEach ou Select) ***********************
	// Tous les ScoreUserQuiz pour un quiz donné
	public static class Filtered_By_Quiz implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Generic scoreUser = context.getGeneric();
			Generic quiz = scoreUser.getComponent(1);
			Property<Generic> selectedQuiz = tag.getProperty(QuizContextAction.SELECTED_QUIZ, context);
			Property<Generic> loggedUser = tag.getLoggedUserProperty(context);

			if (loggedUser.getValue() != null && selectedQuiz != null)
				return Bindings.createBooleanBinding(() -> {
					return quiz.equals(selectedQuiz.getValue());
				}, selectedQuiz, loggedUser);

			if (selectedQuiz != null)
				return Bindings.createBooleanBinding(() -> {
					return quiz.equals(selectedQuiz.getValue());
				}, selectedQuiz);

			return new SimpleBooleanProperty(true);
		}

	}
}
