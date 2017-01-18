package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
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

	// ******************* Filtres (n'affichent qu'une partie du ForEach ou Select) ***********************
	// Tous les ScoreUserQuiz pour un quiz donné
	public static class Filtered_By_Quiz implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Generic quiz = context.getGeneric().getComponent(1);
			Property<Generic> selectedQuiz = tag.getProperty(QuizContextAction.SELECTED_QUIZ, context);

			if (selectedQuiz == null)
				tag.getRootTag().createNewProperty(QuizContextAction.SELECTED_QUIZ);

			return Bindings.createBooleanBinding(() -> {

				if (selectedQuiz.getValue() == null)
					return true;

				Boolean isQuiz = quiz.equals(selectedQuiz.getValue());

				return isQuiz;
			}, selectedQuiz);
		}
	}

	public static class Filtered_By_User implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Generic user = context.getGeneric().getComponent(0);
			Property<Generic> selectedUser = tag.getLoggedUserProperty(context);

			if (selectedUser == null)
				tag.getRootTag().createNewProperty(QuizContextAction.SELECTED_USER);

			return Bindings.createBooleanBinding(() -> {

				if (selectedUser.getValue() == null)
					return true;

				return selectedUser.getValue().equals(user);
			}, selectedUser);
		}

	}

	public static class Filtered implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Generic quiz = context.getGeneric().getComponent(1);
			Generic user = context.getGeneric().getComponent(0);

			if (tag.getProperty(QuizContextAction.SELECTED_QUIZ, context) == null)
				tag.getRootTag().createNewProperty(QuizContextAction.SELECTED_QUIZ, context.getRootContext());
			if (tag.getProperty(QuizContextAction.SELECTED_USER, context) == null)
				tag.getRootTag().createNewProperty(QuizContextAction.SELECTED_USER, context.getRootContext());

			Property<Generic> selectedQuiz = tag.getProperty(QuizContextAction.SELECTED_QUIZ, context);
			Property<Generic> selectedUser = tag.getProperty(QuizContextAction.SELECTED_USER, context);

			return Bindings.createBooleanBinding(() -> {

				if (selectedQuiz.getValue() == null && selectedUser.getValue() == null)
					return true;

				boolean isQuiz = quiz.equals(selectedQuiz.getValue());
				boolean isUser = user.equals(selectedUser.getValue());

				if (selectedQuiz.getValue() != null && selectedUser.getValue() == null)
					return isQuiz;
				if (selectedQuiz.getValue() == null && selectedUser.getValue() != null)
					return isUser;

				return (isQuiz && isUser);

			}, selectedUser, selectedQuiz);
		}
	}
}
