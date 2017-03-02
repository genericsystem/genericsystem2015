package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.model.Quiz;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.security.model.User;

import javafx.beans.property.Property;

public class QuizContextAction {

	public final static String SELECTED_QUIZ = "selectedQuiz";
	public final static String SELECTED_USER = "selectedUser";

	public static class ADD_LISTENED_PROPERTY implements ContextAction {

		public final static String INPUTTEXT_VALUE = "Input Text Value";

		@Override
		public void accept(Context context, Tag tag) {

			if (!tag.getClass().isAssignableFrom(HtmlInputText.class))
				return;

			Property<String> inputProperty = tag.getContextProperty(INPUTTEXT_VALUE, context);

			if (inputProperty == null)
				tag.getParent().createNewInitializedProperty(INPUTTEXT_VALUE, value -> null);

			tag.getContextProperty(INPUTTEXT_VALUE, context).setValue(tag.getDomNodeAttributes(context).get("value"));

		}

	}

	public static class CLEAR_QUIZ implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Property<Generic> selectedQuiz = tag.getContextProperty(SELECTED_QUIZ, context);

			if (selectedQuiz == null) {
				tag.getRootTag().createNewContextProperty(SELECTED_QUIZ);
				return;
			}

			if (selectedQuiz.getValue() == null)
				return;

			selectedQuiz.setValue(null);
		}
	}

	public static class CLEAR_USER implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Property<Generic> selectedUser = tag.getContextProperty(SELECTED_USER, context);

			if (selectedUser == null) {
				tag.getRootTag().createNewContextProperty(SELECTED_USER);
				return;
			}

			if (selectedUser.getValue() == null)
				return;

			selectedUser.setValue(null);
		}
	}

	public static class CLEAR_QUIZCONTEXT_PROPERTIES implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			new CLEAR_USER().accept(context, tag);
			new CLEAR_QUIZ().accept(context, tag);
		}

	}

	public static class SELECT_QUIZ implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Generic quiz = context.getGeneric();

			// Compare le type du tag avec la classe "Quiz" du modèle
			if (!quiz.getRoot().find(Quiz.class).equals(quiz.getMeta()))
				return;

			tag.getRootTag().setContextPropertyValue(SELECTED_QUIZ, context.getRootContext(), quiz);
		}
	}

	public static class SELECT_LOGGEDUSER implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Property<Generic> loggedUser = tag.getLoggedUserProperty(context);

			if (loggedUser == null)
				return;

			tag.getRootTag().setContextPropertyValue(SELECTED_USER, context.getRootContext(), loggedUser.getValue() + "");
		}

	}

	public static class SAVE_QUIZ_RESULT implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {

			Generic quiz = ((SelectionDefaults) tag).getSelectionProperty(context).getValue().getGeneric();
			Generic sUser = context.find(User.class).getInstance("Anti-Seche");
			Generic loggedUser = tag.getLoggedUserProperty(context).getValue();

			ScoreUtils.setResult(context, quiz, sUser, loggedUser);
			ScoreUtils.getResult(context, quiz, loggedUser);
		}
	}

	// NAVIGATION ENTRE LES PAGES
	public static class CLEAR_PAGES implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			if (tag.getContextProperty(QuizTagSwitcher.PAGE, context) != null)
				tag.getContextProperty(QuizTagSwitcher.PAGE, context).setValue(null);
		}

	}

	// Possibilité de factoriser
	public static class CALL_HOME_PAGE implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			tag.setInheritedContextPropertyValue(QuizTagSwitcher.PAGE, context, QuizTagSwitcher.HOME_PAGE);
		}
	}

	public static class CALL_RESULT_PAGE implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			tag.setInheritedContextPropertyValue(QuizTagSwitcher.PAGE, context, QuizTagSwitcher.RESULT_PAGE);
		}
	}

	public static class CALL_QUESTION_PAGE implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			tag.setInheritedContextPropertyValue(QuizTagSwitcher.PAGE, context, QuizTagSwitcher.QUESTION_PAGE);
		}

	}
}
