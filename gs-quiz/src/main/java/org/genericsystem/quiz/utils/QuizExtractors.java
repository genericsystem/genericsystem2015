package org.genericsystem.quiz.utils;

import java.util.Comparator;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Description;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.context.ObservableValueSelector;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.collections.ObservableList;

public class QuizExtractors {

	public static class ANSWERS_EXTRACTOR implements ObservableListExtractor {
		// generics[0] est l'element courant.
		// Le getRoot permet d'utiliser la methode find (Le root donne accès à tous les éléments du context)
		// TODO Rendre la méthode générique -> lui faire trouver les enfants d'un generic ssi il y a un unique enfant
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Answer.class));
		}
	}

	public static class QUESTIONS_EXTRACTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Question.class));
		}
	}

	public static class DESCRIPTION_EXTRACTOR implements ObservableListExtractor {

		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Description.class));
		}

	}

	public static class USER_EXTRACTOR implements ObservableValueSelector {

		@Override
		public Generic apply(Generic[] generics) {
			return generics[0].getComponent(0);
		}

	}

	public static class QUIZ_EXTRACTOR implements ObservableValueSelector {

		@Override
		public Generic apply(Generic[] generics) {
			return generics[0].getComponent(1);
		}

	}

	public static class SCORES_FILTERED implements ObservableListExtractorFromContext {

		@Override
		public ObservableList<Generic> apply(Context context, Tag tag) {
			Comparator<Generic> byScoreUser = (score1, score2) -> {
				if (!score1.getClass().isAssignableFrom(score1.getRoot().find(ScoreUserQuiz.class).getClass()) || !score2.getClass().isAssignableFrom(score2.getRoot().find(ScoreUserQuiz.class).getClass()))
					return 0;
				else
					return Integer.compare((Integer) score2.getValue(), (Integer) score1.getValue());
			};

			Comparator<Generic> byQuiz = (score1, score2) -> {
				if (!score1.getClass().isAssignableFrom(score1.getRoot().find(ScoreUserQuiz.class).getClass()) || !score2.getClass().isAssignableFrom(score2.getRoot().find(ScoreUserQuiz.class).getClass()))
					return 0;
				else
					return score1.getComponent(1).compareTo(score2.getComponent(1));
			};

			if (tag.getContextProperty(QuizContextAction.SELECTED_QUIZ, context) == null)
				tag.getRootTag().createNewContextProperty(QuizContextAction.SELECTED_QUIZ, context.getRootContext());
			if (tag.getContextProperty(QuizContextAction.SELECTED_USER, context) == null)
				tag.getRootTag().createNewContextProperty(QuizContextAction.SELECTED_USER, context.getRootContext());

			Property<Generic> selectedQuiz = tag.getContextProperty(QuizContextAction.SELECTED_QUIZ, context);
			Property<String> selectedUser = tag.getContextProperty(QuizContextAction.SELECTED_USER, context);

			return new ListBinding<Generic>() {
				{
					bind(selectedQuiz, selectedUser);
				}

				@Override
				protected ObservableList<Generic> computeValue() {
					return context.getGeneric().getObservableSubInstances().sorted(byQuiz.thenComparing(byScoreUser)).filtered(scoreUser -> {

						boolean isQuiz = true;
						boolean isUser = true;

						if (selectedQuiz.getValue() != null)
							isQuiz = scoreUser.getComponent(1).equals(selectedQuiz.getValue());
						if (selectedUser.getValue() != null && !selectedUser.getValue().trim().isEmpty())
							isUser = ((String) scoreUser.getComponent(0).getValue()).trim().toLowerCase().contains(selectedUser.getValue().trim().toLowerCase());

						return (isQuiz && isUser);
					});
				}
			};
		}

	}

}
