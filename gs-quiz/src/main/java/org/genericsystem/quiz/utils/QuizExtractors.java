package org.genericsystem.quiz.utils;

import java.util.Comparator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Description;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.TagSwitcher;

import io.reactivex.Observable;
import javafx.beans.property.Property;

public class QuizExtractors {

	public static class ANSWERS_EXTRACTOR implements ObservableListExtractor {
		// generics[0] est l'element courant.
		// Le getRoot permet d'utiliser la methode find (Le root donne accès à tous les éléments du context)
		// TODO Rendre la méthode générique -> lui faire trouver les enfants d'un generic ssi il y a un unique enfant
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return Observable.just(generics[0].getHolders(generics[0].getRoot().find(Answer.class)));
		}
	}

	public static class QUESTIONS_EXTRACTOR implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return Observable.just(generics[0].getHolders(generics[0].getRoot().find(Question.class)));
		}
	}

	public static class DESCRIPTION_EXTRACTOR implements ObservableListExtractor {

		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return Observable.just(generics[0].getHolders(generics[0].getRoot().find(Description.class)));
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

	public static class FILTER_QUIZ implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			if (tag.getContextProperty(QuizContextAction.SELECTED_QUIZ, context) == null)
				tag.getRootTag().createNewContextProperty(QuizContextAction.SELECTED_QUIZ, context.getRootContext());
			Property<Generic> selectedQuiz = tag.getContextProperty(QuizContextAction.SELECTED_QUIZ, context);
			return RxJavaHelpers.optionalValuesOf(selectedQuiz).map(optQuiz -> !optQuiz.isPresent() || context.getGeneric().getComponent(1).equals(optQuiz.get()));
		}
	}

	public static class FILTER_USER implements TagSwitcher {

		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			if (tag.getContextProperty(QuizContextAction.SELECTED_USER, context) == null)
				tag.getRootTag().createNewContextProperty(QuizContextAction.SELECTED_USER, context.getRootContext());
			Property<String> selectedUser = tag.getContextProperty(QuizContextAction.SELECTED_USER, context);
			return RxJavaHelpers.optionalValuesOf(selectedUser).map(optUser -> !optUser.isPresent() || optUser.get().trim().isEmpty()
					|| ((String) context.getGeneric().getComponent(0).getValue()).trim().toLowerCase().contains(optUser.get().trim().toLowerCase()));
		}
	}

	public static class SCORES_FILTERED implements ObservableListExtractorFromContext {

		@Override
		public Observable<Snapshot<Generic>> apply(Context context, Tag tag) {
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

			return Observable.just(context.getGeneric().getSubInstances().sort(byQuiz.thenComparing(byScoreUser)));
		}
	}
}
