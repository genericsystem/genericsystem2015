package org.genericsystem.quiz.utils;

import java.util.Comparator;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableValueSelector;

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

	public static class SCORES_EXTRACTOR implements ObservableListExtractor {

		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Comparator<Generic> byScoreUser = (score1, score2) -> {
				if (!score1.getClass().isAssignableFrom(score1.getRoot().find(ScoreUserQuiz.class).getClass()) || !score2.getClass().isAssignableFrom(score2.getRoot().find(ScoreUserQuiz.class).getClass()))
					return 0;
				else
					return Integer.compare((Integer) score2.getValue(), (Integer) score1.getValue());
			};

			System.out.println(generics[0].getObservableSubInstances().sorted(byScoreUser));
			return generics[0].getObservableSubInstances().sorted(byScoreUser);
		}

	}

	public static class QUESTIONS_EXTRACTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Question.class));
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

}
