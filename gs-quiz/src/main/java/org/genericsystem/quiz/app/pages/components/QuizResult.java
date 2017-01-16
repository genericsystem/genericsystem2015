package org.genericsystem.quiz.app.pages.components;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.QuizDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.Score01;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.Score02;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.UserDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.TitleResult;
import org.genericsystem.quiz.app.pages.components.QuizResult.MySumResult.MyResultP;
import org.genericsystem.quiz.app.pages.components.QuizResult.MySumResult.TitleResultH1;
import org.genericsystem.quiz.app.pages.components.QuizResult.SummaryResults;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.utils.QuizExtractors.QUIZ_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizExtractors.USER_EXTRACTOR;
import org.genericsystem.quiz.utils.ScoreUtils;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.context.ObservableListExtractor.SUBINSTANCES;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

@Children({ SummaryResults.class, AllResults.class })
public class QuizResult extends HtmlDiv {

	// Pour s'afficher, SummaryResult a besoin d'un Quiz en context. Il est utilisé pour afficher les scores d'un utilisateur après qu'il ait passé un quiz
	@Children({ MySumResult.class })
	@SelectContext(SELECTION_SELECTOR.class)
	public static class SummaryResults extends HtmlDiv implements SelectionDefaults {

	}

	// TODO Créer un affichage des résultats pour tous les users et pour le user connecté

	@Children({ TitleResultH1.class, MyResultP.class })
	public static class MySumResult extends HtmlDiv {

		public static class TitleResultH1 extends HtmlH1 {

			@Override
			public void init() {
				addPrefixBinding(context -> this.setText(context, "Récapitulatif des résultats pour le quiz : " + context.getGeneric()));
			}
		}

		public static class MyResultP extends HtmlP {

			@Override
			public void init() {

				addPrefixBinding(context -> {
					Generic quiz = context.getGeneric();
					Generic user = this.getLoggedUserProperty(context).getValue();
					Generic scoreUserQuiz = user.getLink(context.find(ScoreUserQuiz.class), quiz);

					Double grade02 = ScoreUtils.calculateDualGrade(context, quiz, user);

					String goodAnswer = (Integer) scoreUserQuiz.getValue() > 1 ? " bonnes réponses" : " bonne réponse";

					this.setText(context, "Vous venez de terminer le quiz.\nVotre Score est de " + grade02 + " sur 20\nVous avez eu " + scoreUserQuiz + goodAnswer);
				});
			}
		}
	}

	@DirectSelect(ScoreUserQuiz.class)
	@Children({ TitleResult.class, ScoreDiv.class })
	@ForEach(path = ScoreDiv.class, value = SUBINSTANCES.class)
	@Style(name = "width", value = "90%")
	@Style(path = { HtmlDiv.class, HtmlDiv.class }, name = "flex", value = "1")
	public static class AllResults extends HtmlDiv {

		@Children({ HtmlDiv.class, HtmlDiv.class, HtmlDiv.class, HtmlDiv.class, HtmlDiv.class })
		@SetText(path = HtmlDiv.class, pos = 0, value = "Pseudo")
		@SetText(path = HtmlDiv.class, pos = 1, value = "Réponses correctes")
		@SetText(path = HtmlDiv.class, pos = 2, value = "Score 1")
		@SetText(path = HtmlDiv.class, pos = 3, value = "Score 2")
		@SetText(path = HtmlDiv.class, pos = 4, value = "Quiz")
		//
		public static class TitleResult extends FlexRow {

		}

		@Children({ UserDiv.class, HtmlDiv.class, Score01.class, Score02.class, QuizDiv.class })
		@Select(path = UserDiv.class, value = USER_EXTRACTOR.class)
		@Select(path = QuizDiv.class, value = QUIZ_EXTRACTOR.class)
		@BindText(path = UserDiv.class)
		@BindText(path = HtmlDiv.class, pos = 1)
		@BindText(path = QuizDiv.class)
		public static class ScoreDiv extends FlexRow {

			@Override
			public void init() {
				addPrefixBinding(context -> {

					// REMARQUE : context.getGeneric().getComponent(0) == User.
					// context.getGeneric().getComponent(1) == Quiz.
					if (context.getGeneric().getComponent(0).equals(getLoggedUserProperty(context).getValue())) {
						this.addStyle(context, "font-weight", "bold");
					}
				});
			}

			public static class UserDiv extends HtmlDiv {

			}

			public static class Score01 extends HtmlDiv {

				@Override
				public void init() {
					addPrefixBinding(context -> {
						Double grade = ScoreUtils.calculateSimpleGrade(context, context.getGeneric().getComponent(1), context.getGeneric().getComponent(0));
						this.setText(context, grade + " / 20");
					});
				}
			}

			public static class Score02 extends HtmlDiv {

				@Override
				public void init() {

					addPrefixBinding(context -> {
						Double grade01 = ScoreUtils.calculateDualGrade(context, context.getGeneric().getComponent(1), context.getGeneric().getComponent(0));
						this.setText(context, grade01 + " / 20 ");
					});
				}
			}

			public static class QuizDiv extends HtmlDiv {

			}
		}

	}
}
