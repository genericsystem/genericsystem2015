package org.genericsystem.quiz.components;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.components.QuizResult.AllSumResults.ScoreDiv;
import org.genericsystem.quiz.components.QuizResult.AllSumResults.ScoreDiv.ResultDiv01;
import org.genericsystem.quiz.components.QuizResult.AllSumResults.ScoreDiv.ResultDiv02;
import org.genericsystem.quiz.components.QuizResult.AllSumResults.TitleResult;
import org.genericsystem.quiz.components.QuizResult.MySumResult.MyResultP;
import org.genericsystem.quiz.components.QuizResult.MySumResult.TitleResultH1;
import org.genericsystem.quiz.components.QuizResult.SummaryResults;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.utils.QuizExtractors.SCORES_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizExtractors.USER_EXTRACTOR;
import org.genericsystem.quiz.utils.ScoreUtils;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

@Children(SummaryResults.class)
@SetText("DIV DE RESULTATS !!!!!!!!!!")
public class QuizResult extends HtmlDiv {

	// Pour s'afficher, SummaryResult a besoin d'un Quiz en context. Il est utilisé pour afficher les score d'un utilisateur après qu'il ait passé un quiz
	@Children({ MySumResult.class, AllSumResults.class })
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

	@Children({ TitleResult.class, ScoreDiv.class })
	@SetText("Tous les résultats :")
	//
	@Style(path = { HtmlDiv.class, HtmlDiv.class }, name = "flex", value = "1")
	public static class AllSumResults extends HtmlDiv {

		@Children({ HtmlDiv.class, HtmlDiv.class, HtmlDiv.class, HtmlDiv.class })
		@SetText(path = HtmlDiv.class, pos = 0, value = "Pseudo")
		@SetText(path = HtmlDiv.class, pos = 1, value = "Réponses correctes")
		@SetText(path = HtmlDiv.class, pos = 2, value = "Score 1")
		@SetText(path = HtmlDiv.class, pos = 3, value = "Score 2")
		//
		@Style(name = "display", value = "flex")
		@Style(name = "flex", value = "1")
		@Style(name = "flex-direction", value = "row")
		@Style(name = "width", value = "100%")
		public static class TitleResult extends HtmlDiv {

		}

		@Children({ HtmlDiv.class, HtmlDiv.class, ResultDiv01.class, ResultDiv02.class })
		@ForEach(SCORES_EXTRACTOR.class)
		@Select(path = HtmlDiv.class, pos = 0, value = USER_EXTRACTOR.class)
		@BindText(path = HtmlDiv.class, pos = 0)
		@BindText(path = HtmlDiv.class, pos = 1)
		//
		@Style(name = "display", value = "flex")
		@Style(name = "flex", value = "1")
		@Style(name = "flex-direction", value = "row")
		@Style(name = "width", value = "100%")
		public static class ScoreDiv extends HtmlDiv {

			public static class ResultDiv01 extends HtmlDiv {

				@Override
				public void init() {

					addPrefixBinding(context -> {
						Generic scoreUserQuiz = context.getGeneric();

						if (!(scoreUserQuiz.getMeta().equals(scoreUserQuiz.getRoot().find(ScoreUserQuiz.class))))
							return;

						Generic user = scoreUserQuiz.getComponent(0);
						Generic quiz = scoreUserQuiz.getComponent(1);
						Double grade01 = ScoreUtils.calculateSimpleGrade(context, quiz, user);
						this.setText(context, grade01 + " / 20 ");
					});
				}
			}

			public static class ResultDiv02 extends HtmlDiv {

				@Override
				public void init() {

					addPrefixBinding(context -> {
						Generic scoreUserQuiz = context.getGeneric();

						if (!(scoreUserQuiz.getMeta().equals(scoreUserQuiz.getRoot().find(ScoreUserQuiz.class))))
							return;

						Generic user = scoreUserQuiz.getComponent(0);
						Generic quiz = scoreUserQuiz.getComponent(1);
						Double grade02 = ScoreUtils.calculateDualGrade(context, quiz, user);
						this.setText(context, grade02 + " / 20");
					});
				}
			}
		}
	}
}
