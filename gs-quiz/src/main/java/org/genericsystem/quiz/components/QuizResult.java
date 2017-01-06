package org.genericsystem.quiz.components;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.components.QuizResult.AllResults.ScoreDiv;
import org.genericsystem.quiz.components.QuizResult.AllResults.ScoreDiv.ResultDiv;
import org.genericsystem.quiz.components.QuizResult.QuizResult_;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.utils.QuizExtractors.SCORES_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizExtractors.USER_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizTagSwitcher;
import org.genericsystem.quiz.utils.ScoreUtils;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Switch(QuizTagSwitcher.QUIZ_END.class)
@Children(QuizResult_.class)
@SetText("DIV DE RESULTATS !!!!!!!!!!")
public class QuizResult extends HtmlDiv {

	@Children({ MyResult.class, AllResults.class })
	@SelectContext(SELECTION_SELECTOR.class)
	public static class QuizResult_ extends HtmlDiv implements SelectionDefaults {

	}

	// TODO Créer un affichage des résultats pour tous les users et pour le user connecté

	public static class MyResult extends HtmlDiv {

	}

	@Children(ScoreDiv.class)
	@SetText("Tous les résultats :")
	public static class AllResults extends HtmlDiv {

		@Children({ HtmlDiv.class, HtmlDiv.class, ResultDiv.class })
		@ForEach(SCORES_EXTRACTOR.class)
		@Select(path = HtmlDiv.class, pos = 0, value = USER_EXTRACTOR.class)
		@BindText(path = HtmlDiv.class, pos = 0)
		@BindText(path = HtmlDiv.class, pos = 1)
		//
		@Style(name = "display", value = "flex")
		@Style(name = "flex", value = "1")
		@Style(name = "flex-direction", value = "row")
		public static class ScoreDiv extends HtmlDiv {

			public static class ResultDiv extends HtmlDiv {

				@Override
				public void init() {

					addPrefixBinding(context -> {

						Generic scoreUserQuiz = context.getGeneric();

						if (!(scoreUserQuiz.getMeta().equals(scoreUserQuiz.getRoot().find(ScoreUserQuiz.class)))) {
							return;
						}

						Generic user = scoreUserQuiz.getComponent(0);
						Generic quiz = scoreUserQuiz.getComponent(1);
						Double result = ScoreUtils.calculateSimpleGrade(context, quiz, user);
						System.out.println("RESULTAT ----------------- " + result);
						this.setText(context, " -> " + result + " / 20");
					});
				}
			}
		}
	}
}
