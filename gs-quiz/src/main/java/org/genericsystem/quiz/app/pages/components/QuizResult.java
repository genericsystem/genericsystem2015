package org.genericsystem.quiz.app.pages.components;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.QuizDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.Score01;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.Score02;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.ScoreDiv.UserDiv;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.Search;
import org.genericsystem.quiz.app.pages.components.QuizResult.AllResults.TitleResult;
import org.genericsystem.quiz.app.pages.components.QuizResult.MySumResult.MyResultP;
import org.genericsystem.quiz.app.pages.components.QuizResult.MySumResult.TitleResultH1;
import org.genericsystem.quiz.app.pages.components.QuizResult.SummaryResults;
import org.genericsystem.quiz.model.ScoreUserQuiz;
import org.genericsystem.quiz.utils.QuizContextAction;
import org.genericsystem.quiz.utils.QuizExtractors.QUIZ_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizExtractors.SCORES_FILTERED;
import org.genericsystem.quiz.utils.QuizExtractors.USER_EXTRACTOR;
import org.genericsystem.quiz.utils.ScoreUtils;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEachContext;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;

import javafx.collections.MapChangeListener;

@Children({ SummaryResults.class, AllResults.class })
@Style(name = "align-items", value = "center")
public class QuizResult extends FlexDiv {

	// Pour s'afficher, SummaryResult a besoin d'un Quiz en context. Il est utilisé pour afficher les scores d'un utilisateur après qu'il ait passé un quiz
	@Children({ MySumResult.class })
	@SelectContext(SELECTION_SELECTOR.class)
	@Style(name = "width", value = "100%")
	@Style(name = "align-items", value = "center")
	public static class SummaryResults extends FlexDiv implements SelectionDefaults {

	}

	@Children({ TitleResultH1.class, MyResultP.class })
	//
	@Style(name = "width", value = "90%")
	@StyleClass("myResultDiv")
	public static class MySumResult extends FlexDiv {

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
					String point = grade02 > 1 ? " points" : " point";

					this.setText(context, "Vous venez de terminer le quiz.\nVotre Score est de " + grade02 + point + "\nVous avez eu " + scoreUserQuiz + goodAnswer);
				});
			}
		}
	}

	@DirectSelect(ScoreUserQuiz.class)
	@Children({ HtmlDiv.class, TitleResult.class, ScoreDiv.class })
	@Children(path = HtmlDiv.class, pos = 0, value = Search.class)
	//
	@Style(name = "width", value = "90%")
	@Style(path = FlexDiv.class, name = "flex", value = "1")
	@Style(path = { FlexDiv.class, FlexDiv.class }, name = "flex", value = "1")
	@Style(path = HtmlDiv.class, pos = 0, name = "text-align", value = "right")
	@Style(path = HtmlDiv.class, pos = 0, name = "margin-bottom", value = "15px")
	@Style(path = TitleResult.class, name = "min-height", value = "40px")
	@StyleClass(path = TitleResult.class, value = "titleTableDivQ")
	@StyleClass(path = { TitleResult.class, FlexDiv.class }, value = "titleTableCellQ")
	@StyleClass(path = { ScoreDiv.class, FlexDiv.class }, value = "scoreTableCellQ")
	@Style(path = { ScoreDiv.class, UserDiv.class }, name = "border-left", value = "0.5px solid grey")
	@Style(path = { ScoreDiv.class, QuizDiv.class }, name = "border-right", value = "0.5px solid grey")
	//
	// L'étape suivant est de se passer du bouton "Rechercher" et permettre une recherche active pendant que l'on tape le texte.
	@SetText(path = { HtmlDiv.class, HtmlButton.class }, value = "Rechercher")
	public static class AllResults extends FlexDiv {

		@Attribute(name = "placeholder", value = "Entrer un nom d'utilisateur")
		@StyleClass({ "inputTextQ", "inputTextSearchQ" })
		public static class Search extends HtmlInputText {

			@Override
			public void init() {
				addPrefixBinding(context -> {
					this.getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {

						if ("value".equals(change.getKey())) {
							if (change.wasAdded())
								getProperty(QuizContextAction.SELECTED_USER, context).setValue(change.getValueAdded());
						}

					});
				});
			}
		}

		@Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class, FlexDiv.class, FlexDiv.class })
		@SetText(path = FlexDiv.class, pos = 0, value = "Pseudo")
		@SetText(path = FlexDiv.class, pos = 1, value = "Réponses correctes")
		@SetText(path = FlexDiv.class, pos = 2, value = "Score 1")
		@SetText(path = FlexDiv.class, pos = 3, value = "Score 2")
		@SetText(path = FlexDiv.class, pos = 4, value = "Quiz")
		public static class TitleResult extends FlexRow {

		}

		@ForEachContext(SCORES_FILTERED.class)
		@Children({ UserDiv.class, FlexDiv.class, Score01.class, Score02.class, QuizDiv.class })
		@Select(path = UserDiv.class, value = USER_EXTRACTOR.class)
		@Select(path = QuizDiv.class, value = QUIZ_EXTRACTOR.class)
		@BindText(path = UserDiv.class)
		@BindText(path = HtmlDiv.class, pos = 1)
		@BindText(path = QuizDiv.class)
		public static class ScoreDiv extends FlexRow {

			@Override
			public void init() {
				addPrefixBinding(context -> {

					if (context.getGeneric().getComponent(0).equals(getLoggedUserProperty(context).getValue())) {
						this.addStyle(context, "background-color", "#FFE9C5");
						this.addStyle(context, "font-weight", "bold");
					}

					getLoggedUserProperty(context).addListener((observable, oldValue, newValue) -> {
						if (!context.isDestroyed()) {

							if (newValue == null) {
								this.addStyle(context, "background-color", "inherit");
								this.addStyle(context, "font-weight", "normal");
							}

							if (newValue != null && newValue.equals(context.getGeneric().getComponent(0))) {
								this.addStyle(context, "background-color", "#FFE9C5");
								this.addStyle(context, "font-weight", "bold");
							}

						}
					});

				});
			}

			public static class UserDiv extends FlexDiv {

			}

			public static class Score01 extends FlexDiv {

				@Override
				public void init() {
					addPrefixBinding(context -> {
						Double grade = ScoreUtils.calculateSimpleGrade(context, context.getGeneric().getComponent(1), context.getGeneric().getComponent(0));
						this.setText(context, grade + " / 20");
					});
				}
			}

			public static class Score02 extends FlexDiv {

				@Override
				public void init() {

					addPrefixBinding(context -> {
						Double grade01 = ScoreUtils.calculateDualGrade(context, context.getGeneric().getComponent(1), context.getGeneric().getComponent(0));
						this.setText(context, grade01 + " / 20 ");
					});
				}
			}

			public static class QuizDiv extends FlexDiv {

			}
		}

	}
}
