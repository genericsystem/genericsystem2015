package org.genericsystem.quiz.components;

import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.AnswerDiv.QuizCheckBox;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.Empty;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.FinishBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.NextBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.PreviousBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.UnitDiv;
import org.genericsystem.quiz.model.UserAnswer;
import org.genericsystem.quiz.utils.QuizContextAction.NEXT_TAG;
import org.genericsystem.quiz.utils.QuizContextAction.PREVIOUS_TAG;
import org.genericsystem.quiz.utils.QuizContextAction.SAVE_QUIZ_RESULT;
import org.genericsystem.quiz.utils.QuizExtractors.ANSWERS_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizExtractors.QUESTIONS_EXTRACTOR;
import org.genericsystem.quiz.utils.QuizStepper;
import org.genericsystem.quiz.utils.QuizTagSwitcher;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitleDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;

@Switch({ TagSwitcher.LOGGED_USER.class, QuizTagSwitcher.QUIZ_RUNNING.class })
@Children(QuestionDiv_.class)
public class QuestionDiv extends HtmlDiv {
	//
	@Children({ UnitDiv.class, FooterDiv.class, Empty.class })
	//
	@Style(name = "display", value = "flex")
	@Style(name = "flex", value = "1")
	@Style(name = "flex-direction", value = "column")
	@Style(name = "padding", value = "10px")
	@Style(name = "background-color", value = "#DCDCDC")
	@Style(path = { UnitDiv.class, TitleDiv.class }, name = "background-color", value = "#708090")
	@Style(path = { UnitDiv.class, TitleDiv.class }, name = "padding", value = "10px")
	@Style(path = { UnitDiv.class, TitleDiv.class }, name = "margin", value = "10px")
	//
	@ForEach(path = UnitDiv.class, value = QUESTIONS_EXTRACTOR.class)
	@BindText(path = { UnitDiv.class, TitleDiv.class, HtmlH2.class })
	@SelectContext(SELECTION_SELECTOR.class)
	@Stepper(switchClass = UnitDiv.class, headerClass = Empty.class)
	public static class QuestionDiv_ extends HtmlDiv implements StepperDefaults, SelectionDefaults {

		// Le SET_SELECTOR mount le cache. L'init permet de sauvegarder le context et travailler sur le cache du niveau inférieur
		// @Override
		// public void init() {
		// addPrefixBinding(context -> {
		// context.flush();
		// context.unmount();
		// });
		// }

		// Créé une propriété qui stockera les réponses de l'utilisateur
		// @Override
		// public void init() {
		// createNewInitializedProperty("userResponse", context -> new HashMap<Generic, Boolean>());
		// }

		// TODO Remplacer cette classe (temporaire, afin de faire fonctionner le stepper)
		// par une autre Div affichant une information non steppable
		// Ou faire évoluer le Stepper afin qu'il puisse prendre uniquement la switchClass en paramètre
		public static class Empty extends HtmlDiv {

		}

		@Children({ TitleDiv.class, AnswersDiv.class })
		//
		@Style(name = "display", value = "flex")
		@Style(name = "flex", value = "1")
		@Style(name = "min-height", value = "100%")
		@Style(name = "flex-direction", value = "column")
		@Style(path = HtmlDiv.class, name = "border-radius", value = "10px")
		public static class UnitDiv extends HtmlDiv {

		}

		@Children(AnswerDiv.class)
		//
		@Style(name = "display", value = "flex")
		@Style(name = "flex-wrap", value = "wrap")
		@Style(name = "justify-content", value = "space-around")
		@Style(name = "margin", value = "10px")
		@Style(name = "border", value = "1px solid silver")
		@Style(name = "align-items", value = "center")
		@StyleClass("fdirection-r-r-c")
		public static class AnswersDiv extends HtmlDiv {

		}

		@Children({ QuizCheckBox.class, HtmlLabel.class })
		//
		@Style(name = "min-width", value = "174px")
		@Style(name = "display", value = "flex")
		@Style(name = "flex-wrap", value = "nowrap")
		@Style(name = "flex-direction", value = "row")
		@Style(path = QuizCheckBox.class, name = "margin-right", value = "10px")
		@Style(path = QuizCheckBox.class, name = "min-height", value = "15px")
		@Style(path = QuizCheckBox.class, name = "min-width", value = "15px")
		@Style(path = HtmlLabel.class, name = "margin-top", value = "10px")
		@Style(path = HtmlLabel.class, name = "margin-bottom", value = "10px")
		@Style(path = HtmlLabel.class, name = "word-wrap", value = "break-word")
		@Style(path = HtmlLabel.class, name = "hyphens", value = "auto")
		@Style(path = HtmlLabel.class, name = "max-width", value = "100%")
		@StyleClass("width-35-35-90")
		@StyleClass(path = HtmlLabel.class, value = "vertical-align")
		@StyleClass(path = QuizCheckBox.class, value = "vertical-align")
		//
		@ForEach(ANSWERS_EXTRACTOR.class)
		@BindText(path = HtmlLabel.class)
		public static class AnswerDiv extends HtmlDiv {

			@Override
			public void init() {
				// Mets un for entre un label et une checkbox
				// Les 2 Tags doivent être des children de la meme div parente et etre des enfant directs de cette div
				addPostfixBinding(context -> {
					List<HtmlDomNode> nodes = context.getHtmlDomNode(this).getChildren();
					String idTag = null;
					Tag label = null;

					for (HtmlDomNode n : nodes) {
						if (n.getTag() instanceof QuizCheckBox)
							idTag = n.getId();
						if (n.getTag() instanceof HtmlLabel)
							label = n.getTag();
					}

					if (label != null && idTag != null)
						label.addAttribute(context, "for", idTag);

				});

			}

			public static class QuizCheckBox extends CheckBoxWithValue {

				@Override
				public void init() {

					// affiche les réponses précédentes du User logged
					initValueProperty(context -> {

						Generic userA = context.getGeneric().getLink(context.find(UserAnswer.class), getLoggedUserProperty(context).getValue());

						if (userA == null)
							userA = context.getGeneric().setLink(context.find(UserAnswer.class), false, getLoggedUserProperty(context).getValue());

						return (Boolean) userA.getValue();

					});

					// Place un listener sur la checkbox qui écoute l'état de la checkbox et modifie la valeur de UserAnswer en conséquence
					addConvertedValueChangeListener((context, nva) -> context.getGeneric().getLink(context.find(UserAnswer.class), getLoggedUserProperty(context).getValue()).updateValue(nva));
				}
			}

		}

		@Children({ PreviousBtn.class, NextBtn.class, FinishBtn.class })
		//
		@Style(name = "display", value = "flex")
		@Style(name = "justify-content", value = "space-around")
		@Style(name = "align-self", value = "flex-end")
		@Style(name = "width", value = "100%")
		@StyleClass(path = HtmlButton.class, value = "monitorButton")
		public static class FooterDiv extends HtmlDiv {

			@SetText("Next >")
			//
			@Style(name = "text-align", value = "center")
			//
			@BindAction(NEXT_TAG.class)
			public static class NextBtn extends HtmlButton implements QuizStepper {

			}

			@SetText("< Previous")
			//
			@Style(name = "display", value = "none")
			@Style(name = "text-align", value = "center")
			//
			@BindAction(PREVIOUS_TAG.class)
			public static class PreviousBtn extends HtmlButton implements QuizStepper {

			}

			@SetText("Finish")
			//
			@Style(name = "display", value = "none")
			@Style(name = "text-align", value = "center")
			//
			@BindAction({ SAVE_QUIZ_RESULT.class })
			public static class FinishBtn extends HtmlButton implements QuizStepper {

			}

		}

	}
}
