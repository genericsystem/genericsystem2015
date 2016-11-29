package ihm;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.model.Answer;
import org.genericsystem.quiz.model.Question;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitleDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlCheckBox;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;

import ihm.QuestionDiv.Empty;
import ihm.QuestionDiv.FooterDiv;
import ihm.QuestionDiv.FooterDiv.FinishBtn;
import ihm.QuestionDiv.FooterDiv.NextBtn;
import ihm.QuestionDiv.FooterDiv.PreviousBtn;
import ihm.QuestionDiv.UnitDiv;
import javafx.collections.ObservableList;
import utils.StepperBis;

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
@DirectSelect(Question.class)
@ForEach(path = UnitDiv.class, value = ObservableListExtractor.SUBINSTANCES.class)
@BindText(path = { UnitDiv.class, TitleDiv.class, HtmlH2.class })
@Stepper(switchClass = UnitDiv.class, headerClass = Empty.class)
public class QuestionDiv extends HtmlDiv implements StepperDefaults {

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

	@Children({ HtmlCheckBox.class, HtmlLabel.class })
	//
	@Style(name = "min-width", value = "174px")
	@Style(name = "display", value = "flex")
	@Style(name = "flex-wrap", value = "nowrap")
	@Style(name = "flex-direction", value = "row")
	@Style(path = HtmlCheckBox.class, name = "margin-right", value = "10px")
	@Style(path = HtmlCheckBox.class, name = "min-height", value = "15px")
	@Style(path = HtmlCheckBox.class, name = "min-width", value = "15px")
	@Style(path = HtmlLabel.class, name = "margin-top", value = "10px")
	@Style(path = HtmlLabel.class, name = "margin-bottom", value = "10px")
	@Style(path = HtmlLabel.class, name = "word-wrap", value = "break-word")
	@Style(path = HtmlLabel.class, name = "hyphens", value = "auto")
	@Style(path = HtmlLabel.class, name = "max-width", value = "100%")
	@StyleClass("width-35-35-90")
	@StyleClass(path = HtmlLabel.class, value = "vertical-align")
	@StyleClass(path = HtmlCheckBox.class, value = "vertical-align")
	//
	@ForEach(ANSWERS_EXTRACTOR.class)
	@BindText(path = HtmlLabel.class)
	public static class AnswerDiv extends HtmlDiv {

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
		public static class NextBtn extends HtmlButton implements StepperBis {

		}

		@SetText("< Previous")
		//
		@Style(name = "display", value = "none")
		@Style(name = "text-align", value = "center")
		//
		@BindAction(PREVIOUS_TAG.class)
		public static class PreviousBtn extends HtmlButton implements StepperBis {

		}

		@SetText("Finish")
		//
		@Style(name = "display", value = "none")
		@Style(name = "text-align", value = "center")
		public static class FinishBtn extends HtmlButton implements StepperBis {

		}

	}

	public static class NEXT_TAG implements ContextAction {
		@Override
		public void accept(Context context, Tag tagNext) {
			if (StepperBis.class.isAssignableFrom(tagNext.getClass())) {

				// Réussir à insérer les tags de navigation (PREVIOUS, NEXT, FINISH)

				Tag tag = tagNext.getParent();
				Tag tagPrevious = tag.find(PreviousBtn.class);
				Tag tagFinish = tag.find(FinishBtn.class);

				((StepperBis) tagNext).next(context, tagNext, tagPrevious, tagFinish);

			} else
				log.warn("The NEXT action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class PREVIOUS_TAG implements ContextAction {
		@Override
		public void accept(Context context, Tag tagPrevious) {
			if (StepperBis.class.isAssignableFrom(tagPrevious.getClass())) {

				Tag tag = tagPrevious.getParent();
				Tag tagNext = tag.find(NextBtn.class);
				Tag tagFinish = tag.find(FinishBtn.class);

				((StepperBis) tagPrevious).prev(context, tagNext, tagPrevious, tagFinish);

			} else
				log.warn("The PREVIOUS action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class ANSWERS_EXTRACTOR implements ObservableListExtractor {
		// generics[0] est l'element courant.
		// Le getRoot permet d'utiliser la methode find (Le root donne accès à tous les éléments du context)
		// TODO Rendre la méthode générique -> lui faire trouver les enfants d'un generic ssi il y a un unique enfant
		// OU
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			return generics[0].getObservableHolders(generics[0].getRoot().find(Answer.class));
		}
	}
}
