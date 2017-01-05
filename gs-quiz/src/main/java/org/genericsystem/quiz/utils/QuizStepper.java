package org.genericsystem.quiz.utils;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.contextproperties.StepperDefaults;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;

public interface QuizStepper extends StepperDefaults {

	// Tests -> Mettre un tag (bouton "Next") en argument de méthode, afin de le changer si on arrive a la fin de la liste
	// TODO -> Refaire apparaitre le bouton lorsqu'on appuie sur "Previous"
	default void next(Context context, Tag tagNext, Tag tagPrevious, Tag tagFinish) {
		Property<Integer> index = getIteratorIndexProperty(context);
		Tag stepperTag = getStepperTag(context);
		ObservableList<Context> contexts = context.getSubContexts(stepperTag);

		if (contexts == null)
			contexts = context.getParent().getSubContexts(stepperTag);

		if (index.getValue() == -1) {
			index.setValue(0);
			getInstanceNameTag(context).addStyle(context, "display", "none");
			stepperTag.addStyle(contexts.get(0), "display", "flex");

		} else if (index.getValue() + 1 < contexts.size()) {
			stepperTag.addStyle(contexts.get(index.getValue()), "display", "none");
			stepperTag.addStyle(contexts.get(index.getValue() + 1), "display", "flex");
			index.setValue(index.getValue() + 1);
		}

		if (index.getValue() + 1 == contexts.size()) {
			tagNext.addStyle(context, "display", "none");
			tagFinish.addStyle(context, "display", "initial");
		}

		if (index.getValue() == 0) {
			tagPrevious.addStyle(context, "display", "initial");
		}

	}

	default void prev(Context context, Tag tagNext, Tag tagPrevious, Tag tagFinish) {
		Property<Integer> index = getIteratorIndexProperty(context);
		Tag stepperTag = getStepperTag(context);
		ObservableList<Context> contexts = context.getSubContexts(stepperTag);

		if (contexts == null)
			contexts = context.getParent().getSubContexts(getStepperTag(context));

		if (index.getValue() + 1 == contexts.size()) {
			tagNext.addStyle(context, "display", "initial");
			tagFinish.addStyle(context, "display", "none");
		}

		if (index.getValue() > 0) {
			stepperTag.addStyle(contexts.get(index.getValue()), "display", "none");
			stepperTag.addStyle(contexts.get(index.getValue() - 1), "display", "flex");
			index.setValue(index.getValue() - 1);

		} else if (index.getValue() == 0) {
			index.setValue(-1);
			getInstanceNameTag(context).addStyle(context, "display", "flex");
			stepperTag.addStyle(contexts.get(0), "display", "none");

			tagPrevious.addStyle(context, "display", "none");
		}

	}
}
