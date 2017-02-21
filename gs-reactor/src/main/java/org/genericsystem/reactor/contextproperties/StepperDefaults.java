package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;

public interface StepperDefaults extends ContextProperty {

	public static final String INDEX = "index";
	public static final String INSTANCE_NAME_TAG = "instanceNameTag";
	public static final String STEPPER_TAG = "steppedTag";

	default Tag getInstanceNameTag(Context context) {
		return this.<Tag> getContextProperty(INSTANCE_NAME_TAG, context).getValue();
	}

	default Tag getStepperTag(Context context) {
		return this.<Tag> getContextProperty(STEPPER_TAG, context).getValue();
	}

	default Property<Integer> getIteratorIndexProperty(Context context) {
		return getContextProperty(INDEX, context);
	}

	default void stepper(Tag switchedTag, Tag instanceNameTag) {
		addPrefixBinding(context -> {
			createNewInitializedProperty(INDEX, context, -1);
			createNewInitializedProperty(INSTANCE_NAME_TAG, context, instanceNameTag);
			createNewInitializedProperty(STEPPER_TAG, context, switchedTag);
		});
		instanceNameTag.addStyle("display", "flex");
		switchedTag.addPostfixBinding(context -> switchedTag.getDomNodeStyles(context).put("display", "none"));
	}

	default void next(Context context) {
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
	}

	default void prev(Context context) {
		Property<Integer> index = getIteratorIndexProperty(context);
		Tag stepperTag = getStepperTag(context);
		ObservableList<Context> contexts = context.getSubContexts(stepperTag);
		if (contexts == null)
			contexts = context.getParent().getSubContexts(getStepperTag(context));
		if (index.getValue() > 0) {
			stepperTag.addStyle(contexts.get(index.getValue()), "display", "none");
			stepperTag.addStyle(contexts.get(index.getValue() - 1), "display", "flex");
			index.setValue(index.getValue() - 1);
		} else if (index.getValue() == 0) {
			index.setValue(-1);
			getInstanceNameTag(context).addStyle(context.getParent(), "display", "flex");
			stepperTag.addStyle(contexts.get(0), "display", "none");
		}
	}
}
