package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;

public interface SwitchDefaults extends ContextProperty {

	public static final String INDEX = "index";
	public static final String INSTANCE_NAME_TAG = "instanceNameTag";
	public static final String SWITCHED_TAG = "switchedTag";

	default Tag getInstanceNameTag(Context context) {
		return this.<Tag> getProperty(INSTANCE_NAME_TAG, context).getValue();
	}

	default Tag getSwitchedTag(Context context) {
		return this.<Tag> getProperty(SWITCHED_TAG, context).getValue();
	}

	default Property<Integer> getIteratorIndexProperty(Context context) {
		return getProperty(INDEX, context);
	}

	default void switcher_(Tag switchedTag, ObservableListExtractor observableListExtractor, Tag instanceNameTag) {
		addPrefixBinding(context -> {
			createNewInitializedProperty(INDEX, context, m -> -1);
			createNewInitializedProperty(INSTANCE_NAME_TAG, context, c -> instanceNameTag);
			createNewInitializedProperty(SWITCHED_TAG, context, c -> switchedTag);
		});
		instanceNameTag.addStyle("display", "flex");
		switchedTag.forEach(observableListExtractor);
		switchedTag.addPrefixBinding(context -> switchedTag.getDomNodeStyles(context).put("display", "none"));
	}

	default void next(Context context) {
		Property<Integer> index = getIteratorIndexProperty(context);
		Tag switchedTag = getSwitchedTag(context);
		ObservableList<Context> contexts = context.getSubContexts(switchedTag);
		if (contexts == null)
			contexts = context.getParent().getSubContexts(switchedTag);
		if (index.getValue() == -1) {
			index.setValue(0);
			getInstanceNameTag(context).addStyle(context, "display", "none");
			switchedTag.addStyle(contexts.get(0), "display", "flex");
		} else if (index.getValue() + 1 < contexts.size()) {
			switchedTag.addStyle(contexts.get(index.getValue()), "display", "none");
			switchedTag.addStyle(contexts.get(index.getValue() + 1), "display", "flex");
			index.setValue(index.getValue() + 1);
		}
	}

	default void prev(Context context) {
		Property<Integer> index = getIteratorIndexProperty(context);
		Tag switchedTag = getSwitchedTag(context);
		ObservableList<Context> contexts = context.getSubContexts(switchedTag);
		if (contexts == null)
			contexts = context.getParent().getSubContexts(getSwitchedTag(context));
		if (index.getValue() > 0) {
			switchedTag.addStyle(contexts.get(index.getValue()), "display", "none");
			switchedTag.addStyle(contexts.get(index.getValue() - 1), "display", "flex");
			index.setValue(index.getValue() - 1);
		} else if (index.getValue() == 0) {
			index.setValue(-1);
			getInstanceNameTag(context).addStyle(context.getParent(), "display", "flex");
			switchedTag.addStyle(contexts.get(0), "display", "none");
		}
	}
}
