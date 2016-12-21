package org.genericsystem.reactor.contextproperties;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface ActionDefaults extends ContextProperty {

	public static final String ACTION = "action";

	default void bindAction(Consumer<Context> applyOnModel) {
		createNewInitializedProperty("action", model -> (Consumer<Object>) o -> applyOnModel.accept(model));
	}

	default void bindAction(Context context, Consumer<Context> applyOnModel) {
		createNewInitializedProperty("action", context, model -> (Consumer<Object>) o -> applyOnModel.accept(model));
	}

	default Property<Consumer<Object>> getActionProperty(Context model) {
		return getProperty(ACTION, model);
	}
}
