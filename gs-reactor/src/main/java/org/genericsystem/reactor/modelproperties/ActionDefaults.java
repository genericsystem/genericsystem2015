package org.genericsystem.reactor.modelproperties;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;

public interface ActionDefaults extends ModelProperty {

	public static final String ACTION = "action";

	default void bindAction(Consumer<Context> applyOnModel) {
		createNewInitializedProperty("action", model -> (Consumer<Object>) o -> applyOnModel.accept(model));
	}

	default Consumer<Object> getAction(Context model) {
		return (Consumer<Object>) getProperty(ACTION, model).getValue();
	}
}
