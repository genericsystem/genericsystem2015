package org.genericsystem.reactor.aa_modelproperties;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;

public interface ActionDefaults extends ContextProperty {

	public static final String ACTION = "action";

	default void bindAction(Consumer<Context> applyOnModel) {
		createNewInitializedProperty("action", model -> (Consumer<Object>) o -> applyOnModel.accept(model));
	}

	default Consumer<Object> getAction(Context model) {
		return (Consumer<Object>) getProperty(ACTION, model).getValue();
	}
}
