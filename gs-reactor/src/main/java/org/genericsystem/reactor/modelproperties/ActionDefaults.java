package org.genericsystem.reactor.modelproperties;

import java.util.function.Consumer;

import org.genericsystem.reactor.Model;

public interface ActionDefaults<M extends Model> extends ModelProperty<M> {

	public static final String ACTION = "action";

	default void bindAction(Consumer<M> applyOnModel) {
		createNewInitializedProperty("action", model -> (Consumer<Object>) o -> applyOnModel.accept(model));
	}

	default Consumer<Object> getAction(Model model) {
		return (Consumer<Object>) getProperty(ACTION, model).getValue();
	}
}
