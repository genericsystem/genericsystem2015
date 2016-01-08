package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;

public interface Binder<N, W> {

	default <T> Supplier<T> applyOnModel(Function<?, T> methodReference, ModelContext modelContext) {
		return () -> {
			ModelContext modelContext_ = modelContext;
			String s = "/";
			while (modelContext_ != null) {
				s += modelContext_.getModel() + "/";
				try {
					return methodReference.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + methodReference + " on stack : " + s);
		};
	}

	default void init(Function<?, W> method, ModelContext modelContext, N node) {
		init(applyOnModel(method, modelContext), modelContext, node);
	}

	default void init(Supplier<W> applyOnModel, ModelContext modelContext, N node) {
		init(applyOnModel.get(), modelContext, node);
	}

	default void init(W wrapper, ModelContext modelContext, N node) {}

	public static <N, W> Binder<N, Property<W>> injectBinder() {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, N node) {
				wrapper.setValue(modelContext.getParent().getModel());
			}
		};

	}

	public static <N, W> Binder<N, ObservableValue<W>> propertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> wrapper, ModelContext modelContext, N node) {
				applyOnNode.apply(node).bind(wrapper);
			}
		};
	}

	public static <N, SUPERMODEL, W> Binder<N, Function<SUPERMODEL, ObservableValue<W>>> metaPropertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Function<SUPERMODEL, ObservableValue<W>>>() {
			@Override
			public void init(Supplier<Function<SUPERMODEL, ObservableValue<W>>> applyOnModel, ModelContext modelContext, N node) {
				Property<W> property = applyOnNode.apply(node);
				ModelContext modelContext_ = modelContext.getParent();
				String s = "/";
				while (modelContext_ != null) {
					s += modelContext_.getModel() + "/";
					try {
						property.bind(applyOnModel.get().apply(modelContext_.getModel()));
						return;
					} catch (ClassCastException ignore) {}
					modelContext_ = modelContext_.getParent();
				}
				throw new IllegalStateException("Unable to resolve a method reference  on stack : " + s);
			}
		};
	}

	public static <N, T> Binder<N, T> actionBinder(Function<N, Property<T>> applyOnNode) {
		return new Binder<N, T>() {
			@Override
			public void init(Supplier<T> applyOnModel, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue((T) (EventHandler) event -> applyOnModel.get());
			}
		};

	}

	public static <N, SUPERMODEL, W> Binder<N, Function<SUPERMODEL, W>> metaActionBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Function<SUPERMODEL, W>>() {
			@Override
			public void init(Supplier<Function<SUPERMODEL, W>> applyOnModel, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue((W) (EventHandler) event -> applyOnModel.get().apply(modelContext.getParent() != null ? modelContext.getParent().getModel() : null));
			}
		};
	}

	public static <N, SUPERMODEL, W> Binder<N, Function<W, SUPERMODEL>> pushModelActionOnSuperModel(Function<N, Property<Consumer<W>>> applyOnNode) {
		return new Binder<N, Function<W, SUPERMODEL>>() {
			@Override
			public void init(Supplier<Function<W, SUPERMODEL>> applyOnModel, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue(w -> applyOnModel.get().apply(w));
			}
		};
	}

	public static <N, S, W> Binder<N, Function<S, W>> genericMouseActionBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Function<S, W>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Supplier<Function<S, W>> applyOnModel, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue((W) (EventHandler) event -> {
					ModelContext modelContext_ = modelContext;
					String s = "/";
					while (modelContext_ != null) {
						s += modelContext_.getModel() + "/";
						try {
							applyOnModel.get().apply(modelContext_ != null ? modelContext_.getModel() : null);
						} catch (ClassCastException ignore) {}
						modelContext_ = modelContext_.getParent();
					}
				});
			}
		};

	}

	public static <N, W> Binder<N, Property<W>> propertyReverseBinder(Function<N, ObservableValue<W>> applyOnNode) {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, N node) {
				wrapper.bind(applyOnNode.apply(node));
			}
		};
	}

	public static <N, W> Binder<N, ObservableList<W>> observableListPropertyBinder(Function<N, Property<ObservableList<W>>> applyOnNode) {
		return new Binder<N, ObservableList<W>>() {
			@Override
			public void init(ObservableList<W> wrapper, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue(wrapper);
			}
		};
	}

	public static <N, W> Binder<N, Property<W>> propertyBiDirectionalBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, N node) {
				applyOnNode.apply(node).bindBidirectional(wrapper);
			}
		};
	}

	public static <N> Binder<N, ObservableValue<String>> observableListBinder(Function<N, ObservableList<String>> applyOnNode) {
		return new Binder<N, ObservableValue<String>>() {
			@Override
			public void init(ObservableValue<String> wrapper, ModelContext modelContext, N node) {
				ObservableList<String> styleClasses = applyOnNode.apply(node);
				styleClasses.add(wrapper.getValue());
				wrapper.addListener((o, ov, nv) -> {
					styleClasses.remove(ov);
					styleClasses.remove(nv);
				});
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<Boolean>> observableListBinder(Function<N, ObservableList<W>> applyOnNode, W styleClass) {
		return new Binder<N, ObservableValue<Boolean>>() {
			@Override
			public void init(ObservableValue<Boolean> wrapper, ModelContext modelContext, N node) {
				ObservableList<W> styleClasses = applyOnNode.apply(node);
				Consumer<Boolean> consumer = bool -> {
					if (bool)
						styleClasses.add(styleClass);
					else
						styleClasses.remove(styleClass);
				};
				consumer.accept(wrapper.getValue());
				wrapper.addListener((o, ov, nv) -> consumer.accept(nv));
			}
		};
	}
}
