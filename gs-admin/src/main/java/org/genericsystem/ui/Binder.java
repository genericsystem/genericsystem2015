package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;

public interface Binder<N, X, Y> {

	default <T> Supplier<T> applyOnModel(Function<?, T> methodReference, ModelContext modelContext) {
		return () -> {
			ModelContext modelContext_ = modelContext;
			String s = "/";
			while (modelContext_ != null) {
				s += modelContext_.getModel() + "/";
				try {
					return methodReference.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {
				}
				modelContext_ = modelContext_.getParent();

			}
			throw new IllegalStateException("Unable to resolve a method reference : " + methodReference + " on stack : " + s);
		};
	}

	default void init(Function<N, Y> applyOnNode, Function<?, X> method, ModelContext modelContext, N node) {
		init(applyOnNode.apply(node), applyOnModel(method, modelContext), modelContext);
	}

	default void init(Y nodeResult, Supplier<X> applyOnModel, ModelContext modelContext) {
		init(nodeResult, applyOnModel.get());
	}

	default void init(Y nodeResult, X modelResult) {
	}

	public static <N, W, W2> Binder<N, Property<W>, W2> injectBinder() {
		return new Binder<N, Property<W>, W2>() {
			@Override
			public void init(W2 nodeResult, Supplier<Property<W>> applyOnModel, ModelContext modelContext) {
				applyOnModel.get().setValue(modelContext.getParent().getModel());
			}
		};
	}

	public static <N, W, Y> Binder<N, ObservableValue<W>, Property<W>> propertyBinder() {
		return new Binder<N, ObservableValue<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, ObservableValue<W> modelResult) {
				nodeResult.bind(modelResult);
			}
		};
	}

	public static <N, SUPERMODEL, W> Binder<N, Function<SUPERMODEL, ObservableValue<W>>, Property<W>> metaPropertyBinder() {
		return new Binder<N, Function<SUPERMODEL, ObservableValue<W>>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, Supplier<Function<SUPERMODEL, ObservableValue<W>>> applyOnModel, ModelContext modelContext) {
				Property<W> property = nodeResult;
				ModelContext modelContext_ = modelContext.getParent();
				String s = "/";
				while (modelContext_ != null) {
					s += modelContext_.getModel() + "/";
					try {
						property.bind(applyOnModel.get().apply(modelContext_.getModel()));
						return;
					} catch (ClassCastException ignore) {
					}
					modelContext_ = modelContext_.getParent();
				}
				throw new IllegalStateException("Unable to resolve a method reference  on stack : " + s);
			}
		};
	}

	public static <N, W> Binder<N, W, Property<W>> actionBinder() {
		return new Binder<N, W, Property<W>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Property<W> nodeResult, Supplier<W> applyOnModel, ModelContext modelContext) {
				nodeResult.setValue((W) (EventHandler) event -> applyOnModel.get());
			}
		};

	}

	public static <N, SUPERMODEL, W> Binder<N, Function<SUPERMODEL, W>, Property<W>> metaActionBinder() {
		return new Binder<N, Function<SUPERMODEL, W>, Property<W>>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public void init(Property<W> nodeResult, Supplier<Function<SUPERMODEL, W>> applyOnModel, ModelContext modelContext) {
				nodeResult.setValue((W) (EventHandler) event -> {
					ModelContext modelContext_ = modelContext;
					String s = "/";
					while (modelContext_ != null) {
						s += modelContext_.getModel() + "/";
						try {
							applyOnModel.get().apply(modelContext_.getModel());
						} catch (ClassCastException ignore) {
						}
						modelContext_ = modelContext_.getParent();
					}
				});
			}
		};
	}

	public static <N, SUPERMODEL, W> Binder<N, Function<W, SUPERMODEL>, Property<Consumer<W>>> pushModelActionOnSuperModel() {
		return new Binder<N, Function<W, SUPERMODEL>, Property<Consumer<W>>>() {
			@Override
			public void init(Function<N, Property<Consumer<W>>> applyOnNode, Function<?, Function<W, SUPERMODEL>> method, ModelContext modelContext, N node) {
				applyOnNode.apply(node).setValue(w -> applyOnModel(method, modelContext.getModel()).get().apply(w));
				// applyOnNode.apply(node).setValue(w -> applyOnModel.get().apply(w));
				// applyOnNode.apply(node).setValue(w -> method.apply(null));
			}
		};
	}

	public static <N, W> Binder<N, Property<W>, ObservableValue<W>> propertyReverseBinder() {
		return new Binder<N, Property<W>, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> nodeResult, Property<W> modelResult) {
				modelResult.bind(nodeResult);
			}
		};
	}

	public static <N, W> Binder<N, ObservableList<W>, Property<ObservableList<W>>> observableListPropertyBinder() {
		return new Binder<N, ObservableList<W>, Property<ObservableList<W>>>() {
			@Override
			public void init(Property<ObservableList<W>> nodeResult, ObservableList<W> modelResult) {
				nodeResult.setValue(modelResult);
			}
		};
	}

	public static <N, W> Binder<N, Property<W>, Property<W>> propertyBiDirectionalBinder() {
		return new Binder<N, Property<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, Property<W> modelResult) {
				nodeResult.bindBidirectional(modelResult);
			}
		};
	}

	public static <N> Binder<N, ObservableValue<String>, ObservableList<String>> observableListBinder() {
		return new Binder<N, ObservableValue<String>, ObservableList<String>>() {
			@Override
			public void init(ObservableList<String> nodeResult, ObservableValue<String> modelResult) {
				nodeResult.add(modelResult.getValue());
				modelResult.addListener((o, ov, nv) -> {
					nodeResult.remove(ov);
					nodeResult.add(nv);
				});
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<Boolean>, ObservableList<W>> observableListBinder(Function<N, ObservableList<W>> applyOnNode, W styleClass) {
		return new Binder<N, ObservableValue<Boolean>, ObservableList<W>>() {
			@Override
			public void init(ObservableList<W> nodeResult, ObservableValue<Boolean> modelResult) {
				Consumer<Boolean> consumer = bool -> {
					if (bool)
						nodeResult.add(styleClass);
					else
						nodeResult.remove(styleClass);
				};

				consumer.accept(modelResult.getValue());
				modelResult.addListener((o, ov, nv) -> consumer.accept(nv));
			}
		};
	}
}
