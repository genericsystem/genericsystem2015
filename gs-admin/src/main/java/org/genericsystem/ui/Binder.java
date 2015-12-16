package org.genericsystem.ui;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public interface Binder<N, W> {

	default void init(Supplier<W> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
		init(applyOnModel.get(), modelContext, viewContext, childElement);
	}

	void init(W wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement);

	public static <N, W extends Event> Binder<N, W> actionBinder(Function<N, ObjectProperty<EventHandler<W>>> applyOnNode) {
		return new Binder<N, W>() {
			@Override
			public void init(Supplier<W> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).set(event -> applyOnModel.get());
			}

			@Override
			public void init(W wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
			}
		};
	}

	public static <N, W> Binder<N, Property<W>> injectBinder() {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				wrapper.setValue(modelContext.getParent().getModel());
				// ModelContext modelContext_ = modelContext;
				// while (modelContext_ != null) {
				// if (parentModelClass.isInstance(modelContext_.getModel())) {
				// wrapper.setValue(modelContext_.getModel());
				// return;
				// }
				// modelContext_ = modelContext_.getParent();
				// }
				// throw new IllegalStateException("Can't inject : " + parentModelClass);
			}
		};

	}

	public static <N, SUPERMODEL, W extends Event> Binder<N, Function<SUPERMODEL, W>> metaActionBinder(Function<N, ObjectProperty<EventHandler<W>>> applyOnNode) {
		return new Binder<N, Function<SUPERMODEL, W>>() {
			@Override
			public void init(Supplier<Function<SUPERMODEL, W>> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).set(event -> {
					applyOnModel.get().apply(modelContext.getParent() != null ? modelContext.getParent().getModel() : null);
				});
			}

			@Override
			public void init(Function<SUPERMODEL, W> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
			}
		};
	}

	public static <N, SUPERMODEL, W> Binder<N, Function<W, SUPERMODEL>> pushModelActionOnSuperModel(Function<N, ObjectProperty<Consumer<W>>> applyOnNode) {
		return new Binder<N, Function<W, SUPERMODEL>>() {
			@Override
			public void init(Supplier<Function<W, SUPERMODEL>> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).set(w -> {
					applyOnModel.get().apply(w);
				});
			}

			@Override
			public void init(Function<W, SUPERMODEL> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
			}
		};
	}

	public static <N, W> Binder<N, W> genericActionBinder(Function<N, ObjectProperty<W>> applyOnNode) {
		return new Binder<N, W>() {
			@Override
			public void init(Supplier<W> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).set((W) (EventHandler) event -> applyOnModel.get());
			}

			@Override
			public void init(W wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
			}
		};

	}

	public static <N, W> Binder<N, Property<W>> propertyReverseBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				wrapper.bind(applyOnNode.apply(viewContext.getNode()));
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<W>> propertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).bind(wrapper);
			}
		};
	}

	public static <N, W> Binder<N, ObservableList<W>> observableListPropertyBinder(Function<N, Property<ObservableList<W>>> applyOnNode) {
		return new Binder<N, ObservableList<W>>() {
			@Override
			public void init(ObservableList<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).setValue(wrapper);
			}
		};
	}

	public static <N, W> Binder<N, Property<W>> propertyBiDirectionalBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<N, Property<W>>() {
			@Override
			public void init(Property<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				applyOnNode.apply(viewContext.getNode()).bindBidirectional(wrapper);
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<Boolean>> observableListBinder(Function<N, ObservableList<W>> applyOnNode, W styleClass) {
		return new Binder<N, ObservableValue<Boolean>>() {
			@Override
			public void init(ObservableValue<Boolean> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				ObservableList<W> styleClasses = applyOnNode.apply(viewContext.getNode());
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

	public static <N, W> Binder<N, ObservableList<W>> foreachBinder() {
		return new Binder<N, ObservableList<W>>() {

			@Override
			public void init(ObservableList<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {

				List<ModelContext> children = modelContext.getChildren(childElement);

				class ForEachList extends AbstractList<W> implements ListChangeListener<W> {
					{
						addAll(wrapper);
					}

					@SuppressWarnings("unchecked")
					@Override
					public W get(int index) {
						return (W) children.get(index).getModel();
					}

					@Override
					public int size() {
						return children.size();
					}

					@SuppressWarnings("unchecked")
					@Override
					public void add(int index, W model) {
						ModelContext childContext = new ModelContext(modelContext, childElement, model);
						new ViewContext(viewContext, childContext, childElement, model);
						children.add(index, childContext);
					}

					@Override
					public W set(int index, W element) {
						W remove = remove(index);
						add(index, element);
						return remove;
					}

					@Override
					public W remove(int index) {
						ModelContext removed = children.remove(index);
						for (ViewContext<?> internalViewContext : removed.getViewContexts())
							internalViewContext.destroyChild();
						return removed.getModel();
					}

					@Override
					public void onChanged(Change<? extends W> change) {
						while (change.next()) {
							if (change.wasPermutated()) {
								subList(change.getFrom(), change.getTo()).clear();
								addAll(change.getFrom(), change.getList().subList(change.getFrom(), change.getTo()));
							} else {
								if (change.wasRemoved())
									subList(change.getFrom(), change.getFrom() + change.getRemovedSize()).clear();
								if (change.wasAdded())
									addAll(change.getFrom(), change.getAddedSubList());
							}
						}
					}
				}
				wrapper.addListener(new ForEachList());
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<W>> selectorBinder() {
		return new Binder<N, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				List<ModelContext> children = modelContext.getChildren(childElement);
				Consumer<W> consumer = (newModel) -> {
					if (newModel != null) {
						ModelContext childContext = new ModelContext(modelContext, childElement, newModel);
						new ViewContext(viewContext, childContext, childElement, newModel);
						children.add(childContext);
						assert children.size() == 1;
					}
				};
				wrapper.addListener((o, oldModel, newModel) -> {
					if (oldModel == newModel)
						return;
					if (oldModel != null) {
						ModelContext removed = children.remove(0);
						for (ViewContext<?> internalViewContext : removed.getViewContexts())
							internalViewContext.destroyChild();
					}
					if (newModel != null)
						consumer.accept(newModel);
				});
				consumer.accept(wrapper.getValue());
			}
		};
	}
}
