package org.genericsystem.reactor;

import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <T>
 */
public class MetaBinding<N, T extends ObservableList<?>> {

	private final Function<Model, T> applyOnModel;
	private final MetaBinder<N, T> binder;

	public MetaBinding(Function<Model, T> applyOnModel, MetaBinder<N, T> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ViewContext<?, N> viewContext, Element<?, ?> childElement) {
		binder.init(applyOnModel, viewContext, childElement);
	}

	public static <N, M extends Model, SUBMODEL extends Model> MetaBinding<N, ObservableList<SUBMODEL>> forEach(
			Function<M, ObservableList<SUBMODEL>> applyOnModel) {
		return new MetaBinding<>(model -> applyOnModel.apply((M) model), MetaBinder.<N, SUBMODEL> foreachBinder());
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> forEach(Function<M, ObservableList<CompositeModel>> applyOnModel,
			StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		return forEach(model -> {
			ObservableList<CompositeModel> observableList = applyOnModel.apply((M) model);
			Generic[] generics = ((M) model).getGenerics();
			Bindings.bindContent(observableList, new Transformation2<Generic, CompositeModel>(observableListExtractor.apply(generics),
					generic -> constructor.build(CompositeModel.addToGenerics(generic, generics), stringExtractor)));
			return observableList;
		});
	}

	public static <N, M extends Model, T extends Model> MetaBinding<N, ObservableList<T>> selector(Function<M, ObservableValue<T>> applyOnModel) {
		Function<M, ObservableList<T>> applyOnModelList = model -> {
			ObservableValue<T> observableValue = applyOnModel.apply(model);
			return new ListBinding<T>() {
				{
					bind(observableValue);
				}

				@Override
				protected ObservableList<T> computeValue() {
					T value = observableValue.getValue();
					return value != null ? FXCollections.singletonObservableList(value) : FXCollections.emptyObservableList();
				}
			};
		};
		return forEach(applyOnModelList);
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> selector(Function<M, Property<CompositeModel>> applyOnModel,
			StringExtractor stringExtractor, Supplier<Generic> generic, ModelConstructor<CompositeModel> constructor) {
		return selector(model -> {
			Property<CompositeModel> property = applyOnModel.apply((M) model);
			property.setValue(constructor.build(CompositeModel.addToGenerics(generic.get(), ((CompositeModel) model).getGenerics()), stringExtractor));
			return property;
		});
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> selector(Function<M, Property<CompositeModel>> applyOnModel,
			StringExtractor stringExtractor, Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		return selector(model -> {
			Property<CompositeModel> property = applyOnModel.apply((M) model);
			Generic[] generics = ((M) model).getGenerics();
			property.setValue(constructor.build(CompositeModel.addToGenerics(generics[0].getRoot().find(genericClass), generics), stringExtractor));
			return property;
		});
	}
}
