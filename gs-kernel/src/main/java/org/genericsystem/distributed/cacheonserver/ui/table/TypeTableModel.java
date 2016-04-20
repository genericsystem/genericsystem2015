package org.genericsystem.distributed.cacheonserver.ui.table;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericModel {

	private final ObservableValue<TitleRowModel> titleRowModel;
	private final ObservableValue<InsertRowModel> insertRowModel;
	private final ObservableList<InstanceRowModel> instanceModels;

	public TypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type);
		titleRowModel = new ReadOnlyObjectWrapper<>(getTitleRowBuilder().apply(type, attributes, getTitleCellBuilder()));
		insertRowModel = new ReadOnlyObjectWrapper<>(getInsertRowBuilder().apply(type, attributes, getInsertCellBuilder()));
		instanceModels = new Transformation2<Generic, InstanceRowModel>(type.getObservableSubInstances(), instance -> getInstanceRowBuilder().apply(instance, attributes,
				attribute -> getInstanceCellBuilder().apply(instance, attribute, getHolderSubCellBuilder())) /* , carModel -> new Observable[] { CarModel.() } */);

	}

	protected TriFunction<Generic, ObservableList<Generic>, Function<Generic, TitleCellModel>, TitleRowModel> getTitleRowBuilder() {
		return TitleRowModel::new;
	}

	protected TriFunction<Generic, ObservableList<Generic>, Function<Generic, AttributeCellModel>, InsertRowModel> getInsertRowBuilder() {
		return InsertRowModel::new;
	}

	protected TriFunction<Generic, ObservableList<Generic>, Function<Generic, InstanceAttributeCellModel>, InstanceRowModel> getInstanceRowBuilder() {
		return InstanceRowModel::new;
	}

	protected Function<Generic, TitleCellModel> getTitleCellBuilder() {
		return TitleCellModel::new;
	}

	protected Function<Generic, AttributeCellModel> getInsertCellBuilder() {
		return AttributeCellModel::new;
	}

	protected TriFunction<Generic, Generic, Function<Generic, HolderSubCellModel>, InstanceAttributeCellModel> getInstanceCellBuilder() {
		return InstanceAttributeCellModel::new;
	}

	private Function<Generic, HolderSubCellModel> getHolderSubCellBuilder() {
		return HolderSubCellModel::new;
	}

	@FunctionalInterface
	public interface TriFunction<T, U, V, R> {
		R apply(T t, U u, V v);
	}

	/**********************************************************************/

	public ObservableValue<TitleRowModel> getTitleRowModel() {
		return titleRowModel;
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

	public ObservableList<InstanceRowModel> getInstanceModels() {
		return instanceModels;
	}

	public ObservableValue<String> getTableString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)" + " Management");
	}
}
