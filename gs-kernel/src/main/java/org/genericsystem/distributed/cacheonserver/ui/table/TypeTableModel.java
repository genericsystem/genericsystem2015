package org.genericsystem.distributed.cacheonserver.ui.table;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericModel {

	private final ObservableList<InstanceRowModel> subModels;

	public TypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type);
		Function<Generic, Function<Generic, CompositeModel<GenericModel>>> f2 = instance -> attribute -> getInstanceCellBuilder().apply(transform(instance.getObservableHolders(attribute), getHolderSubCellBuilder()));
		Function<Generic, InstanceRowModel> srcToTarget = instance -> getInstanceRowBuilder().apply(instance, transform(attributes, attribute -> f2.apply(instance).apply(attribute)));
		subModels = transform(type.getObservableSubInstances(), srcToTarget);

	}

	public static <SOURCE, TARGET> ObservableList<TARGET> transform(ObservableList<SOURCE> external, Function<SOURCE, TARGET> srcToTarget) {
		return new Transformation2<>(external, srcToTarget);
	}

	protected BiFunction<Generic, ObservableList<CompositeModel<GenericModel>>, InstanceRowModel> getInstanceRowBuilder() {
		return InstanceRowModel::new;
	}

	protected Function<ObservableList<GenericModel>, CompositeModel<GenericModel>> getInstanceCellBuilder() {
		return CompositeModel<GenericModel>::new;
	}

	private Function<Generic, GenericModel> getHolderSubCellBuilder() {
		return GenericModel::new;
	}

	/**********************************************************************/

	public ObservableList<InstanceRowModel> getSubModels() {
		return subModels;
	}

	public ObservableValue<String> getTableString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)" + " Management");
	}
}
