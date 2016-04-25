package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.TriFunction;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericCompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, ObservableList<Generic> attributes) {
		this(generic, stringExtractor, type -> attributes, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		this(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractor, rowBuilder, cellBuilder, subCellBuilder);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			TriFunction<Generic, Function<Generic, ObservableList<Generic>>, Function<Generic, GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		super(generic, stringExtractor, typ -> typ.getObservableSubInstances(), instance -> rowBuilder.apply(instance, observableListExtractor, attribute -> cellBuilder.apply(instance, inst -> inst.getObservableHolders(attribute), subCellBuilder)));
	}
}
