package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends CompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Conf, InstanceRowModel> rowBuilder, Function<Conf, CompositeModel<GenericModel>> cellBuilder,
			Function<Generic, GenericModel> subCellBuilder) {
		this(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractor, rowBuilder, cellBuilder, subCellBuilder);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, ObservableList<Generic> attributes) {
		this(generic, stringExtractor, type -> attributes, InstanceRowModel::new, CompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Conf, InstanceRowModel> rowBuilder,
			Function<Conf, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		super(new Conf(generic, stringExtractor, typ -> typ.getObservableSubInstances(), instance -> rowBuilder.apply(new Conf(instance, observableListExtractor, attribute -> cellBuilder.apply(new Conf(instance, inst -> inst
				.getObservableHolders(attribute), subCellBuilder))))));
	}

	public static Conf build(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Conf, InstanceRowModel> rowBuilder,
			Function<Conf, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		return (new Conf(generic, stringExtractor, typ -> typ.getObservableSubInstances(), instance -> rowBuilder.apply(new Conf(instance, observableListExtractor, attribute -> cellBuilder.apply(new Conf(instance, inst -> inst
				.getObservableHolders(attribute), subCellBuilder))))));
	}

}
