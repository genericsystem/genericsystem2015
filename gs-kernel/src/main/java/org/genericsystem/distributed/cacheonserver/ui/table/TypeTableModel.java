package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.function.Function;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericCompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, InstanceRowModel::new, GenericCompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, ObservableList<Generic> attributes) {
		this(generic, stringExtractor, type -> attributes, InstanceRowModel::new, GenericCompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<CompositeConf<CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			Function<CompositeConf<GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		this(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractor, rowBuilder, cellBuilder, subCellBuilder);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<CompositeConf<CompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			Function<CompositeConf<GenericModel>, CompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		super(new CompositeConf<InstanceRowModel>(generic, stringExtractor, typ -> typ.getObservableSubInstances(), instance -> rowBuilder.apply(new CompositeConf<>(instance, observableListExtractor, attribute -> cellBuilder.apply(new CompositeConf<>(
				instance, inst -> inst.getObservableHolders(attribute), subCellBuilder))))));
	}
}
