package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.Conf;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericCompositeModel.CompositeConf;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends CompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic generic, List<Function<Generic[], ObservableList<Generic>>> observableListExtractors) {
		this(generic, observableListExtractors, InstanceRowModel::new, GenericCompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, Function<Generic, String> stringExtractor, ObservableList<Generic> attributes) {
		this(generic, stringExtractor, Arrays.<Function<Generic[], ObservableList<Generic>>> asList(generics -> generics[0].getObservableSubInstances(), generics -> attributes, generics -> generics[0].getObservableHolders(generics[1])),
				InstanceRowModel::new, GenericCompositeModel<GenericModel>::new, GenericModel::new);
	}

	public TypeTableModel(Generic generic, List<Function<Generic[], ObservableList<Generic>>> observableListExtractors, Function<CompositeConf<GenericCompositeModel<GenericModel>>, InstanceRowModel> rowBuilder,
			Function<CompositeConf<GenericModel>, GenericCompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		this(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractors, rowBuilder, cellBuilder, subCellBuilder);
	}

	public TypeTableModel(Generic generic, List<Function<Generic, String>> stringExtractors, List<Function<Generic[], ObservableList<Generic>>> observableListExtractors,
			Function<CompositeConf<GenericCompositeModel<GenericModel>>, InstanceRowModel> rowBuilder, Function<CompositeConf<GenericModel>, GenericCompositeModel<GenericModel>> cellBuilder, Function<Generic, GenericModel> subCellBuilder) {
		super(new CompositeConf<>(generic, stringExtractors.get(0), observableListExtractors.get(0)));
		init(buildBuilder(rowBuilder, cellBuilder, stringExtractors, observableListExtractors, subCellBuilder));
	}

	public static Function<Generic, GenericCompositeModel<?>> getBuilder(List<Function<CompositeConf<GenericCompositeModel<?>>, GenericCompositeModel<?>>> builders, List<Function<Generic, String>> stringExtractors,
			List<Function<Generic[], ObservableList<Generic>>> observableListExtractors) {
		Function<Generic, GenericCompositeModel<?>> subCellBuild = holder -> builders.get(3).apply(new CompositeConf<>(holder, stringExtractors.get(3), observableListExtractors.get(3), null));
		Function<Generic, GenericCompositeModel<?>> cellBuild = attribute -> builders.get(2).apply(new CompositeConf<>(attribute, stringExtractors.get(2), observableListExtractors.get(2), subCellBuild));
		Function<Generic, GenericCompositeModel<?>> rowBuild = instance -> builders.get(1).apply(new CompositeConf<>(instance, stringExtractors.get(1), observableListExtractors.get(1), cellBuild));
		Function<Generic, GenericCompositeModel<?>> tableBuild = type -> builders.get(0).apply(new CompositeConf<>(type, stringExtractors.get(0), observableListExtractors.get(0), rowBuild));
		return tableBuild;
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
