package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericCompositeModel<InstanceRowModel> {

	public TypeTableModel(CompositeConf<InstanceRowModel> compositeConf) {
		super(compositeConf);
	}

	public static Function<Generic, GenericCompositeModel<?>> getBuilder(List<MetaConf> confs) {
		Function<Generic, GenericCompositeModel<?>> builder = null;
		for (MetaConf conf : confs) {
			final Function<Generic, GenericCompositeModel<?>> leafBuilder = builder;
			builder = generic -> conf.build(generic, leafBuilder);
		}
		return builder;
	}

	public static TypeTableModel build(Generic generic, Function<Generic[], ObservableList<Generic>> attributesExtractor) {
		List<MetaConf> confs = new ArrayList<>();
		confs.add(new MetaConf(GenericModel.EXTRACTOR, null, GenericModel::new));
		confs.add(new MetaConf(GenericModel.EXTRACTOR, generics -> generics[0].getObservableHolders(generics[1]), GenericCompositeModel::new));
		confs.add(new MetaConf(GenericModel.EXTRACTOR, attributesExtractor, InstanceRowModel::new));
		confs.add(new MetaConf(GenericModel.EXTRACTOR, generics -> generics[0].getObservableSubInstances(), TypeTableModel::new));
		return (TypeTableModel) getBuilder(confs).apply(generic);
	}

	public static class MetaConf {

		private final Function<Generic, String> stringExtractor;
		private final Function<Generic[], ObservableList<Generic>> observableListExtractor;
		private final Function<CompositeConf, GenericCompositeModel<?>> modelConstructor;

		public MetaConf(Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<CompositeConf, GenericCompositeModel<?>> modelConstructor) {
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.modelConstructor = modelConstructor;
		}

		public Function<Generic, String> getStringExtractor() {
			return stringExtractor;
		}

		public Function<Generic[], ObservableList<Generic>> getObservableListExtractor() {
			return observableListExtractor;
		}

		public Function<CompositeConf, GenericCompositeModel<?>> getModelConstructor() {
			return modelConstructor;
		}

		public GenericCompositeModel<?> build(Generic generic, Function<Generic, GenericCompositeModel<?>> leafBuilder) {
			return getModelConstructor().apply(new CompositeConf<>(generic, getStringExtractor(), getObservableListExtractor(), leafBuilder));
		}
	}

}
