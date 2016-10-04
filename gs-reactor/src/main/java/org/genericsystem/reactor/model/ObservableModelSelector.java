package org.genericsystem.reactor.model;

import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public interface ObservableModelSelector extends BiFunction<Context, Tag, ObservableValue<Context>> {

	public static class SELECTION_SELECTOR implements Supplier<ObservableModelSelector> {
		@Override
		public ObservableModelSelector get() {
			return (context, tag) -> ((SelectionDefaults) tag).getSelectionProperty(context);
		}
	}

	public static class REMOVABLE_HOLDER_SELECTOR implements Supplier<ObservableModelSelector> {
		@Override
		public ObservableModelSelector get() {
			return (context, tag) -> {
				ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getParent().getGenerics());
				return BindingsTools
						.transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> (!context.getParent().getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? context : null, holders));
			};
		}
	}
}