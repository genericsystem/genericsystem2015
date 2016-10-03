package org.genericsystem.reactor.model;

import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.value.ObservableValue;

public interface ObservableModelSelector extends BiFunction<Context, Tag, ObservableValue<Context>> {

	public static class SELECTION_SELECTOR implements Supplier<ObservableModelSelector> {
		@Override
		public ObservableModelSelector get() {
			return (context, tag) -> ((SelectionDefaults) tag).getSelectionProperty(context);
		}
	}
}
