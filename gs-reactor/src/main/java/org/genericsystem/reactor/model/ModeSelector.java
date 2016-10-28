package org.genericsystem.reactor.model;

import java.util.function.BiFunction;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ModeSelector extends BiFunction<Context, Tag, ObservableValue<Boolean>> {

	public static class NORMAL_MODE_ONLY implements ModeSelector {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Boolean> adminProperty = tag.getAdminModeProperty(context);
			return Bindings.createBooleanBinding(() -> !Boolean.TRUE.equals(adminProperty.getValue()), adminProperty);
		}
	}

	public static class ADMIN_MODE_ONLY implements ModeSelector {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return tag.getAdminModeProperty(context);
		}
	}
}
