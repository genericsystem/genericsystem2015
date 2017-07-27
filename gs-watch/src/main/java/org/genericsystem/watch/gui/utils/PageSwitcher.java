package org.genericsystem.watch.gui.utils;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class PageSwitcher {

	public static final String PAGE = "page";
	public static final String HOME_PAGE = "homePage";
	public static final String FILTERS_STATISTICS = "filtersStatistics";

	public static class HOME_PAGE implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return Bindings.createBooleanBinding(() -> HOME_PAGE.equals(pageProperty.getValue()), pageProperty);
		}
	}

	public static class FILTERS_STATISTICS implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return Bindings.createBooleanBinding(() -> FILTERS_STATISTICS.equals(pageProperty.getValue()), pageProperty);
		}
	}

}
