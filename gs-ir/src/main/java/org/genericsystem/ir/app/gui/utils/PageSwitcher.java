package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.TagSwitcher;

import io.reactivex.Observable;
import javafx.beans.property.Property;

public class PageSwitcher {

	public static final String PAGE = "page";
	public static final String HOME_PAGE = "homePage";
	public static final String FILTERS_STATISTICS = "filtersStatistics";

	public static class HOME_PAGE implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return RxJavaHelpers.optionalValuesOf(pageProperty).map(opt -> opt.isPresent() && HOME_PAGE.equals(opt.get()));
		}
	}

	public static class FILTERS_STATISTICS implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<String> pageProperty = tag.getContextProperty(PAGE, context);
			return RxJavaHelpers.optionalValuesOf(pageProperty).map(opt -> opt.isPresent() && FILTERS_STATISTICS.equals(opt.get()));
		}
	}

}
