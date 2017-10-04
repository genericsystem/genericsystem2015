package org.genericsystem.reactor.context;

import java.util.function.BiFunction;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.security.model.Role.Admin;
import org.genericsystem.security.model.UserRole;

import io.reactivex.Observable;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface TagSwitcher extends BiFunction<Context, Tag, Observable<Boolean>> {

	public static class NORMAL_MODE_ONLY implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return RxJavaHelpers.optionalValuesOf(tag.getAdminModeProperty(context)).map(opt -> !opt.isPresent() || !opt.get());
		}
	}

	public static class ADMIN_MODE_ONLY implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			return RxJavaHelpers.optionalValuesOf(tag.getAdminModeProperty(context)).map(opt -> opt.isPresent() && opt.get());
		}
	}

	public static class LOGGED_USER implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return RxJavaHelpers.optionalValuesOf(loggedUserProperty).map(opt -> opt.isPresent());
		}
	}

	public static class NO_LOGGED_USER implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return RxJavaHelpers.optionalValuesOf(loggedUserProperty).map(opt -> !opt.isPresent());
		}
	}

	public static class LOGGED_USER_ADMIN implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = tag.getLoggedUserProperty(context);
			ObservableValue<Generic> adminObservable = context.find(Admin.class).getObservableLink(context.find(UserRole.class), loggedUserProperty.getValue());
			return RxJavaHelpers.optionalValuesOf(loggedUserProperty).map(opt -> opt.isPresent() && adminObservable.getValue() != null);
		}
	}
}
