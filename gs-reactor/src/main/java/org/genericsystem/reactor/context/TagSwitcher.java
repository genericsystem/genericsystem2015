package org.genericsystem.reactor.context;

import java.util.function.BiFunction;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.security.model.Role.Admin;
import org.genericsystem.security.model.UserRole;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface TagSwitcher extends BiFunction<Context, Tag, ObservableValue<Boolean>> {

	public static class NORMAL_MODE_ONLY implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Boolean> adminProperty = tag.getAdminModeProperty(context);
			return Bindings.createBooleanBinding(() -> !Boolean.TRUE.equals(adminProperty.getValue()), adminProperty);
		}
	}

	public static class ADMIN_MODE_ONLY implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return tag.getAdminModeProperty(context);
		}
	}

	public static class LOGGED_USER implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return Bindings.createBooleanBinding(() -> loggedUserProperty.getValue() != null, loggedUserProperty);
		}
	}

	public static class NO_LOGGED_USER implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return Bindings.createBooleanBinding(() -> loggedUserProperty.getValue() == null, loggedUserProperty);
		}
	}

	public static class LOGGED_USER_ADMIN implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = tag.getLoggedUserProperty(context);
			ObservableValue<Generic> adminObservable = context.find(Admin.class).getObservableLink(context.find(UserRole.class), loggedUserProperty.getValue());
			return Bindings.createBooleanBinding(() -> loggedUserProperty.getValue() != null && adminObservable.getValue() != null, loggedUserProperty, adminObservable);
		}
	}
}
