package org.genericsystem.reactor.model;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import java.util.function.BiFunction;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.security.model.Role.Admin;
import org.genericsystem.security.model.UserRole;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public interface ObservableModelSelector extends BiFunction<Context, Tag, ObservableValue<Context>> {

	public static class SELECTION_SELECTOR implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				return ((SelectionDefaults) tag).getSelectionProperty(context);
			else
				throw new IllegalStateException("SELECTION_SELECTOR is applicable only to tags implementing SelectionDefaults.");
		}
	}

	public static class REMOVABLE_HOLDER_SELECTOR implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getParent().getGenerics());
			return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(
					() -> (!context.getParent().getGeneric().isRequiredConstraintEnabled(context.getGeneric().getComponents().indexOf(context.getGenerics()[2])) && holders.size() == 1) || holders.size() > 1 ? context : null, holders));
		}
	}

	public static class HOLDER_ADDITION_ENABLED_SELECTOR implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getGenerics());
			return Bindings.createObjectBinding(
					() -> holders.isEmpty()
							|| (!(context.getGeneric().getComponents().size() == 1 && context.getGeneric().isPropertyConstraintEnabled()) && !context.getGeneric().isSingularConstraintEnabled(
									context.getGeneric().getComponents().indexOf(context.getGenerics()[2]))) ? context : null, holders);
		}
	}

	public static class LOGGED_USER implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return Bindings.createObjectBinding(() -> loggedUserProperty.getValue() != null ? context : null, loggedUserProperty);
		}
	}

	public static class NO_LOGGED_USER implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = ((UserRoleDefaults) tag).getLoggedUserProperty(context);
			return Bindings.createObjectBinding(() -> loggedUserProperty.getValue() != null ? null : context, loggedUserProperty);
		}
	}

	public static class LOGGED_USER_ADMIN implements ObservableModelSelector {
		@Override
		public ObservableValue<Context> apply(Context context, Tag tag) {
			Property<Generic> loggedUserProperty = tag.getLoggedUserProperty(context);
			ObservableValue<Generic> adminObservable = context.find(Admin.class).getObservableLink(context.find(UserRole.class), loggedUserProperty.getValue());
			return Bindings.createObjectBinding(() -> loggedUserProperty.getValue() != null && adminObservable.getValue() != null ? context : null, loggedUserProperty, adminObservable);
		}
	}
}