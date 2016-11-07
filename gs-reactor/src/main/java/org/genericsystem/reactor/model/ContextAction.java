package org.genericsystem.reactor.model;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults.GenericValueComponents;
import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import java.io.Serializable;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InstanceBuilder.GSHolderBuilderDiv;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.Property;

public interface ContextAction extends BiConsumer<Context, Tag> {

	public static final Logger log = LoggerFactory.getLogger(ContextAction.class);

	public static class ADD_HOLDER implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Property<Serializable> convertedValue = ((ConvertedValueDefaults) tag.getParent()).getConvertedValueProperty(context.getParent());
			if (convertedValue.getValue() != null) {
				Serializable newValue = convertedValue.getValue();
				convertedValue.setValue(null);
				context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
			} else if (Boolean.class.equals(context.getGeneric().getInstanceValueClassConstraint()))
				context.getGenerics()[1].addHolder(context.getGeneric(), false);
		}
	}

	public static class REMOVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.remove();
		}
	}

	public static class FLUSH implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.flush();
			context.unmount();
		}
	}

	public static class FLUSH_CLOSE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.flush();
			context.unmount();
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(null);
			else
				log.warn("The RESET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class CANCEL implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.cancel();
			context.unmount();
		}
	}

	public static class MOUNT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.mount();
		}
	}

	public static class UNMOUNT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.unmount();
		}
	}

	public static class UNMOUNT_CLOSE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.unmount();
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(null);
			else
				log.warn("The RESET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class SHIFTTS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.shiftTs();
		}
	}

	public static class GC implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.gc();
		}
	}

	public static class DISPLAY_NONE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getDisplayProperty(context).setValue("none");
		}
	}

	public static class MODAL_DISPLAY_FLEX implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getParent().find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
		}
	}

	public static class SET_SELECTION implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.mount();
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(context);
			else
				log.warn("The SET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class RESET_SELECTION implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(null);
			else
				log.warn("The RESET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class CREATE_INSTANCE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (GSBuilderDefaults.class.isAssignableFrom(tag.getClass())) {
				GSBuilderDefaults buildTag = (GSBuilderDefaults) tag;
				Generic type = context.getGeneric();
				Map<Generic, GenericValueComponents> gvc = buildTag.getGenericValueComponents(context).getValue();
				Generic[] components = gvc.get(type).getComponents().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).toArray(Generic[]::new);
				if ((gvc.get(type).getGenericValue().getValue() != null || components.length != 0) && components.length == type.getComponents().size()) {
					Generic newInstance = type.setInstance(gvc.get(type).getGenericValue().getValue(), components);

					for (Entry<Generic, GenericValueComponents> entry : gvc.entrySet().stream().filter(e -> !e.getKey().equals(type)).collect(Collectors.toSet())) {
						Generic[] selectedGenerics = entry.getValue().getComponents().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).toArray(Generic[]::new);
						if ((entry.getValue().getGenericValue().getValue() != null || selectedGenerics.length != 0) && selectedGenerics.length + 1 == entry.getKey().getComponents().size()) {
							Generic newHolder = newInstance.setHolder(entry.getKey(), entry.getValue().getGenericValue().getValue(), selectedGenerics);
							if (PasswordDefaults.class.isAssignableFrom(tag.getParent().getParent().getClass()) && context.find(Password.class) != null && newHolder.isInstanceOf(context.find(Password.class)))
								newHolder.setHolder(context.find(Salt.class), ((PasswordDefaults) tag.getParent().getParent()).getSaltProperty(context).getValue());
						}
						entry.getValue().getComponents().stream().forEach(sel -> sel.setValue(null));
						entry.getValue().getGenericValue().setValue(null);
					}

					gvc.get(type).getComponents().stream().forEach(sel -> sel.setValue(null));
					gvc.get(type).getGenericValue().setValue(null);

					Map<Generic, Map<Generic, Property<Serializable>>> relationMap = buildTag.getMultipleRelationProperty(context).getValue();
					for (Entry<Generic, Map<Generic, Property<Serializable>>> entry : relationMap.entrySet())
						for (Generic target : entry.getValue().keySet())
							newInstance.setHolder(entry.getKey(), null, target);
					for (Property<Serializable> convertedProperty : relationMap.values().stream().flatMap(hm -> hm.values().stream()).collect(Collectors.toList()))
						convertedProperty.setValue(null);
					tag.getParent().getParent().find(Header.class).find(GSHolderBuilderDiv.class).find(HtmlSpan.class).addStyle(context, "display", "none");
				} else {
					tag.getParent().getParent().find(Header.class).find(GSHolderBuilderDiv.class).find(HtmlSpan.class).addStyle(context, "display", "inline");
				}
			} else
				log.warn("The CREATE_INSTANCE action can apply only to a tag class implementing GSBuilderDefaults.");
		}
	}

	public static class PREVIOUS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass()))
				((StepperDefaults) tag).prev(context);
			else
				log.warn("The PREVIOUS action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class NEXT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass()))
				((StepperDefaults) tag).next(context);
			else
				log.warn("The NEXT action is applicable only to a tag implementing SwitchDefaults.");
		}
	}

	public static class SET_ADMIN_MODE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getAdminModeProperty(context).setValue(true);
		}
	}

	public static class SET_NORMAL_MODE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getAdminModeProperty(context).setValue(false);
		}
	}

	public static class DISCONNECT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getLoggedUserProperty(context).setValue(null);
			tag.getAdminModeProperty(context).setValue(false);
		}
	}
}
