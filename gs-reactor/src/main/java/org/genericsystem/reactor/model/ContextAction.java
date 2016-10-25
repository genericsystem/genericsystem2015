package org.genericsystem.reactor.model;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents3.GSComposite.Header;
import org.genericsystem.reactor.gscomponents3.Modal.ModalWithDisplay;
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
		}
	}

	public static class CANCEL implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.cancel();
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
				ConvertedValueDefaults input = tag.getParent().getParent().find(Header.class).find(GSInputTextWithConversion.class);
				Generic newInstance = context.getGeneric().setInstance(input.getConvertedValueProperty(context).getValue());
				for (Entry<Generic, Property<Serializable>> entry : buildTag.getHoldersMapProperty(context).getValue().entrySet())
					if (entry.getValue().getValue() != null) {
						Generic newHolder = newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
						if (PasswordDefaults.class.isAssignableFrom(tag.getParent().getParent().getClass()) && newHolder.isInstanceOf(context.find(Password.class)))
							newHolder.setHolder(context.find(Salt.class), ((PasswordDefaults) tag.getParent().getParent()).getSaltProperty(context).getValue());
						entry.getValue().setValue(null);
					}
				for (Entry<Generic, List<Property<Context>>> entry : buildTag.getComponentsMapProperty(context).getValue().entrySet()) {
					List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
					if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
						newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
					entry.getValue().stream().forEach(sel -> sel.setValue(null));
				}
				Map<Generic, Map<Generic, Property<Serializable>>> relationMap = buildTag.getMultipleRelationProperty(context).getValue();
				for (Entry<Generic, Map<Generic, Property<Serializable>>> entry : relationMap.entrySet())
					for (Generic target : entry.getValue().keySet())
						newInstance.setHolder(entry.getKey(), null, target);
				for (Property<Serializable> convertedProperty : relationMap.values().stream().flatMap(hm -> hm.values().stream()).collect(Collectors.toList()))
					convertedProperty.setValue(null);
				input.getConvertedValueProperty(context).setValue(null);
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
}
