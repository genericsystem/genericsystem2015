package org.genericsystem.reactor.model;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

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
import org.genericsystem.reactor.gscomponents2.GSComposite.Header;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.Property;

public interface ContextAction extends BiConsumer<Context, Tag> {

	public static final Logger log = LoggerFactory.getLogger(ContextAction.class);

	public static class ADD_HOLDER implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			ConvertedValueDefaults inputTag = (ConvertedValueDefaults) tag.getParent().find(Header.class).getObservableChildren().stream().filter(t -> t instanceof ConvertedValueDefaults).findFirst().get();
			Property<Serializable> observable = inputTag.getConvertedValueProperty(context.getParent());
			assert observable != null;
			if (observable.getValue() != null) {
				Serializable newValue = observable.getValue();
				observable.setValue(null);
				context.getGenerics()[2].addHolder(context.getGenerics()[1], newValue);
			}
		}
	}

	public static class REMOVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.remove();
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

	public static class CREATE_INSTANCE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (GSBuilderDefaults.class.isAssignableFrom(tag.getClass())) {
				GSBuilderDefaults buildTag = (GSBuilderDefaults) tag;
				ConvertedValueDefaults input = tag.getParent().getParent().find(GSInputTextWithConversion.class);
				Generic newInstance = context.getGeneric().setInstance(input.getConvertedValueProperty(context).getValue());
				for (Entry<Generic, Property<Serializable>> entry : buildTag.getHoldersMapProperty(context).getValue().entrySet())
					if (entry.getValue().getValue() != null) {
						newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
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
}
