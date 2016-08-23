package org.genericsystem.carcolor;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSModalButton extends GSTag{

	public GSModalButton(GSTag parent, GSSection gSection) {
		super(parent, "button");
		bindAction(model -> {
			gSection.getProperty(ReactorStatics.DISPLAY, model).setValue("flex");
		});
		
	}		

	@Override
	protected Tag<GenericModel>.HtmlDomNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}
	
	public void bindAction(Consumer<GenericModel> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}

	public List<ObservableValue<Boolean>> getInvalidList(GenericModel model) {
		return this.<List<ObservableValue<Boolean>>> getProperty(ReactorStatics.INVALID_LIST, model).getValue();
	}

	public Map<Generic, Property<Serializable>> getHoldersMap(GenericModel model) {
		return this.<Map<Generic, Property<Serializable>>> getProperty(ReactorStatics.HOLDERS_MAP, model).getValue();
	}

	public Map<Generic, List<Property<GenericModel>>> getComponentsMap(GenericModel model) {
		return this.<Map<Generic, List<Property<GenericModel>>>> getProperty(ReactorStatics.COMPONENTS_MAP, model).getValue();
	}
	
}
