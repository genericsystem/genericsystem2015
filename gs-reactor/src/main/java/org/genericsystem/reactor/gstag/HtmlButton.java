package org.genericsystem.reactor.gstag;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends GSTag {

	public HtmlButton(GSTag parent) {
		super(parent, "button");
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
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
