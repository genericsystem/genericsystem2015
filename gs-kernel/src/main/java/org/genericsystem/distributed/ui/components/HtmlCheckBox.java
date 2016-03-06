package org.genericsystem.distributed.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlDomNode.CheckBoxHtmlDomNode;

public class HtmlCheckBox extends HtmlElement<HtmlCheckBox, CheckBoxHtmlDomNode> {

	public HtmlCheckBox(HtmlElement<?, ?> parent) {
		super(parent, CheckBoxHtmlDomNode.class);
	}

	@Override
	protected CheckBoxHtmlDomNode createNode(Object parent) {
		return new CheckBoxHtmlDomNode(getWebSocket());
	}

	public <M> HtmlCheckBox setChecked(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(CheckBoxHtmlDomNode::getChecked, applyOnModel);
		return this;
	}

}
