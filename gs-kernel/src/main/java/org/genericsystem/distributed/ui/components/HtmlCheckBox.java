package org.genericsystem.distributed.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeCheckBox;

public class HtmlCheckBox extends HtmlElement<HtmlCheckBox> {

	public HtmlCheckBox(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeCheckBox createNode(Object parent) {
		HtmlNodeCheckBox checkBox = new HtmlNodeCheckBox(getWebSocket());
		checkBox.getTag().set("input");
		return checkBox;
	}

	public <M> HtmlCheckBox setChecked(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(HtmlNode::getChecked, applyOnModel);
		return this;
	}

}
