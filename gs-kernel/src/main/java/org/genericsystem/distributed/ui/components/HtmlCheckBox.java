package org.genericsystem.distributed.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeCheckBox;

public class HtmlCheckBox extends HtmlElement<HtmlCheckBox, HtmlNodeCheckBox> {

	public HtmlCheckBox(HtmlElement<?, ?> parent) {
		super(parent, HtmlNodeCheckBox.class);
	}

	@Override
	protected HtmlNodeCheckBox createNode(Object parent) {
		return new HtmlNodeCheckBox(getWebSocket());
	}

	public <M> HtmlCheckBox setChecked(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(HtmlNodeCheckBox::getChecked, applyOnModel);
		return this;
	}

}
