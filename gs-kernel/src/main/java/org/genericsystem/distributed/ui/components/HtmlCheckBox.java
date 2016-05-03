package org.genericsystem.distributed.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.CheckBoxHtmlDomNode;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox<M extends Model> extends HtmlElement<M, HtmlCheckBox<M>, CheckBoxHtmlDomNode> {

	public HtmlCheckBox(HtmlElement<?, ?, ?> parent) {
		super(parent, CheckBoxHtmlDomNode.class);
	}

	@Override
	protected CheckBoxHtmlDomNode createNode(Object parent) {
		return new CheckBoxHtmlDomNode();
	}

	public HtmlCheckBox bindCheckedBidirectional(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(CheckBoxHtmlDomNode::getChecked, applyOnModel);
		return this;
	}

}
