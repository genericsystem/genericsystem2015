package org.genericsystem.reactor.components;

import java.util.function.Function;
import javafx.beans.property.Property;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.CheckBoxHtmlDomNode;

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
