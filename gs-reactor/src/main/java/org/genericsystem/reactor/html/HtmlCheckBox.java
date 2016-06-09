package org.genericsystem.reactor.html;

import java.util.function.Function;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

import javafx.beans.property.Property;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox<M extends Model> extends Element<M> {

	public HtmlCheckBox(Element<?> parent) {
		super(parent, "input");
	}

	@Override
	protected CheckBoxHtmlDomNode createNode(String parentId) {
		return new CheckBoxHtmlDomNode(parentId);
	}

	public HtmlCheckBox<M> bindCheckedBidirectional(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(CheckBoxHtmlDomNode::getChecked, applyOnModel);
		return this;
	}

}
