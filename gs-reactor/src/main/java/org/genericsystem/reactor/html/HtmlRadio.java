package org.genericsystem.reactor.html;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlRadio<M extends Model> extends Element<M> {

	public HtmlRadio(Element<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "radio");
	}

	public void bindCheckedBidirectional(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(InputCheckHtmlDomNode::getChecked, applyOnModel);
	}

}
