package org.genericsystem.reactor.html;

import java.util.function.Function;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox<M extends Model> extends Tag<M> {

	public HtmlCheckBox(Tag<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "checkbox");
	}

	public void bindCheckedBidirectional(Function<M, Property<Boolean>> applyOnModel) {
		addBidirectionalBinding(InputCheckHtmlDomNode::getChecked, applyOnModel);
	}

}
