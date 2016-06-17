package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton<M extends Model> extends Element<M> {

	public HtmlButton(Element<?> parent) {
		super(parent, "button");
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

	public void bindAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}
}
