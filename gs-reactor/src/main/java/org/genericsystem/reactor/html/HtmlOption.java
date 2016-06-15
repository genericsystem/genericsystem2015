package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlOption<M extends Model> extends Element<M> {

	public HtmlOption(Element<?> parent) {
		super(parent, "option");
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

	public void bindAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}
}
