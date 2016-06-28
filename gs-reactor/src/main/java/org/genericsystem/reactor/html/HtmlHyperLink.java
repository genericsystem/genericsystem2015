package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink<M extends Model> extends Element<M> {

	public HtmlHyperLink(Element<?> parent) {
		super(parent, "a");
	}
	
	public HtmlHyperLink(Element<?> parent, String text) {
		super(parent, "a");
		setText(text);
	}
	
	public HtmlHyperLink(Element<?> parent, String text, Consumer<M> action) {
		this(parent, text);
		bindAction(action);
	}

	public void bindAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

}
