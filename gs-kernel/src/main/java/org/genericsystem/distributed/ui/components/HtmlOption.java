package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.ActionHtmlNode;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlOption<M extends Model> extends HtmlElement<M, HtmlOption<M>, ActionHtmlNode> {

	public HtmlOption(HtmlElement<?, ?, ?> parent) {
		super(parent, ActionHtmlNode.class);
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode("option");
	}
	
	public HtmlOption<M> bindAction(Consumer<M> applyOnModel) {
		addActionBinding(ActionHtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
