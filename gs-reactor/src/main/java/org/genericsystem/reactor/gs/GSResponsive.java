package org.genericsystem.reactor.gs;

import java.util.function.UnaryOperator;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

public class GSResponsive extends GSDiv implements SelectionDefaults {

	@SafeVarargs
	public GSResponsive(Tag parent, FlexDirection direction, UnaryOperator<Tag>... contentTagConsumers) {
		super(parent, direction);
		UnaryOperator<Tag>[] test = contentTagConsumers;
		new GSDiv(this, FlexDirection.ROW) {
			{
				addStyle("flex-wrap", "wrap");
				for (UnaryOperator<Tag> contentTagConsumer : test) {

					Tag tag = contentTagConsumer.apply(this);
					tag.addStyle("margin", "10px");
					tag.addStyle("padding", "10px");
					tag.addStyle("border-radius", "10px");
					tag.addStyle("background-color", "white");
				}
			}
		};
	}
}
