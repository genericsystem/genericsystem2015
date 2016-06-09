package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;

public class CompositeSelect<M extends CompositeModel> extends HtmlSelect<M> implements Composite<M> {

	public CompositeSelect(Element<?> parent) {
		super(parent);

		new HtmlOption<CompositeModel>(this) {
			{
				bindText(CompositeModel::getString);
				bindAction(CompositeModel::select);
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b));
			}
		};
	}
}