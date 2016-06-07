package org.genericsystem.example.reactor;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlSection;

public class AppFooterHtml extends HtmlSection<AppModel> {
	public AppFooterHtml(HtmlElement<?, ?> parent) {
		super(parent);
		addStyleClass("gsfooter");
		addStyle("display", "flex");
		addStyle("flex-direction", "row");
		addStyle("flex-wrap", "nowrap");
		addStyle("justify-content", "space-around");
		addStyle("padding", "10px");
		new HtmlButton<CompositeModel>(this) {
			{
				setText("Save");
				bindAction(CompositeModel::flush);
			}
		};
		new HtmlButton<CompositeModel>(this) {
			{
				setText("Cancel");
				bindAction(CompositeModel::cancel);
			}
		};
	}
}