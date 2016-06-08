package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.EngineModel;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlSection;

public class FlexRow<M extends Model> extends HtmlSection<M> {

	public FlexRow(HtmlElement<?, ?> parent) {
		super(parent);
		addStyle("display", "flex");
		addStyle("flex-direction", "row");
		addStyle("flex-wrap", "nowrap");
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("section");
	}

	public static class H1FlexRow extends FlexRow<CompositeModel> {
		public H1FlexRow(HtmlElement<?, ?> parent, String title) {
			super(parent);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<EngineModel>(this).setText(title);
		}
	};

	public static class SaveCancelFlexRow extends FlexRow<CompositeModel> {

		public SaveCancelFlexRow(HtmlElement<?, ?> parent) {
			super(parent);
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
}