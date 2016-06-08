package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;

public class FlexElement<M extends Model> extends HtmlElement<M, HtmlDomNode> {

	public FlexElement(HtmlElement<?, ?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.COLUMN);
	}

	public FlexElement(HtmlElement<?, ?> parent, FlexTag tag, FlexDirection direction) {
		super(parent, tag.toString(), HtmlDomNode.class);
		addStyle("display", "flex");
		addStyle("flex-direction", direction.toString());
		// addStyle("flex-wrap", "nowrap");
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode();
	}

	public static class H1FlexElement extends FlexElement<CompositeModel> {
		public H1FlexElement(HtmlElement<?, ?> parent, String title) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW, title);
		}

		public H1FlexElement(HtmlElement<?, ?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexTag tag, String title) {
			this(parent, tag, FlexDirection.ROW, title);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexDirection flexDirection, String title) {
			this(parent, FlexTag.SECTION, flexDirection, title);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexTag tag, FlexDirection flexDirection, String title) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<>(this).setText(title);
		}

		public H1FlexElement(HtmlElement<?, ?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<CompositeModel>(this).bindText(CompositeModel::getString);
		}
	};

	public static class SaveCancelFlexRow extends FlexElement<CompositeModel> {

		public SaveCancelFlexRow(HtmlElement<?, ?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(HtmlElement<?, ?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(HtmlElement<?, ?> parent, FlexDirection direction) {
			this(parent, FlexTag.SECTION, direction);
		}

		public SaveCancelFlexRow(HtmlElement<?, ?> parent, FlexTag tag, FlexDirection direction) {
			super(parent, tag, direction);
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