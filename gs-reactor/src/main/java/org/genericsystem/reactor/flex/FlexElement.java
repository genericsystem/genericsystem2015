package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;

public class FlexElement<M extends Model> extends Element<M> {

	public FlexElement(Element<?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.ROW);
	}

	public FlexElement(Element<?> parent, FlexTag tag, FlexDirection direction) {
		super(parent, tag.toString());
		addStyle("display", "flex");
		addStyle("flex-direction", direction.toString());
		addStyle("flex-wrap", "nowrap");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

	public static class H1FlexElement extends FlexElement<CompositeModel> {
		public H1FlexElement(Element<?> parent, String title) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW, title);
		}

		public H1FlexElement(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public H1FlexElement(Element<?> parent, FlexTag tag, String title) {
			this(parent, tag, FlexDirection.ROW, title);
		}

		public H1FlexElement(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public H1FlexElement(Element<?> parent, FlexDirection flexDirection, String title) {
			this(parent, FlexTag.SECTION, flexDirection, title);
		}

		public H1FlexElement(Element<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public H1FlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection, String title) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<>(this).setText(title);
		}

		public H1FlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<CompositeModel>(this).bindText(CompositeModel::getString);
		}
	};

	public static class SaveCancelFlexRow extends FlexElement<CompositeModel> {

		public SaveCancelFlexRow(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(Element<?> parent, FlexDirection direction) {
			this(parent, FlexTag.SECTION, direction);
		}

		public SaveCancelFlexRow(Element<?> parent, FlexTag tag, FlexDirection direction) {
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