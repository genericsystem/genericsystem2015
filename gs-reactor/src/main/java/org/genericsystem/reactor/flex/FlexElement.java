package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.model.GenericModel;

public class FlexElement<M extends Model> extends Tag<M> {

	private final FlexDirection direction;

	public FlexElement(Tag<?> parent, FlexTag tag, FlexDirection direction) {
		super(parent, tag.toString());
		this.direction = direction;
		addStyle("display", "flex");
		addStyle("flex-direction", direction.toString());
		addStyle("flex-wrap", "nowrap");
	}

	public FlexDirection getDirection() {
		return direction;
	}

	public FlexDirection getReverseDirection() {
		return getDirection().reverse();
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

	public static class H1FlexElement extends FlexElement<GenericModel> {
		public H1FlexElement(Tag<?> parent, String title) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW, title);
		}

		public H1FlexElement(Tag<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public H1FlexElement(Tag<?> parent, FlexTag tag, String title) {
			this(parent, tag, FlexDirection.ROW, title);
		}

		public H1FlexElement(Tag<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public H1FlexElement(Tag<?> parent, FlexDirection flexDirection, String title) {
			this(parent, FlexTag.SECTION, flexDirection, title);
		}

		public H1FlexElement(Tag<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public H1FlexElement(Tag<?> parent, FlexTag tag, FlexDirection flexDirection, String title) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<>(this).setText(title);
		}

		public H1FlexElement(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
			addStyle("justify-content", "center");
			addStyle("background-color", "#ffa500");
			new HtmlH1<GenericModel>(this).bindText(GenericModel::getString);
		}
	};

	public static class SaveCancelFlexRow extends FlexElement<GenericModel> {

		public SaveCancelFlexRow(Tag<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(Tag<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.ROW);
		}

		public SaveCancelFlexRow(Tag<?> parent, FlexDirection direction) {
			this(parent, FlexTag.SECTION, direction);
		}

		public SaveCancelFlexRow(Tag<?> parent, FlexTag tag, FlexDirection direction) {
			super(parent, tag, direction);
			addStyle("justify-content", "space-around");
			addStyle("padding", "10px");
			new HtmlButton<GenericModel>(this) {
				{
					setText("Save");
					bindAction(GenericModel::flush);
				}
			};
			new HtmlButton<GenericModel>(this) {
				{
					setText("Cancel");
					bindAction(GenericModel::cancel);
				}
			};
		}

	}
}