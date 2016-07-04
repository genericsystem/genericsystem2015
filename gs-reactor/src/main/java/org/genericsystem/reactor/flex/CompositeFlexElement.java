package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.composite.CompositeElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlRadio;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.SelectorModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class CompositeFlexElement<M extends GenericModel> extends FlexElement<M> implements CompositeElement<M> {

	public CompositeFlexElement(Tag<?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.COLUMN);
	}

	public CompositeFlexElement(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
		super(parent, tag, flexDirection);
		header();
		sections();
		footer();
	}

	protected void header() {

	}

	protected void sections() {
		new FlexElement<GenericModel>(this, FlexTag.SECTION, CompositeFlexElement.this.getReverseDirection()) {
			{
				forEach(CompositeFlexElement.this);
				new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement<M extends GenericModel> extends CompositeFlexElement<M> {

		public TitleCompositeFlexElement(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public TitleCompositeFlexElement(Tag<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public TitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public TitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new FlexElement<GenericModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
				{
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new HtmlH1<GenericModel>(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				};
			};
		}
	}

	public static class ColorTitleCompositeFlexElement<M extends GenericModel> extends TitleCompositeFlexElement<M> {

		public ColorTitleCompositeFlexElement(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Tag<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public ColorTitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new FlexElement<GenericModel>(this, FlexTag.SECTION, ColorTitleCompositeFlexElement.this.getReverseDirection()) {
				{
					bindStyle("background-color", GenericModel::getString);
					forEach(ColorTitleCompositeFlexElement.this);
					new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
				}
			};
		}
	}

	public static class CompositeRadio<M extends SelectorModel> extends CompositeFlexElement<M> implements CompositeElement<M> {

		public CompositeRadio(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		@Override
		protected void sections() {
			new FlexElement<GenericModel>(this, FlexTag.SECTION, CompositeRadio.this.getReverseDirection()) {
				{
					forEach(CompositeRadio.this);
					new HtmlRadio<GenericModel>(this);
					new HtmlLabel<GenericModel>(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}

	}

	public static class ColorCompositeRadio<M extends SelectorModel> extends CompositeFlexElement<M> implements CompositeElement<M> {

		private Tag<GenericModel> flexSubElement;

		public ColorCompositeRadio(Tag<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
			bindBiDirectionalSelection(flexSubElement);
			bindStyle("background-color", SelectorModel::getSelectionString);
			addStyle("padding", "4px");
		}

		@Override
		protected HtmlDomNode createNode(String parentId) {
			return new SelectableHtmlDomNode(parentId);
		}

		@Override
		protected void sections() {
			flexSubElement = new FlexElement<GenericModel>(this, FlexTag.SECTION, ColorCompositeRadio.this.getReverseDirection()) {
				{
					forEach(ColorCompositeRadio.this);
					bindStyle("background-color", GenericModel::getString);
					new HtmlRadio<GenericModel>(this);
					new HtmlLabel<GenericModel>(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}

	}

}
