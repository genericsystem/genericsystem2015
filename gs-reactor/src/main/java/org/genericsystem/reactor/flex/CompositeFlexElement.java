package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.composite.CompositeElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlRadio;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.SelectorModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class CompositeFlexElement<M extends CompositeModel> extends FlexElement<M> implements CompositeElement<M> {

	public CompositeFlexElement(Element<?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.COLUMN);
	}

	public CompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
		super(parent, tag, flexDirection);
		header();
		sections();
		footer();
	}

	protected void header() {

	}

	protected void sections() {
		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				forEach(CompositeFlexElement.this);
				new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement<M extends CompositeModel> extends CompositeFlexElement<M> {

		public TitleCompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public TitleCompositeFlexElement(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public TitleCompositeFlexElement(Element<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public TitleCompositeFlexElement(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
				{
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new HtmlH1<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				};
			};
		}
	}

	public static class ColorTitleCompositeFlexElement<M extends CompositeModel> extends TitleCompositeFlexElement<M> {

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
				{
					bindStyle("background-color", CompositeModel::getString);
					forEach(ColorTitleCompositeFlexElement.this);
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
		}
	}

	public static class CompositeRadio<M extends SelectorModel> extends CompositeFlexElement<M> implements CompositeElement<M> {

		public CompositeRadio(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		@Override
		protected void sections() {
			new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
				{
					forEach(CompositeRadio.this);
					new HtmlRadio<CompositeModel>(this);
					new HtmlLabel<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				}
			};
		}

	}

	public static class ColorCompositeRadio<M extends SelectorModel> extends CompositeFlexElement<M> implements CompositeElement<M> {

		private Element<CompositeModel> flexSubElement;

		public ColorCompositeRadio(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
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
			flexSubElement = new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
				{
					forEach(ColorCompositeRadio.this);
					bindStyle("background-color", CompositeModel::getString);
					new HtmlRadio<CompositeModel>(this);
					new HtmlLabel<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				}
			};
		}
	}

}
