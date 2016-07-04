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
public class CompositeFlexElement<M extends GenericModel> extends FlexTag<M> implements CompositeElement<M> {

	public CompositeFlexElement(Tag<?> parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public CompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		header();
		sections();
		footer();
	}

	protected void header() {

	}

	protected void sections() {
		new FlexTag<GenericModel>(this, CompositeFlexElement.this.getReverseDirection()) {
			{
				forEach(CompositeFlexElement.this);
				new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement<M extends GenericModel> extends CompositeFlexElement<M> {

		public TitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public TitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new FlexTag<GenericModel>(this, FlexDirection.ROW) {
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

		public ColorTitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new FlexTag<GenericModel>(this, ColorTitleCompositeFlexElement.this.getReverseDirection()) {
				{
					bindStyle("background-color", GenericModel::getString);
					forEach(ColorTitleCompositeFlexElement.this);
					new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
				}
			};
		}
	}

	public static class CompositeRadio<M extends SelectorModel> extends CompositeFlexElement<M> implements CompositeElement<M> {

		public CompositeRadio(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		@Override
		protected void sections() {
			new FlexTag<GenericModel>(this, CompositeRadio.this.getReverseDirection()) {
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

		public ColorCompositeRadio(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
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
			flexSubElement = new FlexTag<GenericModel>(this, ColorCompositeRadio.this.getReverseDirection()) {
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
