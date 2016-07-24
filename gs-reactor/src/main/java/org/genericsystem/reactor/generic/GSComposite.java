package org.genericsystem.reactor.generic;

import javafx.beans.binding.Bindings;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.composite.CompositeTag;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlRadio;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSComposite extends GSSection implements CompositeTag {

	public GSComposite(Tag<?> parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSComposite(Tag<?> parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		header();
		sections();
		footer();
	}

	protected void header() {

	}

	protected void sections() {
		new GSSection(this, GSComposite.this.getReverseDirection()) {
			{
				forEach(GSComposite.this);
				new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement extends GSComposite {

		public TitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public TitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new GSSection(this, FlexDirection.ROW) {
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

	public static class ColorTitleCompositeFlexElement extends TitleCompositeFlexElement {

		public ColorTitleCompositeFlexElement(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Tag<?> parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new GSSection(this, ColorTitleCompositeFlexElement.this.getReverseDirection()) {
				{
					bindStyle("background-color", ReactorStatics.TEXT, GenericModel::getString);
					forEach(ColorTitleCompositeFlexElement.this);
					new HtmlLabel<GenericModel>(this).bindText(GenericModel::getString);
				}
			};
		}
	}

	public static class CompositeRadio extends GSComposite implements CompositeTag {

		public CompositeRadio(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		@Override
		protected void sections() {
			new GSSection(this, CompositeRadio.this.getReverseDirection()) {
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

	public static class ColorCompositeRadio extends GSComposite implements CompositeTag {

		private Tag<GenericModel> flexSubElement;

		public ColorCompositeRadio(Tag<?> parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
			createProperty(ReactorStatics.SELECTION);
			storeProperty(ReactorStatics.SELECTION_INDEX, model -> model.getSelectionIndex(this));
			bindBiDirectionalSelection(flexSubElement);
			storeProperty(ReactorStatics.SELECTION_STRING,
					model -> Bindings.createStringBinding(() -> getStringExtractor().apply(getProperty(ReactorStatics.SELECTION, model).getValue() != null ? ((GenericModel) getProperty(ReactorStatics.SELECTION, model).getValue()).getGeneric() : null),
							getProperty(ReactorStatics.SELECTION, model)));
			bindStyle("background-color", ReactorStatics.SELECTION_STRING);
			addStyle("padding", "4px");
		}

		@Override
		protected HtmlDomNode createNode(String parentId) {
			return new SelectableHtmlDomNode(parentId);
		}

		@Override
		protected void sections() {
			flexSubElement = new GSSection(this, ColorCompositeRadio.this.getReverseDirection()) {
				{
					forEach(ColorCompositeRadio.this);
					bindStyle("background-color", ReactorStatics.TEXT, GenericModel::getString);
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
