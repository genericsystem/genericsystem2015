package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.SelectableHtmlDomNode;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.gstag.GSRadio;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSComposite extends GSSection {

	public GSComposite(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSComposite(GSTag parent, FlexDirection flexDirection) {
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
				new GSLabel(this) {
					{
						bindText();
					}
				};
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement extends GSComposite {

		public TitleCompositeFlexElement(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public TitleCompositeFlexElement(GSTag parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new GSSection(this, FlexDirection.ROW) {
				{
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new GSH1(this) {
						{
							setStringExtractor(StringExtractor.MANAGEMENT);
							bindText();
						}
					};
				};
			};
		}
	}

	public static class ColorTitleCompositeFlexElement extends TitleCompositeFlexElement {

		public ColorTitleCompositeFlexElement(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public ColorTitleCompositeFlexElement(GSTag parent) {
			this(parent, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new GSSection(this, ColorTitleCompositeFlexElement.this.getReverseDirection()) {
				{
					bindStyle("background-color", ReactorStatics.BACKGROUND, model -> getGenericStringProperty(model));
					forEach(ColorTitleCompositeFlexElement.this);
					new GSLabel(this) {
						{
							bindText();
						}
					};
				}
			};
		}
	}

	public static class CompositeRadio extends GSComposite {

		public CompositeRadio(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		@Override
		protected void sections() {
			new GSSection(this, CompositeRadio.this.getReverseDirection()) {
				{
					forEach(CompositeRadio.this);
					new GSRadio(this);
					new GSLabel(this) {
						{
							bindText();
						}
					};
				}
			};
		}

	}

	public static class ColorCompositeRadio extends GSComposite implements SelectionDefaults {

		private GSTag flexSubElement;

		public ColorCompositeRadio(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
			createSelectionProperty();
			storeProperty(ReactorStatics.SELECTION_INDEX, model -> model.getSelectionIndex(this));
			bindBiDirectionalSelection(flexSubElement);
			storeProperty(ReactorStatics.SELECTION_STRING,
					model -> Bindings.createStringBinding(() -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(getSelectionProperty(model).getValue() != null ? getSelectionProperty(model).getValue().getGeneric() : null), getSelectionProperty(model)));
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
					bindStyle("background-color", ReactorStatics.BACKGROUND, model -> getGenericStringProperty(model));
					new GSRadio(this);
					new GSLabel(this) {
						{
							bindText();
						}
					};
				}
			};
		}
	}
}
