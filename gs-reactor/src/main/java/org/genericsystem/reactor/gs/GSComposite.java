package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.SelectionIndexProperty;
import org.genericsystem.reactor.TagProperty.SelectionProperty;
import org.genericsystem.reactor.TagProperty.SelectionStringProperty;
import org.genericsystem.reactor.TagProperty.TextProperty;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.gstag.GSRadio;
import org.genericsystem.reactor.model.GenericModel;

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
				new GSLabel(this).bindText(GenericModel::getString);
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
							bindText(GenericModel::getString);
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
					bindStyle("background-color", TextProperty::new, GenericModel::getString);
					forEach(ColorTitleCompositeFlexElement.this);
					new GSLabel(this).bindText(GenericModel::getString);
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
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}

	}

	public static class ColorCompositeRadio extends GSComposite {

		private GSTag flexSubElement;
		protected final TagProperty<GenericModel> selectionProperty;
		protected final TagProperty<Number> selectionIndexProperty;
		protected final TagProperty<String> selectionStringProperty;

		public ColorCompositeRadio(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
			selectionProperty = createNewProperty(SelectionProperty::new);
			selectionIndexProperty = storeProperty(SelectionIndexProperty::new, model -> model.getSelectionIndex(this));
			bindBiDirectionalSelection(flexSubElement, selectionProperty, selectionIndexProperty);
			selectionStringProperty = storeProperty(SelectionStringProperty::new, model -> Bindings
					.createStringBinding(() -> getStringExtractor().apply(selectionProperty.getValue(model.getGeneric()) != null ? selectionProperty.getValue(model.getGeneric()).getGeneric() : null), selectionProperty.getProperty(model.getGeneric())));
			bindStyle("background-color", selectionStringProperty);
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
					bindStyle("background-color", TextProperty::new, GenericModel::getString);
					new GSRadio(this);
					new GSLabel(this) {
						{
							bindText(GenericModel::getString);
						}
					};
				}
			};
		}
	}
}
