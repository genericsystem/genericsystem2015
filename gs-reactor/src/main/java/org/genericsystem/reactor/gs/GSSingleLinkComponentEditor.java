package org.genericsystem.reactor.gs;

import java.util.List;

import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.property.Property;

public class GSSingleLinkComponentEditor extends GSSection {

	protected GSSelect select;

	public GSSingleLinkComponentEditor(GSTag parent) {
		this(parent, InstanceCompositeSelect::new);
	}

	public GSSingleLinkComponentEditor(GSTag parent, GSSelectConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		addStyle("justify-content", "center");
		addStyle("align-items", "center");
		select = constructor.build(this);
		select.select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
		select.addPostfixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				select.getDomNodeStyles(model).put("background-color", select.getSelectionString(model).getValue());
		});
		select.optionElement.addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				select.optionElement.getDomNodeStyles(model).put("background-color", select.optionElement.getGenericStringProperty(model).getValue());
		});
		select.addStyle("width", "100%");
		select.addStyle("height", "100%");
		select.addPostfixBinding(model -> {
			Property<List<Property<GenericModel>>> selectedComponents = select.getComponentsProperty(model);
			if (selectedComponents != null)
				selectedComponents.getValue().add(select.getSelectionProperty(model));
		});
		new GSLabelDisplayer(this) {
			{
				select(gs -> !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) && !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null);
			}
		};
	}

	@FunctionalInterface
	public interface GSSelectConstructor {
		GSSelect build(GSTag parent);
	}

	public static class GSLinkComponentEditor extends GSSingleLinkComponentEditor {

		public GSLinkComponentEditor(GSTag parent) {
			super(parent);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSLinkComponentAdder extends GSSingleLinkComponentEditor {

		public GSLinkComponentAdder(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSLinkComponentBuilder extends GSSingleLinkComponentEditor {

		public GSLinkComponentBuilder(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
		}
	}
}