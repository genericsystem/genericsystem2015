package org.genericsystem.reactor.gs;

import java.util.List;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.GSLabel.GSLabelDisplayer;
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
		addStyle("width", "100%");
		addStyle("height", "100%");
		addStyle("justify-content", "center");
		addStyle("align-items", "center");
		select = constructor.build(this);
		select.select(gs -> gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
		select.addPostfixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				model.getObservableStyles(select).put("background-color", (String) model.getObservableValue(select, ReactorStatics.SELECTION_STRING).getValue());
		});
		select.optionElement.addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				model.getObservableStyles(select.optionElement).put("background-color", model.getString().getValue());
		});
		select.addStyle("width", "100%");
		select.addStyle("height", "100%");
		select.addPostfixBinding(model -> {
			Property<List<Property<GenericModel>>> selectedComponents = getProperty(ReactorStatics.COMPONENTS, model.getParent());
			if (selectedComponents != null)
				selectedComponents.getValue().add(model.getProperty(select, ReactorStatics.SELECTION));
		});
		new GSLabelDisplayer(this) {
			{
				select(gs -> !gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
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
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
		}
	}

	public static class GSLinkComponentAdder extends GSSingleLinkComponentEditor {

		public GSLinkComponentAdder(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[4])));
		}
	}

	public static class GSLinkComponentCreator extends GSSingleLinkComponentEditor {

		public GSLinkComponentCreator(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}
}