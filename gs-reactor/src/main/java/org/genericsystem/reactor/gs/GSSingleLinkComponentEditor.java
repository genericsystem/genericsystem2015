package org.genericsystem.reactor.gs;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.GSLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.property.Property;

public class GSSingleLinkComponentEditor extends GSSection {

	protected GSSelect select;

	public GSSingleLinkComponentEditor(GSTag parent, TagProperty<List<Property<GenericModel>>> componentsProperty) {
		this(parent, InstanceCompositeSelect::new, componentsProperty);
	}

	public GSSingleLinkComponentEditor(GSTag parent, GSSelectConstructor constructor, TagProperty<List<Property<GenericModel>>> componentsProperty) {
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
				model.getObservableStyles(select).put("background-color", select.selectionStringProperty.getValue(model.getGeneric()));
		});
		select.optionElement.addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				model.getObservableStyles(select.optionElement).put("background-color", model.getString().getValue());
		});
		select.addStyle("width", "100%");
		select.addStyle("height", "100%");
		select.addPostfixBinding(model -> {
			Property selectedComponents = componentsProperty.getProperty(model.getGenerics()[2]);
			if (selectedComponents.getValue() == null)
				selectedComponents.setValue(new ArrayList<Property<GenericModel>>());
			((List<Property<GenericModel>>) selectedComponents.getValue()).add(select.selectionProperty.getProperty(model.getGeneric()));
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

		public GSLinkComponentEditor(GSTag parent, TagProperty<List<Property<GenericModel>>> componentsProperty) {
			super(parent, componentsProperty);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
		}
	}

	public static class GSLinkComponentAdder extends GSSingleLinkComponentEditor {

		public GSLinkComponentAdder(GSTag parent, TagProperty<List<Property<GenericModel>>> componentsProperty) {
			super(parent, CompositeSelectWithEmptyEntry::new, componentsProperty);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[4])));
		}
	}

	public static class GSLinkComponentCreator extends GSSingleLinkComponentEditor {

		public GSLinkComponentCreator(GSTag parent, TagProperty<List<Property<GenericModel>>> componentsProperty) {
			super(parent, CompositeSelectWithEmptyEntry::new, componentsProperty);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}
}