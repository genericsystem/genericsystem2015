package org.genericsystem.reactor.gs;

import java.util.List;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;

public class GSSingleLinkComponentEditor extends GSSection {

	protected GSSelect select;

	public GSSingleLinkComponentEditor(Tag parent) {
		this(parent, InstanceCompositeSelect::new);
	}

	public GSSingleLinkComponentEditor(Tag parent, GSSelectConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		addStyle("justify-content", "center");
		addStyle("align-items", "center");
		select = constructor.build(this);
		select.select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
		select.addStyle("width", "100%");
		select.addStyle("height", "100%");
		select.addPostfixBinding(model -> {
			Property<List<Property<Context>>> selectedComponents = select.getComponentsProperty(model);
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
		GSSelect build(Tag parent);
	}

	public static class GSLinkComponentEditor extends GSSingleLinkComponentEditor {

		public GSLinkComponentEditor(Tag parent) {
			super(parent);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSLinkComponentAdder extends GSSingleLinkComponentEditor {

		public GSLinkComponentAdder(Tag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSLinkComponentBuilder extends GSSingleLinkComponentEditor {

		public GSLinkComponentBuilder(Tag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
		}
	}
}