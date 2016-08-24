package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderAdder;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderBuilder;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderEditorWithRemoval;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderAdder;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderBuilder;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderEditorWithRemoval;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkAdder;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkBuilder;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkEditorWithRemoval;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSInstanceLinkComponentsTitleDisplayer;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSLinkComponentsDisplayer;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSLinkComponentsTitleDisplayer;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.StringExtractor;

public class GSSubcellDisplayer extends GSSection {

	protected final GSTagConstructor holderDisplayerConstructor;
	protected final GSTagConstructor booleanHolderDisplayerConstructor;
	protected final GSTagConstructor linkDisplayerConstructor;
	private final boolean needMeta;

	public GSSubcellDisplayer(GSTag parent, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
		this(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
	}

	public GSSubcellDisplayer(GSTag parent, FlexDirection direction, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
		this(parent, direction, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor, true);
	}

	public GSSubcellDisplayer(GSTag parent, FlexDirection direction, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor, boolean needMeta) {
		super(parent, direction);
		this.holderDisplayerConstructor = holderDisplayerConstructor;
		this.booleanHolderDisplayerConstructor = booleanHolderDisplayerConstructor;
		this.linkDisplayerConstructor = linkDisplayerConstructor;
		this.needMeta = needMeta;
		addStyle("flex", "1");
		addStyle("overflow", "hidden");
		content();
	}

	public void content() {
		new GSSection(this, this.getDirection()) {
			{
				style(this);
				if (needMeta)
					select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
				else
					select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
				holderDisplayerConstructor.build(this);
			}
		};
		new GSSection(this, this.getDirection()) {
			{
				style(this);
				if (needMeta)
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
				else
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
				booleanHolderDisplayerConstructor.build(this);
			}
		};
		new GSSection(this, this.getDirection()) {
			{
				addStyle("flex", "1");
				select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
				GSTag components = linkDisplayerConstructor.build(this);
				style(components);
			}
		};
	}

	public void style(GSTag tag) {
		tag.addStyle("justify-content", "center");
		tag.addStyle("align-items", "center");
		tag.addStyle("flex", "1");
		tag.addStyle("margin-right", "1px");
		tag.addStyle("margin-bottom", "1px");
		tag.addStyle("overflow", "hidden");
	}

	public static class GSSubcellEditor extends GSSubcellDisplayer {

		public GSSubcellEditor(GSTag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellEditor(GSTag parent, FlexDirection direction) {
			this(parent, direction, GSHolderEditor::new, GSBooleanHolderEditor::new, GSLinkEditor::new);
		}

		public GSSubcellEditor(GSTag parent, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			this(parent, FlexDirection.ROW, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor);
		}

		public GSSubcellEditor(GSTag parent, FlexDirection direction, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			super(parent, direction, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor, false);
		}

		@Override
		public void style(GSTag tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5e2");
			tag.addStyle("justify-content", "flex-start");
			tag.addStyle("align-items", "stretch");
		}
	}

	public static class GSSubcellEditorWithRemoval extends GSSubcellEditor {

		public GSSubcellEditorWithRemoval(GSTag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellEditorWithRemoval(GSTag parent, FlexDirection direction) {
			super(parent, direction, GSHolderEditorWithRemoval::new, GSBooleanHolderEditorWithRemoval::new, GSLinkEditorWithRemoval::new);
		}
	}

	public static class GSSubcellAdder extends GSSubcellEditor {

		public GSSubcellAdder(GSTag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellAdder(GSTag parent, FlexDirection direction) {
			super(parent, direction, GSHolderAdder::new, GSBooleanHolderAdder::new, GSLinkAdder::new);
		}
	}

	public static class GSInstanceSubcellDisplayer extends GSSubcellDisplayer {

		public GSInstanceSubcellDisplayer(GSTag parent) {
			super(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsDisplayer::new);
		}

		@Override
		public void style(GSTag tag) {
			super.style(tag);
			tag.addPrefixBinding(modelContext -> ((Model) modelContext).getObservableStyles(tag).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? tag.getGenericStringProperty(modelContext).getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends GSSubcellDisplayer {

		public LinkTitleDisplayer(GSTag parent) {
			this(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsTitleDisplayer::new);
		}

		public LinkTitleDisplayer(GSTag parent, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
			super(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
		}

		@Override
		public void style(GSTag tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
		}
	}

	public static class InstanceTitleDisplayer extends InstanceLinkTitleDisplayer {

		public InstanceTitleDisplayer(GSTag parent) {
			super(parent);
			select(gs -> gs[0].getMeta());
		}
	}

	public static class InstanceLinkTitleDisplayer extends LinkTitleDisplayer {

		public InstanceLinkTitleDisplayer(GSTag parent) {
			super(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSInstanceLinkComponentsTitleDisplayer::new);
		}
	}

	@FunctionalInterface
	public interface GSTagConstructor {
		GSTag build(GSTag parent);
	}

	public static class GSAttributeBuilder extends GSSubcellEditor {

		public GSAttributeBuilder(GSTag parent, FlexDirection direction) {
			super(parent, direction, GSHolderBuilder::new, GSBooleanHolderBuilder::new, GSLinkBuilder::new);
		}

		@Override
		public void style(GSTag tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5a5");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
			tag.addStyle("overflow", "hidden");
		}
	}
}
