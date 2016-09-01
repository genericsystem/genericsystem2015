package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
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

public class GSSubcellDisplayer extends GSDiv {

	protected final TagConstructor holderDisplayerConstructor;
	protected final TagConstructor booleanHolderDisplayerConstructor;
	protected final TagConstructor linkDisplayerConstructor;
	private final boolean needMeta;

	public GSSubcellDisplayer(Tag parent, TagConstructor holderDisplayerConstructor, TagConstructor booleanHolderDisplayerConstructor, TagConstructor linkDisplayerConstructor) {
		this(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
	}

	public GSSubcellDisplayer(Tag parent, FlexDirection direction, TagConstructor holderDisplayerConstructor, TagConstructor booleanHolderDisplayerConstructor, TagConstructor linkDisplayerConstructor) {
		this(parent, direction, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor, true);
	}

	public GSSubcellDisplayer(Tag parent, FlexDirection direction, TagConstructor holderDisplayerConstructor, TagConstructor booleanHolderDisplayerConstructor, TagConstructor linkDisplayerConstructor, boolean needMeta) {
		super(parent, direction);
		this.holderDisplayerConstructor = holderDisplayerConstructor;
		this.booleanHolderDisplayerConstructor = booleanHolderDisplayerConstructor;
		this.linkDisplayerConstructor = linkDisplayerConstructor;
		this.needMeta = needMeta;
		addStyle("flex", "1");
		content();
	}

	public void content() {
		new GSDiv(this, this.getDirection()) {
			{
				style(this);
				if (needMeta)
					select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
				else
					select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
				holderDisplayerConstructor.build(this);
			}
		};
		new GSDiv(this, this.getDirection()) {
			{
				style(this);
				if (needMeta)
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
				else
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
				booleanHolderDisplayerConstructor.build(this);
			}
		};
		new GSDiv(this, this.getDirection()) {
			{
				addStyle("flex", "1");
				select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
				Tag components = linkDisplayerConstructor.build(this);
				style(components);
			}
		};
	}

	public void style(Tag tag) {
		tag.addStyle("justify-content", "center");
		tag.addStyle("align-items", "center");
		tag.addStyle("flex", "1");
		tag.addStyle("margin-right", "1px");
		tag.addStyle("margin-bottom", "1px");
	}

	public static class GSSubcellEditor extends GSSubcellDisplayer {

		public GSSubcellEditor(Tag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellEditor(Tag parent, FlexDirection direction) {
			this(parent, direction, GSHolderEditor::new, GSBooleanHolderEditor::new, GSLinkEditor::new);
		}

		public GSSubcellEditor(Tag parent, TagConstructor holderEditorConstructor, TagConstructor booleanHolderEditorConstructor, TagConstructor linkEditorConstructor) {
			this(parent, FlexDirection.ROW, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor);
		}

		public GSSubcellEditor(Tag parent, FlexDirection direction, TagConstructor holderEditorConstructor, TagConstructor booleanHolderEditorConstructor, TagConstructor linkEditorConstructor) {
			super(parent, direction, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor, false);
		}

		@Override
		public void style(Tag tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5e2");
			tag.addStyle("justify-content", "flex-start");
			tag.addStyle("align-items", "stretch");
		}
	}

	public static class GSSubcellEditorWithRemoval extends GSSubcellEditor {

		public GSSubcellEditorWithRemoval(Tag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellEditorWithRemoval(Tag parent, FlexDirection direction) {
			super(parent, direction, GSHolderEditorWithRemoval::new, GSBooleanHolderEditorWithRemoval::new, GSLinkEditorWithRemoval::new);
		}
	}

	public static class GSSubcellAdder extends GSSubcellEditor {

		public GSSubcellAdder(Tag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellAdder(Tag parent, FlexDirection direction) {
			super(parent, direction, GSHolderAdder::new, GSBooleanHolderAdder::new, GSLinkAdder::new);
		}
	}

	public static class GSInstanceSubcellDisplayer extends GSSubcellDisplayer {

		public GSInstanceSubcellDisplayer(Tag parent) {
			super(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsDisplayer::new);
		}

		@Override
		public void style(Tag tag) {
			super.style(tag);
			tag.addPrefixBinding(modelContext -> tag.getDomNodeStyles(modelContext).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? tag.getGenericStringProperty(modelContext).getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends GSSubcellDisplayer {

		public LinkTitleDisplayer(Tag parent) {
			this(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsTitleDisplayer::new);
		}

		public LinkTitleDisplayer(Tag parent, TagConstructor holderDisplayerConstructor, TagConstructor booleanHolderDisplayerConstructor, TagConstructor linkDisplayerConstructor) {
			super(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
		}

		@Override
		public void style(Tag tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
		}
	}

	public static class InstanceTitleDisplayer extends InstanceLinkTitleDisplayer {

		public InstanceTitleDisplayer(Tag parent) {
			super(parent);
			select(gs -> gs[0].getMeta());
		}
	}

	public static class InstanceLinkTitleDisplayer extends LinkTitleDisplayer {

		public InstanceLinkTitleDisplayer(Tag parent) {
			super(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSInstanceLinkComponentsTitleDisplayer::new);
		}
	}

	@FunctionalInterface
	public interface TagConstructor {
		Tag build(Tag parent);
	}

	public static class GSAttributeBuilder extends GSSubcellEditor {

		public GSAttributeBuilder(Tag parent, FlexDirection direction) {
			super(parent, direction, GSHolderBuilder::new, GSBooleanHolderBuilder::new, GSLinkBuilder::new);
		}

		@Override
		public void style(Tag tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5a5");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
		}
	}
}
