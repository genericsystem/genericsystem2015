package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderCreator;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderCreator;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkCreator;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSInstanceLinkComponentsTitleDisplayer;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSLinkComponentsDisplayer;
import org.genericsystem.reactor.gs.GSSingleLinkComponentDisplayer.GSLinkComponentsTitleDisplayer;
import org.genericsystem.reactor.gstag.GSLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

public class GSCellDisplayer extends GSSection {

	protected final GSTagConstructor holderDisplayerConstructor;
	protected final GSTagConstructor booleanHolderDisplayerConstructor;
	protected final GSTagConstructor linkDisplayerConstructor;
	private final boolean needMeta;

	public GSCellDisplayer(GSTag parent, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
		this(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
	}

	public GSCellDisplayer(GSTag parent, FlexDirection direction, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
		this(parent, direction, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor, true);
	}

	public GSCellDisplayer(GSTag parent, FlexDirection direction, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor, boolean needMeta) {
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

	public void style(Tag<?> tag) {
		tag.addStyle("justify-content", "center");
		tag.addStyle("align-items", "center");
		tag.addStyle("flex", "1");
		tag.addStyle("margin-right", "1px");
		tag.addStyle("margin-bottom", "1px");
		tag.addStyle("overflow", "hidden");
	}

	public static class GSCellEditor extends GSCellDisplayer {

		public GSCellEditor(GSTag parent, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			this(parent, FlexDirection.ROW, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor);
		}

		public GSCellEditor(GSTag parent, FlexDirection direction, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			super(parent, direction, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor, false);
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5e2");
		}
	}

	public static class GSInstanceCellDisplayer extends GSCellDisplayer {

		public GSInstanceCellDisplayer(GSTag parent) {
			super(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsDisplayer::new);
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addPrefixBinding(modelContext -> ((Model) modelContext).getObservableStyles(tag).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(((GenericModel) modelContext).getGeneric().getMeta())) ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends GSCellDisplayer {

		public LinkTitleDisplayer(GSTag parent) {
			this(parent, GSLabelDisplayer::new, GSCheckBoxDisplayer::new, GSLinkComponentsTitleDisplayer::new);
		}

		public LinkTitleDisplayer(GSTag parent, GSTagConstructor holderDisplayerConstructor, GSTagConstructor booleanHolderDisplayerConstructor, GSTagConstructor linkDisplayerConstructor) {
			super(parent, FlexDirection.ROW, holderDisplayerConstructor, booleanHolderDisplayerConstructor, linkDisplayerConstructor);
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
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

	public static class GSAttributeCreator extends GSCellEditor {

		public GSAttributeCreator(GSTag parent, FlexDirection direction) {
			super(parent, direction, GSHolderCreator::new, GSBooleanHolderCreator::new, GSLinkCreator::new);
		}

		@Override
		public void style(Tag<?> tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5a5");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
			tag.addStyle("overflow", "hidden");
		}
	}
}
