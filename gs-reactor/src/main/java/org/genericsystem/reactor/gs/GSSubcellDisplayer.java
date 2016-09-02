package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderBuilder;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderBuilder;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkBuilder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentAdder;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;

public class GSSubcellDisplayer extends GSDiv {

	private final boolean needMeta;

	public GSSubcellDisplayer(Tag parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSSubcellDisplayer(Tag parent, FlexDirection direction) {
		this(parent, direction, true);
	}

	public GSSubcellDisplayer(Tag parent, FlexDirection direction, boolean needMeta) {
		super(parent, direction);
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
				getHolderDisplayerConstructor().build(this);
			}
		};
		new GSDiv(this, this.getDirection()) {
			{
				style(this);
				if (needMeta)
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
				else
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
				getBooleanHolderDisplayerConstructor().build(this);
			}
		};
		new GSDiv(this, this.getDirection()) {
			{
				addStyle("flex", "1");
				select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
				Tag components = getLinkDisplayerConstructor().build(this);
				style(components);
			}
		};
	}

	public TagConstructor getHolderDisplayerConstructor() {
		return GSLabelDisplayer::new;
	}

	public TagConstructor getBooleanHolderDisplayerConstructor() {
		return GSCheckBoxDisplayer::new;
	}

	public TagConstructor getLinkDisplayerConstructor() {
		return tag -> new GSDiv(tag, ((GSDiv) tag).getDirection()) {
			{
				forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
				new GSLabelDisplayer(this);
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

	@FunctionalInterface
	public interface TagConstructor {
		Tag build(Tag parent);
	}

	public static class GSSubcellEditor extends GSSubcellDisplayer {

		public GSSubcellEditor(Tag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellEditor(Tag parent, FlexDirection direction) {
			super(parent, direction, false);
		}

		@Override
		public TagConstructor getHolderDisplayerConstructor() {
			return GSHolderEditor::new;
		}

		@Override
		public TagConstructor getBooleanHolderDisplayerConstructor() {
			return GSBooleanHolderEditor::new;
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return GSLinkEditor::new;
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
			super(parent, direction);
		}

		@Override
		public TagConstructor getHolderDisplayerConstructor() {
			return tag -> new GSHolderEditor(tag) {
				{
					removalLink(this);
				}
			};
		}

		@Override
		public TagConstructor getBooleanHolderDisplayerConstructor() {
			return tag -> new GSBooleanHolderEditor(tag) {
				{
					removalLink(this);
				}
			};
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return tag -> new GSLinkEditor(tag) {
				{
					removalLink(this);
				}
			};
		}

		private void removalLink(Tag tag) {
			new HtmlHyperLink(tag) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("×");
					bindAction(Context::remove);
				}
			};
		}
	}

	public static class GSSubcellAdder extends GSSubcellEditor {

		public GSSubcellAdder(Tag parent) {
			this(parent, FlexDirection.ROW);
		}

		public GSSubcellAdder(Tag parent, FlexDirection direction) {
			super(parent, direction);
		}

		@Override
		public TagConstructor getHolderDisplayerConstructor() {
			return tag -> new GSHolderEditor(tag, GSInputTextWithConversion::new) {
				{
					input.addConvertedValueChangeListener((model, nva) -> {
						if (nva != null)
							model.getGenerics()[1].addHolder(model.getGeneric(), nva);
					});
					new HtmlHyperLink(this) {
						{
							addStyle("justify-content", "center");
							addStyle("text-decoration", "none");
							addStyle("height", "100%");
							setText("+");
							bindAction(context -> addHolder(context, input));
						}
					};
				}
			};
		}

		@Override
		public TagConstructor getBooleanHolderDisplayerConstructor() {
			return tag -> new GSBooleanHolderEditor(tag, GSCheckBoxWithValue::new) {
				{
					checkbox.addConvertedValueChangeListener((model, nva) -> {
						if (nva != null)
							model.getGenerics()[1].addHolder(model.getGeneric(), nva);
					});
					new HtmlHyperLink(this) {
						{
							addStyle("justify-content", "center");
							addStyle("text-decoration", "none");
							setText("+");
							bindAction(context -> addHolder(context, checkbox));
						}
					};
				}
			};
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return tag -> new GSLinkEditor(tag, GSLinkComponentAdder::new) {
				{
					addStyle("height", "100%");
					addPostfixBinding(model -> {
						Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
						ChangeListener<Context> listener = (o, v, nva) -> {
							List<Generic> selectedGenerics = selectedComponents.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
							if (selectedGenerics.size() + 1 == model.getGeneric().getComponents().size()) {
								selectedComponents.getValue().stream().forEach(sel -> sel.setValue(null));
								try {
									model.getGenerics()[1].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
								} catch (RollbackException e) {
									e.printStackTrace();
								}
							}
						};
						selectedComponents.getValue().forEach(component -> component.addListener(listener));
					});
				}
			};
		}

		private void addHolder(Context context, ConvertedValueDefaults tag) {
			Property<Serializable> observable = tag.getConvertedValueProperty(context);
			if (observable.getValue() != null) {
				Serializable newValue = observable.getValue();
				observable.setValue(null);
				context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
			}
		}
	}

	public static class LinkTitleDisplayer extends GSSubcellDisplayer {

		public LinkTitleDisplayer(Tag parent) {
			super(parent, FlexDirection.ROW);
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return tag -> new GSDiv(tag, ((GSDiv) tag).getDirection()) {
				{
					forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
					new GSLabelDisplayer(this);
				}
			};
		}

		@Override
		public void style(Tag tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
		}
	}

	public static class InstanceLinkTitleDisplayer extends LinkTitleDisplayer {

		public InstanceLinkTitleDisplayer(Tag parent) {
			super(parent);
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return tag -> new GSDiv(tag, ((GSDiv) tag).getDirection()) {
				{
					forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
					new GSLabelDisplayer(this);
				}
			};
		}
	}

	public static class GSAttributeBuilder extends GSSubcellEditor {

		public GSAttributeBuilder(Tag parent, FlexDirection direction) {
			super(parent, direction);
		}

		@Override
		public TagConstructor getHolderDisplayerConstructor() {
			return GSHolderBuilder::new;
		}

		@Override
		public TagConstructor getBooleanHolderDisplayerConstructor() {
			return GSBooleanHolderBuilder::new;
		}

		@Override
		public TagConstructor getLinkDisplayerConstructor() {
			return GSLinkBuilder::new;
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
