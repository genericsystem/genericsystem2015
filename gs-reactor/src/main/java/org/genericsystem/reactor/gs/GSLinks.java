package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.CheckInputsValidityVisitor;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextCreatorWithConversion;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSCheckBox;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSLinks {
	static final Logger log = LoggerFactory.getLogger(Tag.class);

	public static class LabelDisplayer extends GSSection {

		private final ObservableListExtractor observableListExtractor;

		public LabelDisplayer(GSTag parent, ObservableListExtractor observableListExtractor) {
			super(parent, FlexDirection.ROW);
			this.observableListExtractor = observableListExtractor;
			content();
		}

		private void content() {
			new GSSection(this, FlexDirection.ROW) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
					new GSLabel(this) {
						{
							select(gs -> !Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
							bindText(GenericModel::getString);
						}
					};
					new GSCheckBoxEditor(this) {
						{
							addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
							select(gs -> Boolean.class.equals(gs[0].getMeta().getInstanceValueClassConstraint()) ? gs[0] : null);
						}
					};
				}
			};
			new GSSection(this, FlexDirection.ROW) {
				{
					style(this);
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
					new GSLabel(this) {
						{
							bindText(GenericModel::getString);
						}
					};
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
	}

	public static class LinkDisplayer extends LabelDisplayer {

		public LinkDisplayer(GSTag parent) {
			super(parent, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addPrefixBinding(modelContext -> ((Model) modelContext).getObservableStyles(tag).put("background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(((GenericModel) modelContext).getGeneric().getMeta())) ? ((GenericModel) modelContext).getString().getValue() : "#dda5e2"));
		}
	}

	public static class LinkTitleDisplayer extends LabelDisplayer {

		public LinkTitleDisplayer(GSTag parent, ObservableListExtractor observableListExtractor) {
			super(parent, observableListExtractor);
		}

		@Override
		public void style(Tag<?> tag) {
			super.style(tag);
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#ffa5a5");
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

	public static class GSCellEditor extends GSSection {

		private final GSTagConstructor holderEditorConstructor;
		private final GSTagConstructor booleanHolderEditorConstructor;
		private final GSTagConstructor linkEditorConstructor;

		public GSCellEditor(GSTag parent, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			this(parent, FlexDirection.ROW, holderEditorConstructor, booleanHolderEditorConstructor, linkEditorConstructor);
		}

		public GSCellEditor(GSTag parent, FlexDirection direction, GSTagConstructor holderEditorConstructor, GSTagConstructor booleanHolderEditorConstructor, GSTagConstructor linkEditorConstructor) {
			// TODO: filter only once.
			super(parent, direction);
			this.holderEditorConstructor = holderEditorConstructor;
			this.booleanHolderEditorConstructor = booleanHolderEditorConstructor;
			this.linkEditorConstructor = linkEditorConstructor;
			addStyle("flex", "1");
			addStyle("overflow", "hidden");
			content();
		}

		private void content() {
			new GSSection(this, this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
					holderEditorConstructor.build(this);
				}
			};
			new GSSection(this, this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
					booleanHolderEditorConstructor.build(this);
				}
			};
			new GSSection(this, this.getDirection()) {
				{
					style(this);
					select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
					linkEditorConstructor.build(this);
				}
			};
		}

		public void style(Tag<?> tag) {
			tag.addStyle("flex", "1");
			tag.addStyle("color", "#ffffff");
			tag.addStyle("background-color", "#dda5e2");
			tag.addStyle("margin-right", "1px");
			tag.addStyle("margin-bottom", "1px");
			tag.addStyle("justify-content", "center");
			tag.addStyle("align-items", "center");
		}
	}

	@FunctionalInterface
	public interface GSInputTextConstructor {
		GSInputTextWithConversion build(GSTag parent);
	}

	public static class GSHolderEditor extends GSSection {

		protected GSInputTextWithConversion input;

		public GSHolderEditor(GSTag parent) {
			this(parent, GSInputTextEditorWithConversion::new);
		}

		public GSHolderEditor(GSTag parent, GSInputTextConstructor constructor) {
			super(parent, FlexDirection.ROW);
			addStyle("flex", "1");
			addStyle("width", "100%");
			addStyle("height", "100%");
			input = constructor.build(this);
		}
	}

	public static class GSHolderEditorWithRemoval extends GSHolderEditor {

		public GSHolderEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSHolderAdder extends GSHolderEditor {

		public GSHolderAdder(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = input.getProperty(ReactorStatics.VALUE, model);
						if (observable.getValue() != null) {
							model.getGenerics()[3].addHolder(model.getGenerics()[2], observable.getValue());
							observable.setValue(null);
						}
					});
				}
			};
		}
	}

	public static class GSHolderCreator extends GSHolderEditor {

		public GSHolderCreator(GSTag parent) {
			super(parent, GSInputTextCreatorWithConversion::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator)
				input.addPrefixBinding(model -> ((GSInstanceCreator) parent.getParent().getParent()).getHoldersValues().put(model.getGeneric(), model.getProperty(input, ReactorStatics.VALUE)));
		}
	}

	@FunctionalInterface
	public interface GSCheckBoxConstructor {
		GSCheckBox build(GSTag parent);
	}

	public static class GSBooleanHolderEditor extends GSSection {

		protected GSCheckBox checkbox;

		public GSBooleanHolderEditor(GSTag parent) {
			this(parent, GSCheckBoxEditor::new);
		}

		public GSBooleanHolderEditor(GSTag parent, GSCheckBoxConstructor constructor) {
			super(parent, FlexDirection.ROW);
			addStyle("flex", "1");
			addStyle("width", "100%");
			addStyle("height", "100%");
			new GSSection(this, FlexDirection.ROW) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					addStyle("width", "100%");
					addStyle("height", "100%");
					checkbox = constructor.build(this);
				}
			};
		}
	}

	public static class GSBooleanHolderEditorWithRemoval extends GSBooleanHolderEditor {

		public GSBooleanHolderEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSBooleanHolderAdder extends GSBooleanHolderEditor {

		public GSBooleanHolderAdder(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = checkbox.getProperty(ReactorStatics.VALUE, model);
						model.getGenerics()[3].addHolder(model.getGenerics()[2], observable.getValue());
						observable.setValue(null);
					});
				}
			};
		}
	}

	public static class GSBooleanHolderCreator extends GSBooleanHolderEditor {

		public GSBooleanHolderCreator(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator)
				checkbox.addPrefixBinding(model -> ((GSInstanceCreator) parent.getParent().getParent()).getHoldersValues().put(model.getGeneric(), model.getProperty(checkbox, ReactorStatics.VALUE)));
		}
	}

	@FunctionalInterface
	public interface GSLinkComponentConstructor {
		GSLinkComponentSelector build(GSTag parent);
	}

	public static class GSLinkEditor extends GSSection {

		protected GSLinkComponentSelector components;

		public GSLinkEditor(GSTag parent) {
			this(parent, GSLinkComponentEditor::new);
		}

		public GSLinkEditor(GSTag parent, GSLinkComponentConstructor constructor) {
			super(parent, FlexDirection.ROW);
			addStyle("width", "100%");
			addStyle("height", "100%");
			createNewProperty(ReactorStatics.COMPONENTS);
			components = constructor.build(this);
		}
	}

	public static class GSLinkEditorWithRemoval extends GSLinkEditor {

		public GSLinkEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSLinkCreator extends GSLinkEditor {

		public GSLinkCreator(GSTag parent) {
			this(parent, GSLinkComponentCreator::new);
		}

		public GSLinkCreator(GSTag parent, GSLinkComponentConstructor constructor) {
			super(parent, constructor);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator)
				addPostfixBinding(model -> ((GSInstanceCreator) parent.getParent().getParent()).getLinksValues().put(model.getGeneric(), (List<Property<GenericModel>>) getProperty(ReactorStatics.COMPONENTS, model).getValue()));
		}
	}

	public static class GSLinkAdder extends GSLinkCreator {

		public GSLinkAdder(GSTag parent) {
			super(parent, GSLinkComponentAdder::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED, model -> Bindings.createStringBinding(() -> {
						List<Generic> selectedGenerics = ((List<Property<GenericModel>>) getProperty(ReactorStatics.COMPONENTS, model).getValue()).stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric())
								.filter(gen -> gen != null).collect(Collectors.toList());
						return selectedGenerics.size() + 1 != model.getGeneric().getComponents().size() ? ReactorStatics.DISABLED : "";
					}, ((List<Property<GenericModel>>) getProperty(ReactorStatics.COMPONENTS, model).getValue()).stream().toArray(Property[]::new)));
					bindAction(model -> {
						try {
							List<Property<GenericModel>> selectedComponents = (List<Property<GenericModel>>) getProperty(ReactorStatics.COMPONENTS, model).getValue();
							List<Generic> selectedGenerics = selectedComponents.stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
							model.getGenerics()[3].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
							selectedComponents.stream().forEach(sel -> sel.setValue(null));
						} catch (RollbackException e) {
							e.printStackTrace();
						}
					});
				}
			};
		}
	}

	@FunctionalInterface
	public interface GSSelectConstructor {
		GSSelect build(GSTag parent);
	}

	public static class GSLinkComponentSelector extends GSSection {

		protected GSSelect select;

		public GSLinkComponentSelector(GSTag parent) {
			this(parent, InstanceCompositeSelect::new);
		}

		public GSLinkComponentSelector(GSTag parent, GSSelectConstructor constructor) {
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
				Property selectedComponents = getProperty(ReactorStatics.COMPONENTS, model.getParent());
				if (selectedComponents != null) {
					if (selectedComponents.getValue() == null)
						selectedComponents.setValue(new ArrayList<Property<GenericModel>>());
					((List<Property<GenericModel>>) selectedComponents.getValue()).add(model.getProperty(select, ReactorStatics.SELECTION));
				}
			});
			new GSLabel(this) {
				{
					select(gs -> !gs[1].isReferentialIntegrityEnabled(pos(gs[1], gs[0])) ? gs[0] : null);
					bindText(GenericModel::getString);
				}
			};
		}
	}

	public static class GSLinkComponentEditor extends GSLinkComponentSelector {

		public GSLinkComponentEditor(GSTag parent) {
			super(parent);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
		}
	}

	public static class GSLinkComponentAdder extends GSLinkComponentSelector {

		public GSLinkComponentAdder(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[4])));
		}
	}

	public static class GSLinkComponentCreator extends GSLinkComponentSelector {

		public GSLinkComponentCreator(GSTag parent) {
			super(parent, CompositeSelectWithEmptyEntry::new);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSInstanceCreator extends GSComposite {

		private Property<Serializable> newInstanceValue;
		private Map<Generic, Property<Serializable>> holdersValues = new HashMap<>();
		private Map<Generic, List<Property<GenericModel>>> componentsValues = new HashMap<>();

		public GSInstanceCreator(GSTag parent, FlexDirection flexDirection) {
			super(parent, flexDirection);
		}

		public Map<Generic, Property<Serializable>> getHoldersValues() {
			return holdersValues;
		}

		public Map<Generic, List<Property<GenericModel>>> getLinksValues() {
			return componentsValues;
		}

		@Override
		protected void header() {
			new GSHolderCreator(this) {
				{
					input.addPrefixBinding(model -> newInstanceValue = model.getProperty(input, ReactorStatics.VALUE));
				}
			};
		}

		@Override
		protected void sections() {
			new GSAttributeCreator(this, FlexDirection.ROW) {
				{
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
				}
			};
		}

		@Override
		protected void footer() {
			new GSSection(this, this.getDirection()) {
				{
					if (this.getDirection().equals(FlexDirection.ROW)) {
						addStyle("flex", "0");
						addStyle("min-width", "100px");
					} else {
						addStyle("flex", "1");
					}
					addStyle("background-color", "#dda5a5");
					addStyle("margin-right", "1px");
					addStyle("margin-bottom", "1px");
					new GSButton(this) {
						{
							bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED, model -> {
								ObservableValue<Boolean> observable = new CheckInputsValidityVisitor(model).isInvalid();
								return Bindings.createStringBinding(() -> Boolean.TRUE.equals(observable.getValue()) ? ReactorStatics.DISABLED : "", observable);
							});
							bindAction(model -> {
								Generic newInstance = model.getGeneric().setInstance(newInstanceValue.getValue());
								for (Entry<Generic, Property<Serializable>> entry : holdersValues.entrySet())
									if (entry.getValue().getValue() != null) {
										newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
										entry.getValue().setValue(null);
									}
								for (Entry<Generic, List<Property<GenericModel>>> entry : componentsValues.entrySet()) {
									List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
									if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
										newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
									entry.getValue().stream().forEach(sel -> sel.setValue(null));
								}
								newInstanceValue.setValue(null);
							});
							setText("Add");
							addStyle("width", "100%");
							addStyle("height", "100%");
						}
					};
				}
			};
		}
	}
}
