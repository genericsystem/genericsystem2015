package org.genericsystem.reactor.gs;

import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.ComponentsProperty;
import org.genericsystem.reactor.TagProperty.DisabledProperty;
import org.genericsystem.reactor.TagProperty.DisplayProperty;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentAdder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentCreator;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentEditor;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;

public class GSLinkEditor extends GSSection {

	protected final GSSingleLinkComponentEditor components;
	protected final TagProperty<List<Property<GenericModel>>> componentsProperty;

	public GSLinkEditor(GSTag parent) {
		this(parent, GSLinkComponentEditor::new);
	}

	public GSLinkEditor(GSTag parent, GSLinkComponentConstructor constructor) {
		super(parent, FlexDirection.ROW);
		componentsProperty = createNewProperty(ComponentsProperty::new);
		components = constructor.build(this, componentsProperty);
	}

	@FunctionalInterface
	public interface GSLinkComponentConstructor {
		GSSingleLinkComponentEditor build(GSTag parent, TagProperty<List<Property<GenericModel>>> componentsProperty);
	}

	public static class GSLinkEditorWithRemoval extends GSLinkEditor {

		public GSLinkEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					addStyle("height", "100%");
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
				addPostfixBinding(model -> ((GSInstanceCreator) parent.getParent().getParent()).getLinksValues().put(model.getGeneric(), componentsProperty.getValue(model.getGeneric())));
		}
	}

	public static class GSLinkAdder extends GSLinkCreator {

		public GSLinkAdder(GSTag parent) {
			super(parent, GSLinkComponentAdder::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					addStyle("height", "100%");
					setText("+");
					bindAttribute(ReactorStatics.DISPLAY, DisplayProperty::new, model -> Bindings.createStringBinding(() -> componentsProperty.getValue(model.getGeneric()) != null ? "flex" : "none", componentsProperty.getProperty(model.getGeneric())));
					bindAttribute(ReactorStatics.DISABLED, DisabledProperty::new, model -> Bindings.createStringBinding(() -> {
						if (componentsProperty.getValue(model.getGeneric()) == null)
							return ReactorStatics.DISABLED;
						List<Generic> selectedGenerics = componentsProperty.getValue(model.getGeneric()).stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
						return selectedGenerics.size() + 1 != model.getGeneric().getComponents().size() ? ReactorStatics.DISABLED : "";
					}, componentsProperty.getValue(model.getGeneric()) != null ? componentsProperty.getValue(model.getGeneric()).stream().toArray(Property[]::new) : null));
					bindAction(model -> {
						try {
							List<Generic> selectedGenerics = componentsProperty.getValue(model.getGeneric()).stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
							model.getGenerics()[3].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
							componentsProperty.getValue(model.getGeneric()).stream().forEach(sel -> sel.setValue(null));
						} catch (RollbackException e) {
							e.printStackTrace();
						}
					});
				}
			};
		}
	}
}
