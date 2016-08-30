package org.genericsystem.reactor.gs;

import java.util.List;
import java.util.stream.Collectors;

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentAdder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentBuilder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentEditor;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

public class GSLinkEditor extends GSSection implements ComponentsDefaults {

	protected GSSingleLinkComponentEditor components;

	public GSLinkEditor(Tag parent) {
		this(parent, GSLinkComponentEditor::new);
	}

	public GSLinkEditor(Tag parent, GSLinkComponentConstructor constructor) {
		super(parent, FlexDirection.ROW);
		createComponentsListProperty();
		components = constructor.build(this);
	}

	@FunctionalInterface
	public interface GSLinkComponentConstructor {
		GSSingleLinkComponentEditor build(Tag parent);
	}

	public static class GSLinkEditorWithRemoval extends GSLinkEditor {

		public GSLinkEditorWithRemoval(Tag parent) {
			super(parent);
			new HtmlHyperLink(this) {
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

	public static class GSLinkBuilder extends GSLinkEditor implements GSBuilderDefaults {

		public GSLinkBuilder(Tag parent) {
			this(parent, GSLinkComponentBuilder::new);
		}

		public GSLinkBuilder(Tag parent, GSLinkComponentConstructor constructor) {
			super(parent, constructor);
			addPostfixBinding(model -> {
				if (getComponentsMapProperty(model) != null)
					getComponentsMapProperty(model).getValue().put(model.getGeneric(), getComponentsProperty(model).getValue());
			});
		}
	}

	public static class GSLinkAdder extends GSLinkBuilder {

		public GSLinkAdder(Tag parent) {
			super(parent, GSLinkComponentAdder::new);
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
	}
}