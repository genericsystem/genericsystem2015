package org.genericsystem.reactor.gs;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentAdder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentBuilder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentEditor;
import org.genericsystem.reactor.gstag.GSHyperLink;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;

public class GSLinkEditor extends GSSection {

	protected GSSingleLinkComponentEditor components;

	public GSLinkEditor(GSTag parent) {
		this(parent, GSLinkComponentEditor::new);
	}

	public GSLinkEditor(GSTag parent, GSLinkComponentConstructor constructor) {
		super(parent, FlexDirection.ROW);
		createNewInitializedProperty(ReactorStatics.COMPONENTS, model -> new ArrayList<Property<GenericModel>>());
		components = constructor.build(this);
	}

	@FunctionalInterface
	public interface GSLinkComponentConstructor {
		GSSingleLinkComponentEditor build(GSTag parent);
	}

	public static class GSLinkEditorWithRemoval extends GSLinkEditor {

		public GSLinkEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSLinkBuilder extends GSLinkEditor {

		public GSLinkBuilder(GSTag parent) {
			this(parent, GSLinkComponentBuilder::new);
		}

		public GSLinkBuilder(GSTag parent, GSLinkComponentConstructor constructor) {
			super(parent, constructor);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceBuilder)
				addPostfixBinding(model -> {
					Property<Map<Generic, List<Property<GenericModel>>>> componentsMap = getProperty(ReactorStatics.COMPONENTS_MAP, model);
					Property<List<Property<GenericModel>>> components = getProperty(ReactorStatics.COMPONENTS, model);
					componentsMap.getValue().put(model.getGeneric(), components.getValue());
				});
		}
	}

	public static class GSLinkAdder extends GSLinkBuilder {

		public GSLinkAdder(GSTag parent) {
			super(parent, GSLinkComponentAdder::new);
			addStyle("height", "100%");
			addPostfixBinding(model -> {
				Property<List<Property<GenericModel>>> selectedComponents = getProperty(ReactorStatics.COMPONENTS, model);
				ChangeListener<GenericModel> listener = (o, v, nva) -> {
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