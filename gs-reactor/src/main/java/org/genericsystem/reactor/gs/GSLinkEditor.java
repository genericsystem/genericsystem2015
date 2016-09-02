package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentBuilder;
import org.genericsystem.reactor.gs.GSSingleLinkComponentEditor.GSLinkComponentEditor;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

public class GSLinkEditor extends GSDiv implements ComponentsDefaults {

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
}