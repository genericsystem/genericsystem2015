package org.genericsystem.newgui.metacomponent;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.newgui.component.IComponent;
import org.genericsystem.newgui.context.IContext;

public abstract class AbstractMetaComponent implements IMetaComponent {

	public List<IMetaComponent> children = new ArrayList<>();

	public void applyChildren(IComponent parent, IContext context) {
		getChildren().forEach(metaComponentChild -> {
			metaComponentChild.apply(parent, context);
		});
	}

	public AbstractMetaComponent(IMetaComponent parent) {
		if (parent != null)
			parent.getChildren().add(this);
	}

	@Override
	public List<IMetaComponent> getChildren() {
		return children;
	}

}
