package org.genericsystem.newgui.metacomponent;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.newgui.component.IComponent;
import org.genericsystem.newgui.metacontext.IMetaContext;

public abstract class AbstractMetaComponent implements IMetaComponent {

	// public Class<? extends Node> typeNode;
	public List<IMetaComponent> children = new ArrayList<>();

	// public Bindings link;

	//
	// public AbstractMetaComponent(Class<? extends Node> typeNode, List<MetaComponentImpl> childs, Bindings link) {
	// this.typeNode = typeNode;
	// }
	//

	public void applyChildren(IComponent parent, IMetaContext metaContext) {
		getChildren().forEach(metaComponentChild -> {
			IComponent component = metaComponentChild.apply(metaContext, parent);
			// parent.getChildrenNodes().add(component.getNode());
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
