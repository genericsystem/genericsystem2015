package org.genericsystem.ui.components;

import javafx.scene.Group;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.ModelContext;
import org.genericsystem.ui.ViewContext;

public class GSApplication extends Element<Group> {

	public GSApplication(Object model, Group parentNode) {
		super(Group.class);
		new ViewContext<Group>(null, new ModelContext(null, model), this, parentNode).getNode();
	}
}
