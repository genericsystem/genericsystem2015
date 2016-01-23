package org.genericsystem.ui.components;

import javafx.scene.Group;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.ViewContext.RootViewContext;

public class GSApplication extends Element<Group> {

	public GSApplication(Object model, Group parentNode) {
		super(Group.class);
		new RootViewContext<>(model, this, parentNode);
	}
}
