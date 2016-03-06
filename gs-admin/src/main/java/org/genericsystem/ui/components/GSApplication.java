package org.genericsystem.ui.components;

import javafx.scene.Group;

import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.distributed.ui.ViewContext.RootViewContext;

public class GSApplication extends Element<Group> {

	public GSApplication(Model model, Group parentNode) {
		super(null, Group.class);
		new RootViewContext<>(model, this, parentNode);
	}
}