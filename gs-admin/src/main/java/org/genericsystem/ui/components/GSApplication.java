package org.genericsystem.ui.components;

import java.util.List;
import java.util.function.Function;
import javafx.scene.Group;
import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ViewContext.RootViewContext;

public class GSApplication extends Element<Group> {

	public GSApplication(Model model, Group parentNode) {
		super(null, Group.class);
		new RootViewContext<>(model, this, parentNode);
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Function<Group, List> getGraphicChildren() {
		return Group::getChildren;
	}
}