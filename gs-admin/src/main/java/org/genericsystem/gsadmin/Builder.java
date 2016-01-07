package org.genericsystem.gsadmin;

import org.genericsystem.ui.Element;

public interface Builder {
	public void init(Element<?> parent);

	// A builder must have a build method that call directly the underlying constructor
}
