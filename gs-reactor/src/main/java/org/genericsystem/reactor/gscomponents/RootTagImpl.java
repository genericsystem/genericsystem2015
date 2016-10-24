package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Tag.RootTag;

public class RootTagImpl extends GSDiv implements RootTag {

	private AnnotationsManager annotationsManager;

	public RootTagImpl() {
		super((Tag) null);
	}

	@Override
	public void beforeProcessAnnotations() {
		annotationsManager = new AnnotationsManager();
	}

	@Override
	public AnnotationsManager getAnnotationsManager() {
		return annotationsManager;
	}
}
