package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.Tag.RootTag;

public class RootTagImpl extends GSDiv implements RootTag {

	private AnnotationsManager annotationsManager;

	public RootTagImpl() {
		annotationsManager = new AnnotationsManager();
		beforeProcessAnnotations();
		getRootTag().getAnnotationsManager().processAnnotations(this);
		init();
	}

	@Override
	public AnnotationsManager getAnnotationsManager() {
		return annotationsManager;
	}
}
