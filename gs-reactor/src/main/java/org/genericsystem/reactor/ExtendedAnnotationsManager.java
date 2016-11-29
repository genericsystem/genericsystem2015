package org.genericsystem.reactor;

import org.genericsystem.reactor.gscomponents.TagImpl;

public class ExtendedAnnotationsManager extends AnnotationsManager {

	@Override
	public void processChildrenAnnotations(Tag tag) {
		((TagImpl) tag).setTagNode(tag.getRootTag().buildTagNode(tag));
	}
}
