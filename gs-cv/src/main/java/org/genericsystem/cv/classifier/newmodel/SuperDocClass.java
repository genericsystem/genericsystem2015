package org.genericsystem.cv.classifier.newmodel;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;

@SystemGeneric
@SuppressWarnings({ "rawtypes", "unchecked" })
public class SuperDocClass implements Generic {

	public Snapshot<DocClass.DocInstance> getAllDocs() {
		return (Snapshot) getSubInstances();
	}

	public Snapshot<DocClass> getAllDocClasses() {
		return (Snapshot) getInheritings();
	}
}