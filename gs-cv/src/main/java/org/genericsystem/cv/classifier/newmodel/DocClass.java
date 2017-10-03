package org.genericsystem.cv.classifier.newmodel;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;

@SystemGeneric
@Supers({ SuperDocClass.class })
@InstanceClass(DocClass.DocInstance.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
public class DocClass implements Generic {

	@SystemGeneric
	public static class DocInstance implements Generic {
	}

	public DocClass.DocInstance setDoc(String json) {
		return (DocClass.DocInstance) setInstance(json);
	}

	public Snapshot<DocClass.DocInstance> getAllDocs() {
		return (Snapshot) getInstances();
	}

	public DocClass.DocInstance getDoc(String filename) {
		return (DocClass.DocInstance) getInstance(filename);
	}

	@SystemGeneric
	@Components(DocClass.class)
	@PropertyConstraint
	@InstanceValueClassConstraint(String.class)
	public static class ClassDirectory implements Generic {
	}

}

// Add the attributes from the old model (filename, timestamp...)