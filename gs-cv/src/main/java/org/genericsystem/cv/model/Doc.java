package org.genericsystem.cv.model;

import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;

/**
 * This class stores the documents that belong to the parent class.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@Components(DocClass.class)
@InstanceClass(DocInstance.class)
public class Doc implements Generic {

	public static class DocInstance implements Generic {

		public DocClassInstance getDocClass() {
			return (DocClassInstance) this.getBaseComponent();
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		public List<ZoneInstance> getZones() {
			Snapshot<ZoneInstance> shot = (Snapshot) getBaseComponent().getHolders(getRoot().find(ZoneGeneric.class));
			return shot.toList();
		}

	}

	public DocInstance setDoc(String name, DocClassInstance docClassInstance) {
		return (DocInstance) setInstance(name, docClassInstance);
	}

	public DocInstance getDoc(String name, DocClassInstance docClassInstance) {
		return (DocInstance) getInstance(name, docClassInstance);
	}

}
