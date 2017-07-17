package org.genericsystem.cv.model;

import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocFilename;
import org.genericsystem.cv.model.Doc.DocFilename.DocFilenameInstance;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.Doc.DocTimestamp.DocTimestampInstance;
import org.genericsystem.cv.model.Doc.RefreshTimestamp.RefreshTimestampInstance;
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
@Dependencies(DocFilename.class)
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
		
		public DocFilenameInstance setDocFilename(String filename) {
			return (DocFilenameInstance) setHolder(getRoot().find(DocFilename.class), filename);
		}
		
		public DocFilenameInstance getDocFilename(String filename) {
			return (DocFilenameInstance) getHolder(getRoot().find(DocFilename.class), filename);
		}

	}

	public DocInstance setDoc(String name, DocClassInstance docClassInstance) {
		return (DocInstance) setInstance(name, docClassInstance);
	}

	public DocInstance getDoc(String name, DocClassInstance docClassInstance) {
		return (DocInstance) getInstance(name, docClassInstance);
	}
	
	@SystemGeneric
	@Components(Doc.class)
	@InstanceValueClassConstraint(String.class)
	@PropertyConstraint
	@InstanceClass(DocFilenameInstance.class)
	public static class DocFilename implements Generic {
		
		public static class DocFilenameInstance implements Generic {
			
			public DocInstance getDoc() {
				return (DocInstance) getBaseComponent();
			}
		}
		
	}
	
	@SystemGeneric
	@Components(Doc.class)
	@PropertyConstraint
	@InstanceClass(RefreshTimestampInstance.class)
	public static class RefreshTimestamp implements Generic {
		
		public static class RefreshTimestampInstance implements Generic {

			public DocInstance getDoc() {
				return (DocInstance) getBaseComponent();
			}
		}
		
		public RefreshTimestampInstance setRefreshTimestamp(String timestamp, DocInstance docInstance) {
			return (RefreshTimestampInstance) setInstance(timestamp, docInstance);
		}
		
		public RefreshTimestampInstance getRefreshTimestamp(DocInstance docInstance) {
			return (RefreshTimestampInstance) getInstance(docInstance);
		}
	}
	
	@SystemGeneric
	@Components(Doc.class)
	@PropertyConstraint
	@InstanceClass(DocTimestampInstance.class)
	public static class DocTimestamp implements Generic {
		
		public static class DocTimestampInstance implements Generic {

			public DocInstance getDoc() {
				return (DocInstance) getBaseComponent();
			}
		}
		
		public DocTimestampInstance setDocTimestamp(String timestamp, DocInstance docInstance) {
			return (DocTimestampInstance) setInstance(timestamp, docInstance);
		}
		
		public DocTimestampInstance getDocTimestamp(DocInstance docInstance) {
			return (DocTimestampInstance) getInstance(docInstance);
		}
	}

}
