package org.genericsystem.cv.model;

import java.time.LocalDate;
import java.util.Date;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp;
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp.ZoneTimestampInstance;

/**
 * This class stores the results of the OCR for a given document, zone, and
 * filter.
 * 
 * @author Jean Mathorel
 * @author Pierrik Lassalas
 *
 */
@SystemGeneric
@PropertyConstraint
@Dependencies(ZoneTimestamp.class)
@Components({ Doc.class, ZoneGeneric.class, ImgFilter.class })
@InstanceClass(ZoneTextInstance.class)
public class ZoneText implements Generic {

	public static class ZoneTextInstance implements Generic {

		public DocInstance getDoc() {
			return (DocInstance) this.getComponent(0);
		}

		public ZoneInstance getZone() {
			return (ZoneInstance) this.getComponent(1);
		}

		public ImgFilterInstance getImgFilter() {
			return (ImgFilterInstance) this.getComponent(2);
		}
		
		public int getZoneNum() {
			return (int) getZone().getValue();
		}
		
	}

	public ZoneTextInstance setZoneText(String text, DocInstance doc, ZoneInstance zone, ImgFilterInstance imgFilter) {
		return (ZoneTextInstance) setInstance(text, doc, zone, imgFilter);
	}

	public ZoneTextInstance getZoneText(DocInstance doc, ZoneInstance zone, ImgFilterInstance imgFilter) {
		return (ZoneTextInstance) getInstance(doc, zone, imgFilter);
	}
	
	@SystemGeneric
	@Components(ZoneText.class)
	@PropertyConstraint
	@InstanceClass(ZoneTimestampInstance.class)
	public static class ZoneTimestamp implements Generic {
		
		public static class ZoneTimestampInstance implements Generic {
			
			public ZoneTextInstance getZoneText() {
				return (ZoneTextInstance) getBaseComponent();
			}
		}
		
		public ZoneTimestampInstance setZoneTimestamp(String timestamp, ZoneTextInstance zoneTextInstance) {
			return (ZoneTimestampInstance) setInstance(timestamp, zoneTextInstance);
		}
		
		public ZoneTimestampInstance getZoneTimestamp(ZoneTextInstance zoneTextInstance) {
			return (ZoneTimestampInstance) getInstance(zoneTextInstance);
		}
	}
}
