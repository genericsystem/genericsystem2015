package org.genericsystem.cv.classifier.newmodel;

import java.io.IOException;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Consolidated.ConsolidatedInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Doc.DocInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocPath.DocPathInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocTimestamp.DocTimestampInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone.ZoneInstance;
import org.opencv.core.Rect;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Simple Model to store the data extracted from documents in Generic System.
 * 
 * @author Pierrik Lassalas
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public class SimpleModel {

	private static final ObjectMapper mapper = new ObjectMapper();

	@SystemGeneric
	@InstanceClass(ImgFilterInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class ImgFilter implements Generic {

		@SystemGeneric
		public static class ImgFilterInstance implements Generic {

		}

		public Snapshot<ImgFilterInstance> getImgFilters() {
			return (Snapshot) getInstances();
		}

		public ImgFilterInstance setImgFilter(String filtername) {
			return (ImgFilterInstance) setInstance(filtername);
		}

		public ImgFilterInstance getImgFilter(String filtername) {
			return (ImgFilterInstance) getInstance(filtername);
		}
	}

	@SystemGeneric
	@InstanceClass(DocInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class Doc implements Generic {

		@SystemGeneric
		public static class DocInstance implements Generic {

			public Snapshot<ZoneInstance> getZoneInstances() {
				return (Snapshot) getHolders(getRoot().find(Zone.class));
			}

			public ZoneInstance setZone(Rect rect) {
				try {
					String json = mapper.writeValueAsString(rect);
					return (ZoneInstance) setHolder(getRoot().find(Zone.class), json);
				} catch (JsonProcessingException e) {
					throw new IllegalStateException("An error has occured while converting the rectangle to a json string", e);
				}
			}

			public ZoneInstance getZone(Rect rect) {
				try {
					String json = mapper.writeValueAsString(rect);
					return getZone(json);
				} catch (JsonProcessingException e) {
					throw new IllegalStateException("An error has occured while converting the rectangle to a json string", e);
				}
			}

			public ZoneInstance getZone(String json) {
				return (ZoneInstance) getHolder(getRoot().find(Zone.class), json);
			}

			public DocPathInstance setDocPath(String relativePath) {
				return (DocPathInstance) setHolder(getRoot().find(DocPath.class), relativePath);
			}

			public DocPathInstance getDocPath() {
				return (DocPathInstance) getHolder(getRoot().find(DocPath.class));
			}

			public DocTimestampInstance setDocTimestamp(Long timestamp) {
				return (DocTimestampInstance) setHolder(getRoot().find(DocTimestamp.class), timestamp);
			}

			public DocTimestampInstance getDocTimestamp() {
				return (DocTimestampInstance) getHolder(getRoot().find(DocTimestamp.class));
			}

		}

		public Snapshot<DocInstance> getDocInstances() {
			return (Snapshot) getInstances();
		}

		public DocInstance setDoc(String name) {
			return (DocInstance) setInstance(name);
		}

		public DocInstance getDoc(String name) {
			return (DocInstance) getInstance(name);
		}
	}

	@SystemGeneric
	@Components(Doc.class)
	@InstanceClass(ZoneInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class Zone implements Generic {

		@SystemGeneric
		public static class ZoneInstance implements Generic {

			public DocInstance getDocInstance() {
				return (DocInstance) getBaseComponent();
			}

			public ConsolidatedInstance setConsolidated(String consolidated) {
				return (ConsolidatedInstance) setHolder(getRoot().find(Consolidated.class), consolidated);
			}

			public ConsolidatedInstance getConsolidated() {
				return (ConsolidatedInstance) getHolder(getRoot().find(Consolidated.class));
			}

			public Rect getZoneRect() {
				try {
					Rect rect = mapper.readValue(getValue().toString(), Rect.class);
					return rect;
				} catch (IOException e) {
					throw new IllegalStateException("An error has occured while converting the json to a rectangle", e);
				}
			}
		}

		public Snapshot<ZoneInstance> getZoneInstances() {
			return (Snapshot) getInstances();
		}

		public Snapshot<ZoneInstance> getEmptyZoneInstances() {
			return (Snapshot) getInstances().filter(zone -> {
				ConsolidatedInstance consolidated = ((ZoneInstance) zone).getConsolidated();
				return consolidated == null || consolidated.getValue() == null || "".equals(consolidated.getValue());
			});
		}

		public ZoneInstance setZone(String json) {
			return (ZoneInstance) setInstance(json);
		}

		public ZoneInstance getZone(String json) {
			return (ZoneInstance) getInstance(json);
		}
	}

	@SystemGeneric
	@Components(Zone.class)
	@PropertyConstraint
	@InstanceClass(ConsolidatedInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class Consolidated implements Generic {

		@SystemGeneric
		public static class ConsolidatedInstance implements Generic {

			public ZoneInstance getZoneInstance() {
				return (ZoneInstance) getBaseComponent();
			}

		}
	}

	@SystemGeneric
	@Components(Doc.class)
	@PropertyConstraint
	@InstanceClass(DocPathInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class DocPath implements Generic {

		@SystemGeneric
		public static class DocPathInstance implements Generic {

			public DocInstance getDocInstance() {
				return (DocInstance) getBaseComponent();
			}
		}
	}

	@SystemGeneric
	@Components(Doc.class)
	@PropertyConstraint
	@InstanceClass(DocTimestampInstance.class)
	@InstanceValueClassConstraint(Long.class)
	public static class DocTimestamp implements Generic {

		@SystemGeneric
		public static class DocTimestampInstance implements Generic {

			public DocInstance getDocInstance() {
				return (DocInstance) getBaseComponent();
			}

		}
	}
}
