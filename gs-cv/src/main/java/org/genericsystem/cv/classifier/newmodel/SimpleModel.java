package org.genericsystem.cv.classifier.newmodel;

import java.io.IOException;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ConsolidatedType.ConsolidatedInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocClassType.DocClassInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocType.DocInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgDocRel.ImgDocLink;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgPathType.ImgPathInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgTimestampType.ImgTimestampInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.LayoutType.LayoutInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNumType.ZoneNumInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneType.ZoneInstance;
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
	@InstanceClass(DocClassInstance.class)
	public static class DocClassType implements Generic {

		@SystemGeneric
		public static class DocClassInstance implements Generic {

		}

	}

	@SystemGeneric
	@Components(DocClassType.class)
	@InstanceClass(LayoutInstance.class)
	public static class LayoutType implements Generic {

		@SystemGeneric
		public static class LayoutInstance implements Generic {

			public DocClassInstance getDocClassInstance() {
				return (DocClassInstance) getBaseComponent();
			}

			// Add utility methods to wrap interaction with inheritance
		}
	}

	/**
	 * ImgDocRel represents the relation (many to one) between an abstract {@link DocType} and a concrete {@link ImgType}.
	 * 
	 * @author Pierrik Lassalas
	 */
	@SystemGeneric
	@Components({ ImgType.class, DocType.class })
	@InstanceClass(ImgDocLink.class)
	public static class ImgDocRel implements Generic {

		@SystemGeneric
		public static class ImgDocLink implements Generic {

			public ImgInstance getImgInstance() {
				return (ImgInstance) getComponent(0);
			}

			public DocInstance getDocInstance() {
				return (DocInstance) getComponent(1);
			}

		}
	}

	/**
	 * A DocType represents the "abstract" document, which can be linked to one or more {@link ImgType} through {@link ImgDocRel}. At this stage, duplicates of ImgType are removed.
	 * 
	 * @author Pierrik Lassalas
	 */
	@SystemGeneric
	@Components(DocClassType.class)
	@SingularConstraint
	@InstanceClass(DocInstance.class)
	public static class DocType implements Generic {

		@SystemGeneric
		public static class DocInstance implements Generic {

			public DocClassInstance getDocClassInstance() {
				return (DocClassInstance) getBaseComponent();
			}
		}

	}

	/**
	 * An ImgType represents the actual image of the processed document. Duplicates of the same document can exist as multiple instances of this class.
	 * 
	 * @author Pierrik Lassalas
	 */
	@SystemGeneric
	@InstanceClass(ImgInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class ImgType implements Generic {

		@SystemGeneric
		public static class ImgInstance implements Generic {

			public Snapshot<ZoneInstance> getZoneInstances() {
				return (Snapshot) getHolders(getRoot().find(ZoneType.class));
			}

			public ZoneInstance setZone(Rect rect) {
				try {
					String json = mapper.writeValueAsString(rect);
					return (ZoneInstance) setHolder(getRoot().find(ZoneType.class), json);
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

			public ZoneInstance setZone(String json) {
				return (ZoneInstance) setHolder(getRoot().find(ZoneType.class), json);
			}

			public ZoneInstance getZone(String json) {
				return (ZoneInstance) getHolder(getRoot().find(ZoneType.class), json);
			}

			public ImgPathInstance setImgPath(String relativePath) {
				return (ImgPathInstance) setHolder(getRoot().find(ImgPathType.class), relativePath);
			}

			public ImgPathInstance getImgPath() {
				return (ImgPathInstance) getHolder(getRoot().find(ImgPathType.class));
			}

			public ImgTimestampInstance setImgTimestamp(Long timestamp) {
				return (ImgTimestampInstance) setHolder(getRoot().find(ImgTimestampType.class), timestamp);
			}

			public ImgTimestampInstance getImgTimestamp() {
				return (ImgTimestampInstance) getHolder(getRoot().find(ImgTimestampType.class));
			}

		}

		public Snapshot<ImgInstance> getImgInstances() {
			return (Snapshot) getInstances();
		}

		public ImgInstance setImg(String name) {
			return (ImgInstance) setInstance(name);
		}

		public ImgInstance getImg(String name) {
			return (ImgInstance) getInstance(name);
		}
	}

	@SystemGeneric
	@Components(ImgType.class)
	@InstanceClass(ZoneInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class ZoneType implements Generic {

		@SystemGeneric
		public static class ZoneInstance implements Generic {

			public ImgInstance getImgInstance() {
				return (ImgInstance) getBaseComponent();
			}

			public ZoneNumInstance setZoneNum(int num) {
				return (ZoneNumInstance) setHolder(getRoot().find(ZoneNumType.class), num);
			}

			public ZoneNumInstance getZoneNum() {
				return (ZoneNumInstance) getHolder(getRoot().find(ZoneNumType.class));
			}

			public ConsolidatedInstance setConsolidated(String consolidated) {
				return (ConsolidatedInstance) setHolder(getRoot().find(ConsolidatedType.class), consolidated);
			}

			public ConsolidatedInstance getConsolidated() {
				return (ConsolidatedInstance) getHolder(getRoot().find(ConsolidatedType.class));
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
	@Components(ZoneType.class)
	@PropertyConstraint
	@InstanceClass(ZoneNumInstance.class)
	@InstanceValueClassConstraint(Integer.class)
	public static class ZoneNumType implements Generic {

		@SystemGeneric
		public static class ZoneNumInstance implements Generic {

			public ZoneInstance getZoneInstance() {
				return (ZoneInstance) getBaseComponent();
			}
		}

	}

	@SystemGeneric
	@Components(ZoneType.class)
	@PropertyConstraint
	@InstanceClass(ConsolidatedInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class ConsolidatedType implements Generic {

		@SystemGeneric
		public static class ConsolidatedInstance implements Generic {

			public ZoneInstance getZoneInstance() {
				return (ZoneInstance) getBaseComponent();
			}
		}
	}

	@SystemGeneric
	@Components(ImgType.class)
	@PropertyConstraint
	@InstanceClass(ImgPathInstance.class)
	@InstanceValueClassConstraint(String.class)
	public static class ImgPathType implements Generic {

		@SystemGeneric
		public static class ImgPathInstance implements Generic {

			public ImgInstance getImgInstance() {
				return (ImgInstance) getBaseComponent();
			}
		}
	}

	@SystemGeneric
	@Components(ImgType.class)
	@PropertyConstraint
	@InstanceClass(ImgTimestampInstance.class)
	@InstanceValueClassConstraint(Long.class)
	public static class ImgTimestampType implements Generic {

		@SystemGeneric
		public static class ImgTimestampInstance implements Generic {

			public ImgInstance getImgInstance() {
				return (ImgInstance) getBaseComponent();
			}

		}
	}
}
