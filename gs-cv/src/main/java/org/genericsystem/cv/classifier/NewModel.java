package org.genericsystem.cv.classifier;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.classifier.NewModel.DocClass.DocInstance;
import org.genericsystem.cv.classifier.NewModel.Field.FieldInstance;
import org.genericsystem.cv.classifier.NewModel.Fields.FieldsInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Rect;

@SuppressWarnings({ "unchecked", "rawtypes" })
public class NewModel {

	public static void main(String[] args) {
		Engine engine = new Engine(SuperDocClass.class, DocClass.class, Fields.class, Field.class, RectX.class, RectY.class, RectW.class, RectH.class, Consolidated.class);

		SuperDocClass superDocClass = engine.find(SuperDocClass.class);
		Generic test = engine.setInstance(superDocClass, "test");
		System.out.println(test.detailedInfo());
		// DocClass idFrFront = superDocClass.setDocClass("id-fr-front");
		// DocInstance doc1 = idFrFront.setDoc("test1.png");

		// Engine engine = new Engine();
		// Generic superDocClass = engine.setInstance("superDocClass");
		// Generic idFrFront = engine.setInstance(Arrays.asList(superDocClass), "id-fr-front");
		// Generic doc1 = idFrFront.setInstance("test");

		System.out.println(superDocClass.detailedInfo());
		// System.out.println(idFrFront.detailedInfo());
		// System.out.println(doc1.detailedInfo());

		engine.close();
	}

	@SystemGeneric
	public static class SuperDocClass implements Generic {

		public DocClass setDocClass(String name) {
			return (DocClass) getRoot().setInstance(Arrays.asList(this), name);
		}

		public Snapshot<DocInstance> getAllDocs() {
			return (Snapshot) getSubInstances();
		}

		public Snapshot<DocClass> getAllDocClasses() {
			return (Snapshot) getInheritings();
		}
	}

	@SystemGeneric
	@Supers({ SuperDocClass.class })
	@InstanceClass(DocClass.DocInstance.class)
	public static class DocClass implements Generic {

		@SystemGeneric
		public static class DocInstance implements Generic {
		}

		public DocInstance setDoc(String filename) {
			return (DocInstance) setInstance(filename);
		}

		public Snapshot<DocInstance> getAllDocs() {
			return (Snapshot) getInstances();
		}

		public DocInstance getDoc(String filename) {
			return (DocInstance) getInstance(filename);
		}

	}

	@SystemGeneric
	@Components(DocClass.class)
	@InstanceClass(Fields.FieldsInstance.class)
	public static class Fields implements Generic {

		@SystemGeneric
		public static class FieldsInstance implements Generic {

			public DocClassInstance getDocClass() {
				return (DocClassInstance) getBaseComponent();
			}

			public FieldInstance setField(Rect rect, String consolidated) {
				String uid = ModelTools.generateZoneUID(rect);
				FieldInstance field = (FieldInstance) setHolder(getRoot().find(Field.class), uid);
				return field.setField(rect, consolidated);
			}

			public FieldInstance getField(String name) {
				return (FieldInstance) getHolder(getRoot().find(Field.class), name);
			}

			public Snapshot<Field> getAllField() {
				return (Snapshot) getHolders(getRoot().find(Field.class));
			}

			// public Snapshot<Field> getConsolidatedField() {
			// return (Snapshot) getAllField().filter(field -> field.iscons);
			// }
		}

	}

	@SystemGeneric
	@Components(Fields.class)
	@InstanceClass(Field.FieldInstance.class)
	public static class Field implements Generic {

		@SystemGeneric
		public static class FieldInstance implements Generic {

			public FieldsInstance getFields() {
				return (FieldsInstance) getBaseComponent();
			}

			public FieldInstance setField(Rect rect, String consolidated) {
				setHolder(getRoot().find(RectX.class), rect.x);
				setHolder(getRoot().find(RectY.class), rect.y);
				setHolder(getRoot().find(RectW.class), rect.width);
				setHolder(getRoot().find(RectH.class), rect.height);
				setHolder(getRoot().find(Consolidated.class), consolidated);
				return this;
			}

			public Rect getRect() {
				int x = (int) getHolder(getRoot().find(RectX.class)).getValue();
				int y = (int) getHolder(getRoot().find(RectY.class)).getValue();
				int width = (int) getHolder(getRoot().find(RectW.class)).getValue();
				int height = (int) getHolder(getRoot().find(RectH.class)).getValue();
				return new Rect(x, y, width, height);
			}

			public String getConsolidated() {
				return String.valueOf(getHolder(getRoot().find(Consolidated.class)).getValue());
			}

			public boolean isConsolidated() {
				return getConsolidated() != null;
			}
		}

	}

	@SystemGeneric
	@Components(Field.class)
	@InstanceValueClassConstraint(Integer.class)
	@PropertyConstraint
	public static class RectX implements Generic {
	}

	@SystemGeneric
	@Components(Field.class)
	@InstanceValueClassConstraint(Integer.class)
	@PropertyConstraint
	public static class RectY implements Generic {
	}

	@SystemGeneric
	@Components(Field.class)
	@InstanceValueClassConstraint(Integer.class)
	@PropertyConstraint
	public static class RectW implements Generic {
	}

	@SystemGeneric
	@Components(Field.class)
	@InstanceValueClassConstraint(Integer.class)
	@PropertyConstraint
	public static class RectH implements Generic {
	}

	@SystemGeneric
	@Components(Field.class)
	@InstanceValueClassConstraint(String.class)
	@PropertyConstraint
	public static class Consolidated implements Generic {
	}
}
