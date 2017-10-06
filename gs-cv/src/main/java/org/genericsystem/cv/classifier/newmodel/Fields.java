package org.genericsystem.cv.classifier.newmodel;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.utils.ModelTools;
import org.opencv.core.Rect;

@SystemGeneric
@Components(DocClass.class)
@PropertyConstraint
@InstanceClass(Fields.FieldsInstance.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
public class Fields implements Generic {

	@SystemGeneric
	public static class FieldsInstance implements Generic {

		public DocClassInstance getDocClass() {
			return (DocClassInstance) getBaseComponent();
		}

		public Field.FieldInstance setField(Rect rect, String consolidated) {
			String uid = ModelTools.generateZoneUID(rect);
			Field.FieldInstance field = (Field.FieldInstance) setHolder(getRoot().find(Field.class), uid);
			return field.setField(rect, consolidated);
		}

		public Field.FieldInstance getField(String name) {
			return (Field.FieldInstance) getHolder(getRoot().find(Field.class), name);
		}

		public Snapshot<Field> getAllField() {
			return (Snapshot) getHolders(getRoot().find(Field.class));
		}

		// public Snapshot<Field> getConsolidatedField() {
		// return (Snapshot) getAllField().filter(field -> field.iscons);
		// }
	}

}