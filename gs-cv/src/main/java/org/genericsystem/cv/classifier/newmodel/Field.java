package org.genericsystem.cv.classifier.newmodel;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.classifier.newmodel.Fields.FieldsInstance;
import org.opencv.core.Rect;

@SystemGeneric
@Components(Fields.class)
@InstanceClass(Field.FieldInstance.class)
public class Field implements Generic {

	@SystemGeneric
	public static class FieldInstance implements Generic {

		public FieldsInstance getFields() {
			return (FieldsInstance) getBaseComponent();
		}

		public Field.FieldInstance setField(Rect rect, String consolidated) {
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