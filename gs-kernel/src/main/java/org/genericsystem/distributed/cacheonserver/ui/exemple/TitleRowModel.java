package org.genericsystem.distributed.cacheonserver.ui.exemple;

import java.io.Serializable;
import java.util.Objects;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import org.genericsystem.common.Generic;

public class TitleRowModel extends GenericModel {

	public TitleRowModel(Generic generic) {
		super(generic);
	}

	public ObservableValue<String> getTitleString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() + "(s)" : Objects.toString(value) + "(s)") + " Management");
	}
}
