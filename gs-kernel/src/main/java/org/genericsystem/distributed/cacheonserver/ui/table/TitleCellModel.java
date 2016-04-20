package org.genericsystem.distributed.cacheonserver.ui.table;

import java.io.Serializable;
import java.util.Objects;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;

/**
 * @author Nicolas Feybesse
 *
 */
public class TitleCellModel extends GenericModel {

	public TitleCellModel(Generic generic) {
		super(generic);
	}

	@Override
	public ObservableValue<String> getString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)));
	}
}
