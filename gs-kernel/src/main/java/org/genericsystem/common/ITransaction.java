package org.genericsystem.common;

import org.genericsystem.defaults.DefaultVertex;

public interface ITransaction<T extends DefaultVertex<T>> extends IDifferential<T> {
	long getTs();

	AbstractRoot<T> getRoot();
};