package org.genericsystem.common;


public interface Protocol {

	long pickNewTs();

	Vertex getVertex(long id);

	void close();

}
