package org.genericsystem.common;

/**
 * @author Nicolas Feybesse
 *
 */
public interface Protocol {

	long pickNewTs();

	Vertex getVertex(long id);

	void close();

}
