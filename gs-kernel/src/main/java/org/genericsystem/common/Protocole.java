package org.genericsystem.common;


public interface Protocole {

	long pickNewTs();

	Vertex getVertex(long id);

	void close();

}
