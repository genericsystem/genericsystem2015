package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import java.io.Serializable;
import java.util.List;
import org.genericsystem.common.Protocole.ServerCacheProtocole;
import org.genericsystem.common.Vertex;

public abstract class AbstractGSLightClient extends AbstractGSClient implements ServerCacheProtocole {

	@Override
	public long pickNewTs() {
		KK
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Vertex getVertex(long id) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void close() {
		// TODO Auto-generated method stub

	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Vertex addInstance(long meta, List<Long> overrides, Serializable value, List<Long> components) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Vertex update(long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long merge(long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long setInstance(long meta, List<Long> overrides, Serializable value, List<Long> components) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long forceRemove(long generic) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long remove(long generic) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long conserveRemove(long generic) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long flush() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long tryFlush() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void clear() {
		// TODO Auto-generated method stub

	}

	@Override
	public void mount() {
		// TODO Auto-generated method stub

	}

	@Override
	public void unmount() {
		// TODO Auto-generated method stub

	}

	@Override
	<T> void send(Buffer buffer, Handler<Buffer> reponseHandler) {
		// TODO Auto-generated method stub

	}

	@Override
	public int getCacheLevel() {
		// TODO Auto-generated method stub
		return 0;
	}

}
