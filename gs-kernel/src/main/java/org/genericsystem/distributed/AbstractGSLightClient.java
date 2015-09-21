package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.common.Protocole.ServerCacheProtocole;
import org.genericsystem.common.Vertex;

public abstract class AbstractGSLightClient extends AbstractGSClient implements ServerCacheProtocole {

	@Override
	public long shiftTs(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(SHIFT_TS).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));

	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(cacheId).appendLong(id), buff -> {
			task.handle(new GSBuffer(buff).getGSVertexArray());
		}));

	}

	@Override
	public Vertex addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(ADD_INSTANCE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	@Override
	public Vertex update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(UPDATE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(MERGE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));

	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(SET_INSTANCE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));

	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(FORCE_REMOVE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long remove(long cacheId, long generic) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(REMOVE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(CONSERVE_REMOVE).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long flush(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(FLUSH).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long tryFlush(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(TRY_FLUSH).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));

	}

	@Override
	public long clear(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(TRY_FLUSH).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long mount(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(MOUNT).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public long unmount(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(UNMOUNT).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	@Override
	public int getCacheLevel(long cacheId) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_CACHE_LEVEL).appendLong(cacheId), buff -> task.handle(new GSBuffer(buff).getInt())));
	}

}
