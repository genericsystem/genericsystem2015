package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;
import com.google.common.base.Supplier;

public abstract class AbstractGSLightClient extends AbstractGSClient implements ServerCacheProtocole {

	@SuppressWarnings("unchecked")
	protected <R> R unsafeException(Supplier<Object> unsafe) {
		Object result = unsafe.get();
		if (result instanceof RuntimeException)
			throw (RuntimeException) result;
		return (R) result;
	}

	@Override
	public long newCacheId() {
		return unsafeException(() -> unsafe(() -> newCacheIdPromise().get()));
	}

	public CompletableFuture<Object> newCacheIdPromise() {
		return promise(NEW_CACHE, buff -> {
			System.out.println("Coucou");
			return buff.getLong();
		}, buffer -> buffer);
	}

	@Override
	public long shiftTs(long cacheId) {
		return unsafeException(() -> unsafe(() -> newShiftTsPromise(cacheId).get()));
	}

	public CompletableFuture<Object> newShiftTsPromise(long cacheId) {
		return promise(SHIFT_TS, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		return unsafeException(() -> unsafe(() -> getDependenciesPromise(cacheId, id).get()));
	}

	public CompletableFuture<Vertex[]> getDependenciesPromise(long cacheId, long id) {
		return promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArray(), buffer -> buffer.appendLong(cacheId).appendLong(id));
	}

	@Override
	public long addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeException(() -> unsafe(() -> addInstancePromise(cacheId, meta, overrides, value, components).get()));
	}

	public CompletableFuture<Object> addInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return promise(ADD_INSTANCE, buff -> buff.getLongThrowException(), buffer -> ((GSBuffer) buffer.appendLong(cacheId)).appendGSSignature(meta, overrides, value, components));
	}

	@Override
	public long update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeException(() -> unsafeException(() -> unsafe(() -> updatePromise(cacheId, update, overrides, value, newComponents).get())));
	}

	public CompletableFuture<Object> updatePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return promise(UPDATE, buff -> buff.getLongThrowException(), buffer -> ((GSBuffer) buffer).appendGSSignature(update, overrides, value, newComponents));
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeException(() -> unsafe(() -> mergePromise(cacheId, update, overrides, value, newComponents).get()));
	}

	public CompletableFuture<Object> mergePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return promise(MERGE, buff -> buff.getLongThrowException(), buffer -> ((GSBuffer) buffer.appendLong(cacheId)).appendGSSignature(update, overrides, value, newComponents));
	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeException(() -> unsafe(() -> setInstancePromise(cacheId, meta, overrides, value, components).get()));
	}

	public CompletableFuture<Object> setInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return promise(SET_INSTANCE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(meta, overrides, value, components));
	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> forceRemovePromise(cacheId, generic).get()));
	}

	public CompletableFuture<Object> forceRemovePromise(long cacheId, long generic) {
		return promise(FORCE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long remove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> removePromise(cacheId, generic).get()));
	}

	public CompletableFuture<Object> removePromise(long cacheId, long generic) {
		return promise(REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> conserveRemovePromise(cacheId, generic).get()));
	}

	public CompletableFuture<Object> conserveRemovePromise(long cacheId, long generic) {
		return promise(CONSERVE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long flush(long cacheId) {
		return unsafeException(() -> unsafe(() -> flushPromise(cacheId).get()));
	}

	public CompletableFuture<Object> flushPromise(long cacheId) {
		return promise(FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long tryFlush(long cacheId) {
		return unsafeException(() -> unsafe(() -> tryFlushPromise(cacheId).get()));
	}

	public CompletableFuture<Object> tryFlushPromise(long cacheId) {
		return promise(TRY_FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long clear(long cacheId) {
		return unsafeException(() -> unsafe(() -> clearPromise(cacheId).get()));
	}

	public CompletableFuture<Object> clearPromise(long cacheId) {
		return promise(CLEAR, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long mount(long cacheId) {
		return unsafeException(() -> unsafe(() -> mountPromise(cacheId).get()));
	}

	public CompletableFuture<Object> mountPromise(long cacheId) {
		return promise(MOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long unmount(long cacheId) {
		return unsafeException(() -> unsafe(() -> unmountPromise(cacheId).get()));
	}

	public CompletableFuture<Object> unmountPromise(long cacheId) {
		return promise(UNMOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public int getCacheLevel(long cacheId) {
		return unsafeException(() -> unsafe(() -> getCacheLevelPromise(cacheId).get()));
	}

	public CompletableFuture<Object> getCacheLevelPromise(long cacheId) {
		return promise(GET_CACHE_LEVEL, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

}
