package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.kernel.Statics;

import com.google.common.base.Supplier;

public class CosClient extends AbstractGSClient implements CosProtocole {


	public CosClient(String host, int port, String path) {
		super(host, port, path);
	}
	
	@SuppressWarnings("unchecked")
	protected <R> R unsafeConcurrencyControlException(Supplier<Object> unsafe) throws ConcurrencyControlException {
		Object result = unsafe.get();
		if (result instanceof ConcurrencyControlException)
			throw (ConcurrencyControlException) result;
		return (R) result;
	}

	@Override
	public long newCacheId() {
		return unsafeException(() -> unsafe(() -> newCacheIdPromise().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> newCacheIdPromise() {
		return promise(NEW_CACHE, buff -> buff.getLongThrowException(), buffer -> buffer);
	}

	@Override
	public long shiftTs(long cacheId) {
		return unsafeException(() -> unsafe(() -> newShiftTsPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> newShiftTsPromise(long cacheId) {
		return promise(SHIFT_TS, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		return unsafeException(() -> unsafe(() -> getDependenciesPromise(cacheId, id).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> getDependenciesPromise(long cacheId, long id) {
		return promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArrayThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(id));
	}

	@Override
	public long addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeException(() -> unsafe(() -> addInstancePromise(cacheId, meta, overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> addInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return promise(ADD_INSTANCE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(meta, overrides, value, components));
	}

	@Override
	public long update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeException(() -> unsafe(() -> updatePromise(cacheId, update, overrides, value, newComponents).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> updatePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return promise(UPDATE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(update, overrides, value, newComponents));
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeException(() -> unsafe(() -> mergePromise(cacheId, update, overrides, value, newComponents).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> mergePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return promise(MERGE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(update, overrides, value, newComponents));
	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeException(() -> unsafe(() -> setInstancePromise(cacheId, meta, overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> setInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return promise(SET_INSTANCE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(meta, overrides, value, components));
	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> forceRemovePromise(cacheId, generic).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> forceRemovePromise(long cacheId, long generic) {
		return promise(FORCE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long remove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> removePromise(cacheId, generic).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> removePromise(long cacheId, long generic) {
		return promise(REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		return unsafeException(() -> unsafe(() -> conserveRemovePromise(cacheId, generic).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> conserveRemovePromise(long cacheId, long generic) {
		return promise(CONSERVE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic));
	}

	@Override
	public long flush(long cacheId) {
		return unsafeException(() -> unsafe(() -> flushPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> flushPromise(long cacheId) {
		return promise(FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long tryFlush(long cacheId) throws ConcurrencyControlException {
		return unsafeConcurrencyControlException(() -> unsafeException(() -> unsafe(() -> tryFlushPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT))));
	}

	public CompletableFuture<Object> tryFlushPromise(long cacheId) {
		return promise(TRY_FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long clear(long cacheId) {
		return unsafeException(() -> unsafe(() -> clearPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> clearPromise(long cacheId) {
		return promise(CLEAR, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long mount(long cacheId) {
		return unsafeException(() -> unsafe(() -> mountPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> mountPromise(long cacheId) {
		return promise(MOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public long unmount(long cacheId) {
		return unsafeException(() -> unsafe(() -> unmountPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> unmountPromise(long cacheId) {
		return promise(UNMOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId));
	}

	@Override
	public int getCacheLevel(long cacheId) {
		return unsafeException(() -> unsafe(() -> getCacheLevelPromise(cacheId).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)));
	}

	public CompletableFuture<Object> getCacheLevelPromise(long cacheId) {
		return promise(GET_CACHE_LEVEL, buff -> buff.getIntThrowException(), buffer -> buffer.appendLong(cacheId));
	}

}
