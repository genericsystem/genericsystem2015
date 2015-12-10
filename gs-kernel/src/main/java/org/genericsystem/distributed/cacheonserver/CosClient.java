package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;

public class CosClient extends AbstractGSClient implements CosProtocole {

	public CosClient(String host, int port, String path) {
		super(host, port, path);
	}

	@Override
	public long newCacheId() {
		return unsafeRollbackManaged(newCacheIdPromise());
	}

	public CompletableFuture<Long> newCacheIdPromise() {
		return unsafeExceptionPromise(promise(NEW_CACHE, buff -> buff.getLongThrowException(), buffer -> buffer));
	}

	@Override
	public long shiftTs(long cacheId) {
		return unsafeRollbackManaged(newShiftTsPromise(cacheId));
	}

	public CompletableFuture<Long> newShiftTsPromise(long cacheId) {
		return unsafeExceptionPromise(promise(SHIFT_TS, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public Vertex[] getDependencies(long cacheId, long id) {
		return unsafeRollbackManaged(getDependenciesPromise(cacheId, id));
	}

	public CompletableFuture<Vertex[]> getDependenciesPromise(long cacheId, long id) {
		return unsafeExceptionPromise(promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArrayThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(id)));
	}

	@Override
	public long addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeRollbackManaged(addInstancePromise(cacheId, meta, overrides, value, components));
	}

	public CompletableFuture<Long> addInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeExceptionPromise(promise(ADD_INSTANCE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(meta, overrides, value, components)));
	}

	@Override
	public long update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeRollbackManaged(updatePromise(cacheId, update, overrides, value, newComponents));
	}

	public CompletableFuture<Long> updatePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeExceptionPromise(promise(UPDATE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(update, overrides, value, newComponents)));
	}

	@Override
	public long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeRollbackManaged(mergePromise(cacheId, update, overrides, value, newComponents));
	}

	public CompletableFuture<Long> mergePromise(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> newComponents) {
		return unsafeExceptionPromise(promise(MERGE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(update, overrides, value, newComponents)));
	}

	@Override
	public long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeRollbackManaged(setInstancePromise(cacheId, meta, overrides, value, components));
	}

	public CompletableFuture<Long> setInstancePromise(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components) {
		return unsafeExceptionPromise(promise(SET_INSTANCE, buff -> buff.getLongThrowException(), buffer -> new GSBuffer(buffer).appendLong(cacheId).appendGSSignature(meta, overrides, value, components)));
	}

	@Override
	public long forceRemove(long cacheId, long generic) {
		return unsafeRollbackManaged(forceRemovePromise(cacheId, generic));
	}

	public CompletableFuture<Long> forceRemovePromise(long cacheId, long generic) {
		return unsafeExceptionPromise(promise(FORCE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic)));
	}

	@Override
	public long remove(long cacheId, long generic) {
		return unsafeRollbackManaged(removePromise(cacheId, generic));
	}

	public CompletableFuture<Long> removePromise(long cacheId, long generic) {
		return unsafeExceptionPromise(promise(REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic)));
	}

	@Override
	public long conserveRemove(long cacheId, long generic) {
		return unsafeRollbackManaged(conserveRemovePromise(cacheId, generic));
	}

	public CompletableFuture<Long> conserveRemovePromise(long cacheId, long generic) {
		return unsafeExceptionPromise(promise(CONSERVE_REMOVE, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId).appendLong(generic)));
	}

	@Override
	public long flush(long cacheId) {
		return unsafeRollbackManaged(flushPromise(cacheId));
	}

	public CompletableFuture<Long> flushPromise(long cacheId) {
		return unsafeExceptionPromise(promise(FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public long tryFlush(long cacheId) throws ConcurrencyControlException {
		return unsafeRollbackAndConcurrencyControlExceptionManaged(tryFlushPromise(cacheId));
	}

	public CompletableFuture<Long> tryFlushPromise(long cacheId) {
		return unsafeExceptionPromise(promise(TRY_FLUSH, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public long clear(long cacheId) {
		return unsafeRollbackManaged(clearPromise(cacheId));
	}

	public CompletableFuture<Long> clearPromise(long cacheId) {
		return unsafeExceptionPromise(promise(CLEAR, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public long mount(long cacheId) {
		return unsafeRollbackManaged(mountPromise(cacheId));
	}

	public CompletableFuture<Long> mountPromise(long cacheId) {
		return unsafeExceptionPromise(promise(MOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public long unmount(long cacheId) {
		return unsafeRollbackManaged(unmountPromise(cacheId));
	}

	public CompletableFuture<Long> unmountPromise(long cacheId) {
		return unsafeExceptionPromise(promise(UNMOUNT, buff -> buff.getLongThrowException(), buffer -> buffer.appendLong(cacheId)));
	}

	@Override
	public int getCacheLevel(long cacheId) {
		return unsafeRollbackManaged(getCacheLevelPromise(cacheId));
	}

	public CompletableFuture<Integer> getCacheLevelPromise(long cacheId) {
		return unsafeExceptionPromise(promise(GET_CACHE_LEVEL, buff -> buff.getIntThrowException(), buffer -> buffer.appendLong(cacheId)));
	}
}
