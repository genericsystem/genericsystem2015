package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.cacheonclient.CocProtocol;

/**
 * @author Nicolas Feybesse
 *
 */
public class BasicEngine extends AbstractServer implements Generic, CocProtocol {

	@Override
	public BasicEngine getRoot() {
		return this;
	}

	public BasicEngine() {
		this(new Class[] {});
	}

	public BasicEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public BasicEngine(String value, Class<?>... userClasses) {
		this(value, null, userClasses);
	}

	public BasicEngine(String value, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), value, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
		startSystemCache(userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		isInitialized = true;
	}

	@Override
	public AbstractCache newCache() {
		return new AbstractCache(this) {

			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction(BasicEngine.this);
			}

			@Override
			protected Generic plug(Generic generic) {
				return ((Transaction) getTransaction()).plug(generic);
			}

			@Override
			protected void unplug(Generic generic) {
				((Transaction) getTransaction()).unplug(generic);
			}
		};
	}

	// @Override
	// public HeavyCache newCache() {
	// return new HeavyCache(this) {
	//
	// @Override
	// protected IDifferential<Generic> buildTransaction() {
	// return new Transaction((BasicEngine) getRoot());
	// }
	//
	// @Override
	// protected Generic plug(Generic generic) {
	// return ((Transaction) getTransaction()).plug(generic);
	// }
	//
	// @Override
	// protected void unplug(Generic generic) {
	// ((Transaction) getTransaction()).unplug(generic);
	// }
	//
	// };
	// }

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().stream(ts).map(generic -> generic.getVertex()).toArray(Vertex[]::new) : Statics.EMPTY;
	}

	@Override
	public void apply(long ts, long[] removeIds, Vertex[] addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert Arrays.stream(addVertices).allMatch(addVertex -> getRoot().getGenericById(addVertex.getTs()) == null);
		assert Arrays.stream(addVertices).allMatch(addVertex -> addVertex.getBirthTs() == Long.MAX_VALUE);
		Snapshot<Generic> removes = new Container(Arrays.stream(removeIds).mapToObj(removeId -> getRoot().getGenericById(removeId)));
		Snapshot<Generic> adds = new Container(Arrays.stream(addVertices).map(addVertex -> getRoot().build(addVertex)));
		try {
			new Transaction(this, ts).apply(removes, adds);
		} catch (ConcurrencyControlException | OptimisticLockConstraintViolationException e) {
			adds.forEach(add -> getRoot().release(add.getTs()));
			throw e;
		}
	}
}
