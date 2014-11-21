import ceylon.collection {
	ArrayList
}
class CartesianIterator({Vertex?*}[] iterables) satisfies Iterator<Vertex?[]>{
	shared actual Vertex[]|Finished next() => finished;		
	
	Integer iterablesSize=iterables.size;
	ArrayList<Vertex?> values=ArrayList<Vertex?>(iterablesSize);
	ArrayList<Iterator<Vertex?>> iterators=ArrayList<Iterator<Vertex?>>(iterablesSize);
	variable Boolean empty=false;
	
	for(i->iterable in iterables.indexed){
		iterators.set(i, iterable.iterator());
		if (!iterable.hasNext()) {
			empty = true;
			break;
		}
	}
	
	void setNextValue(Integer index) {
		Iterator<Vertex?>? it = iterators[index];
		if(exists it){
			<Vertex?|Finished>() nextValue = it.next;
			if(!nextValue is Finished){
				if (exists nextValue){
					values.set(index,nextValue);
				}
			}
		}
	}
	
	if (!empty) {
		for (Integer i = 0; i < iterablesSize - 1; i++)
		setNextValue(i);
	}
	
	
	@Override
	public boolean hasNext() {
		if (empty)
		return false;
		for (int i = 0; i < iterablesSize; i++)
		if (iterators[i].hasNext())
		return true;
		return false;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public T[] next() {
		int cursor;
		for (cursor = iterablesSize - 1; cursor >= 0; cursor--)
		if (iterators[cursor].hasNext())
		break;
		
		for (int i = cursor + 1; i < iterablesSize; i++)
		iterators[i] = iterables[i].iterator();
		
		for (int i = cursor; i < iterablesSize; i++)
		setNextValue(i);
		
		return (T[]) values.clone();
	}
	
	private void setNextValue(int index) {
		Iterator<Object> it = iterators[index];
		if (it.hasNext())
		values[index] = it.next();
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
}

