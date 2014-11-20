class Projection(Vertex origin,components) satisfies Signature {
	shared actual Vertex? meta=>origin.meta;
	shared actual Vertex[] supers=>[origin];
	shared actual Value? content=>origin.content;
	shared actual Vertex?[] components;
	shared actual String string => content?.string else "<null>";
	
	shared actual String info => "*"+super.info;
}