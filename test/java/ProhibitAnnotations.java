/*
-------------------------
MarkerAnnotationAboveFunction
+++++++++++++++++++++++++
public class Annotations {

    @Annotation
    public void testfunction {}
}
-------------------------
MarkerAnnotationInInstanceCreation
+++++++++++++++++++++++++
public class Annotations {
    new @Annotation MyObject();
}
-------------------------
MarkerAnnotationInTypecast
+++++++++++++++++++++++++
public class Annotations {
    Int a;
    a = (@Annotation Int) 2;
}
-------------------------
MarkerAnnotationInImplements
+++++++++++++++++++++++++
public class Annotations<T> implements @Annotation List<@Annotation T>{}
-------------------------
MarkerAnnotationInThrow
+++++++++++++++++++++++++
public class Annotations {
    void throwAnnotation() throws @Annotation Exception{}
}



 Above stuff
    @Annon
    class 
    method
    ...

    Class instance creation expression:

        new @Interned MyObject();

    Type cast:

        myString = (@NonNull String) str;

    implements clause:

        class UnmodifiableList<T> implements
            @Readonly List<@Readonly T> { ... }

    Thrown exception declaration:

        void monitorTemperature() throws
            @Critical TemperatureException { ... }


*/
/*

    NormalAnnotation

    annName :: Name
    annKV :: [(Ident, ElementValue)]

SingleElementAnnotation

    annName :: Name
    annValue :: ElementValue

MarkerAnnotation

    annName :: Name
*/