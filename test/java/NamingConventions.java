package De.fuas.algorithms;

class innerClass {}
interface inner_Interface {}
class _innerClass {}
interface _innerInterface {}
class inner_Class {}
interface inner_Interface {}

class NamingConventions {

    //class innerClass {} -> *
    enum _innerEnum {}
    //interface inner_Interface {} -> *
    //class _innerClass {} -> *
    enum _innerEnum {}
    //interface _innerInterface {} -> *
    //class inner_Class {} -> *
    enum inner_Enum {}
    //interface inner_Interface {} -> *

    // * - rule doesnt find nested class/interface decls 

    static int Static;
    static int sta_tic;
    static int _static;

    int Member;
    int mem_ber;
    int _member;
    
    void variables() {

        final int FinalVar;
        final int final_var;
        final int _finalvar;

        int Var;
        int var_2;
        int _var; 
    } 

    void Method() {}
    void met_hod() {}
    void _Method() {}

    void params(int Param, int pa_ram, int _param) {}

}
