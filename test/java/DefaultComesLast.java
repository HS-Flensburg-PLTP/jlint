class Test {
    public void test() {
        switch(i){
            case 1 : 
                a = "a";
                break;
            default:
                break;
            case 2 : 
                a = "b";
                break;
        }
    }
    
    public void foo() {
    switch(i){
        default:
            break;
        case 2 :
            a = "b";
            break;
        case 3 :
            a = "c";
        }
    }

    public void bar() {
        switch (i) {
            case 1:
                break;
            case 2:
                break;
        }
    }

    public void cheese() {
        switch (i) {
            case 1:
                break;
            case 2:
                break;
            default:
                break;
        }
    }

    public void mouse() {
        switch (i) {
            case 1:
                break;
            case 2:
                break;
            default:
                break;
            default:
                break;
        }
    }
    
}

