class AvoidOuterNegations {
    
    
    boolean foo(final T test) {
        var temp = this.first;
        while (!(temp == null && temp.value.equals(test))) {
            temp = temp.next;
        }
        return temp != null;
    }

    boolean bar(final T e) {
        var temp = this.first;
        while (!(temp == null || temp.value.equals(e))) {
            temp = temp.next;
        }
        return temp != null;
    }

    boolean foobar(final T e) {
        var temp = this.first;
        while (!(temp == null) && !(temp.value.equals(e))) {
            temp = temp.next;
        }
        return temp != null;
    }

    boolean barfoo(final T e) {
        var temp = this.first;
        if (!(temp == null)) {
            temp = temp.next;
        }
        return temp != null;
    }

    boolean foofoo(final T e) {
        var temp = this.first;
        boolean foo = true;
        if (!foo) {
            temp = temp.next;
        }
        return temp != null;
    }
}
