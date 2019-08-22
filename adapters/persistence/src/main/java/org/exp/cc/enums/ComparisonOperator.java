package org.exp.cc.enums;

/**
 * Comparison operator enum.
 */
public enum ComparisonOperator {
    EQ("$eq"), GT("$gt"), GTE("$gte"), LT("$lt"), LTE("$lte"), IN("$in");

    private final String operator;

    ComparisonOperator(final String operator) {
        this.operator = operator;
    }

    public String getOperator() {
        return operator;
    }
}
