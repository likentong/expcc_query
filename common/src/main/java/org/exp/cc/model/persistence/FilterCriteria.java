package org.exp.cc.model.persistence;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

/**
 * Filter criteria.
 */
public class FilterCriteria {
    private final List<QueryOperator> filter;

    @JsonCreator
    public FilterCriteria(@JsonProperty("filter") final List<QueryOperator> filter) {
        this.filter = filter;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final FilterCriteria that = (FilterCriteria) o;
        return Objects.equals(filter, that.filter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(filter);
    }

}
