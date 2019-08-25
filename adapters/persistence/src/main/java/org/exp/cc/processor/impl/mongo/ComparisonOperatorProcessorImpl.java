package org.exp.cc.processor.impl.mongo;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Mongo comparison operator processor.
 */
@Component
public class ComparisonOperatorProcessorImpl implements ComparisonOperatorProcessor {
    private final Map<String, BiFunction<String, Object, Criteria>> comparisonOperatorProcessor =
            ImmutableMap.<String, BiFunction<String, Object, Criteria>>builder()
                    .put(ComparisonOperator.EQ.getOperator(), this::eqComparisonProcessor)
                    .put(ComparisonOperator.GT.getOperator(), this::gtComparisonProcessor)
                    .put(ComparisonOperator.GTE.getOperator(), this::gteComparisonProcessor)
                    .put(ComparisonOperator.LT.getOperator(), this::ltComparisonProcessor)
                    .put(ComparisonOperator.LTE.getOperator(), this::lteComparisonProcessor)
                    .put(ComparisonOperator.IN.getOperator(), this::inComparisonProcessor)
                    .build();

    @Override
    public Criteria[] generateCriteria(final QueryFields queryFields) {
        checkArgument(queryFields != null, "queryFields cannot be null.");
        checkArgument(queryFields.getFields() != null, "fields cannot be null.");

        final List<String> validComparisonOperator = Arrays.stream(ComparisonOperator.values())
                .map(ComparisonOperator::getOperator)
                .collect(Collectors.toList());

        return queryFields.getFields()
                .entrySet()
                .stream()
                .map(fields -> {
                    final String fieldName = fields.getKey();

                    final Criteria[] criteria = fields.getValue()
                            .getOperator()
                            .entrySet()
                            .stream()
                            .map(operation -> {
                                final String operator = operation.getKey();

                                if (StringUtils.isBlank(operator)) {
                                    throw new InvalidQueryException(String.format("Operator cannot be blank : [%s]", fieldName));
                                }

                                if (!validComparisonOperator.contains(operator)) {
                                    throw new InvalidQueryException(String.format("%s comparison operator not supported.", operator));
                                }

                                if (this.comparisonOperatorProcessor.get(operator) == null) {
                                    throw new ApplicationRuntimeException(String.format("Comparison operator processor not found for %s", operator));
                                }

                                return this.comparisonOperatorProcessor.get(operator).apply(fieldName, operation.getValue());
                            })
                            .toArray(Criteria[]::new);

                    if (criteria.length == 0) {
                        throw new InvalidQueryException(String.format("%s does not has a query operator.", fieldName));
                    }

                    return (criteria.length == 1) ? criteria[0] : new Criteria().andOperator(criteria);
                })
                .toArray(Criteria[]::new);
    }

    private Criteria eqComparisonProcessor(final String fieldName, final Object fieldValue) {
        return Criteria.where(fieldName).is(fieldValue);
    }

    private Criteria gtComparisonProcessor(final String fieldName, final Object fieldValue) {
        return Criteria.where(fieldName).gt(fieldValue);
    }

    private Criteria gteComparisonProcessor(final String fieldName, final Object fieldValue) {
        return Criteria.where(fieldName).gte(fieldValue);
    }

    private Criteria ltComparisonProcessor(final String fieldName, final Object fieldValue) {
        return Criteria.where(fieldName).lt(fieldValue);
    }

    private Criteria lteComparisonProcessor(final String fieldName, final Object fieldValue) {
        return Criteria.where(fieldName).lte(fieldValue);
    }

    private Criteria inComparisonProcessor(final String fieldName, final Object fieldValue) {
        if (fieldValue instanceof Set) {
            return Criteria.where(fieldName).in((Set) fieldValue);
        } else if (fieldValue instanceof List) {
            return Criteria.where(fieldName).in((List) fieldValue);
        } else {
            return Criteria.where(fieldName).in(fieldValue);
        }
    }
}
