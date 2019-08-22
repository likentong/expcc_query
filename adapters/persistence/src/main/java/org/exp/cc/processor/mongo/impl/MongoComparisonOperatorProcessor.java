package org.exp.cc.processor.mongo.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.mongo.ComparisonOperatorProcessor;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * Mongo comparison operator processor.
 */
@Component
public class MongoComparisonOperatorProcessor implements ComparisonOperatorProcessor {
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
        final List<String> validComparisonOperator = Arrays.stream(ComparisonOperator.values())
                .map(ComparisonOperator::getOperator)
                .collect(Collectors.toList());

        return queryFields.getFields()
                .entrySet()
                .stream()
                .map(fields -> {
                    final String fieldName = fields.getKey();
                    final Criteria andCriteria = new Criteria();

                    final Criteria[] criteria = fields.getValue()
                            .getOperator()
                            .entrySet()
                            .stream()
                            .map(operation -> {
                                final String operator = operation.getKey();

                                if (!validComparisonOperator.contains(operator)) {
                                    throw new InvalidQueryException(String.format("%s comparison operator not supported.", operator));
                                }

                                if (this.comparisonOperatorProcessor.get(operator) == null) {
                                    throw new ApplicationRuntimeException(String.format("Comparison operator processor not found for %s", operator));
                                }

                                return this.comparisonOperatorProcessor.get(operator).apply(fieldName, operation.getValue());
                            })
                            .toArray(Criteria[]::new);

                    return criteria.length == 1 ? criteria[0] : andCriteria.andOperator(criteria);
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
        return Criteria.where(fieldName).in(fieldValue);
    }
}
