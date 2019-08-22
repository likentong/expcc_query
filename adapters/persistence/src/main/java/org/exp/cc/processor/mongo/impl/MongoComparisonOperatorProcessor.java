package org.exp.cc.processor.mongo.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.mongo.ComparisonOperatorProcessor;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * Mongo comparison operator processor.
 */
@Component
public class MongoComparisonOperatorProcessor implements ComparisonOperatorProcessor {
    private final Map<String, BiFunction<String, Object, Criteria>> comparisonOperatorProcessor =
            ImmutableMap.of(
                    ComparisonOperator.EQ.getOperator(), this::eqComparisonProcessor,
                    ComparisonOperator.GT.getOperator(), this::gtComparisonProcessor,
                    ComparisonOperator.GTE.getOperator(), this::gteComparisonProcessor,
                    ComparisonOperator.LT.getOperator(), this::ltComparisonProcessor,
                    ComparisonOperator.LTE.getOperator(), this::lteComparisonProcessor
            );

    @Override
    public Criteria[] generateCriteria(final QueryFields queryFields) {
        //pending checking
        final List<String> validComparisonOperator = Arrays.stream(ComparisonOperator.values())
                .map(ComparisonOperator::getOperator)
                .collect(Collectors.toList());

        return queryFields.getFields()
                .entrySet()
                .stream()
                .map(fields -> {
                    final List<Criteria> comparisonCriteria = new ArrayList<>();
                    final String fieldName = fields.getKey();

                    //every field only has one operator
                    final Optional<Map.Entry<String, Object>> queryOperator = fields.getValue()
                            .getOperator()
                            .entrySet()
                            .stream()
                            .findFirst();

                    if (queryOperator.isPresent()) {
                        final String operator = queryOperator.get().getKey();

                        if (!validComparisonOperator.contains(operator)) {
                            throw new InvalidQueryException(String.format("%s comparison operator not supported.", operator));
                        }

                        if (this.comparisonOperatorProcessor.get(operator) == null) {
                            throw new ApplicationRuntimeException(String.format("Comparison operator processor not found for %s", operator));
                        }

                        comparisonCriteria.add(this.comparisonOperatorProcessor.get(operator).apply(fieldName, queryOperator.get().getValue()));
                    } else {
                        throw new InvalidQueryException(String.format("Field [%s] does not has a query operator.", fieldName));
                    }

                    return comparisonCriteria;
                })
                .flatMap(Collection::stream)
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
}
