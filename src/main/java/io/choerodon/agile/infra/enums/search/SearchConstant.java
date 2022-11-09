package io.choerodon.agile.infra.enums.search;

import java.util.Arrays;
import java.util.List;

import io.choerodon.agile.infra.enums.FieldCode;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class SearchConstant {

    /**
     * 不需要加密的字段
     */
    public static final List<String> NO_ENCRYPT_FIELDS = Arrays.asList(FieldCode.SUB_PROJECT);


    public enum Relationship {
        AND, OR;


        public static boolean contains(String value) {
            for (Relationship relationship : Relationship.values()) {
                if (relationship.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }


    public enum Operation {
        EQUAL,
        LIKE,
        IN,
        NOT_IN,
        BETWEEN,
        IS_NULL,
        IS_NOT_NULL,
        /**
         * 括号
         */
        BRACKET;

        /**
         * 选择器支持的操作
         */
        public static final List<String> SELECTOR_OPERATIONS =
                Arrays.asList(
                        IN.toString(),
                        NOT_IN.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString());

        public static final List<String> DATE_OR_NUMBER_OPERATIONS =
                Arrays.asList(
                        BETWEEN.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString(),
                        EQUAL.toString());

        public static final List<String> STRING_OPERATIONS =
                Arrays.asList(
                        LIKE.toString(),
                        EQUAL.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString());


        public static boolean isEqual(String value) {
            return EQUAL.toString().equals(value);
        }

        public static boolean isLike(String value) {
            return LIKE.toString().equals(value);
        }

        public static boolean isIn(String value) {
            return IN.toString().equals(value);
        }

        public static boolean isBetween(String value) {
            return BETWEEN.toString().equals(value);
        }

        public static boolean isNull(String value) {
            return IS_NULL.toString().equals(value);
        }

        public static boolean isNotNull(String value) {
            return IS_NOT_NULL.toString().equals(value);
        }

        public static boolean isBracket(String value) {
            return BRACKET.toString().equals(value);
        }

        public static boolean contains(String value) {
            for (Operation operation : Operation.values()) {
                if (operation.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }

    public enum ValueSpecial {
        CURRENT_TIMESTAMP;

        public static boolean contains(String value) {
            for (ValueSpecial valueSpecial : ValueSpecial.values()) {
                if (valueSpecial.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }

}
